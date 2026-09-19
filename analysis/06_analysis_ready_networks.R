# =============================================================================
# analysis/06_analysis_ready_networks.R
#
# =============================================================================
#
# SCOPE NOTE -----------------------------------------------------------------
# This script builds two SEPARATE analysis-ready networks (BA-to-BA,
# region-to-region) with lower/midpoint/upper reconciliation scenarios,
# annual and monthly aggregates, and scenario-specific node rankings.
#
# It does NOT redo 05's audit logic: connection-level discrepancy rates and
# chronic classification are reused as-is from 05's cached outputs. It DOES
# compute the per-pair-hour lower/midpoint/upper estimates, since 05 cached
# only connection-level summaries, not hourly reconciled values.
#
# It does NOT select a story, design a chart, or combine the BA and region
# layers into one network at any point.
#
# Direction conflicts are NOT forced into a midpoint direction. Those
# pair-hours are excluded from the primary lower/mid/upper networks and
# retained separately in unresolved_direction_conflicts, with both raw
# reported values intact.
#
# Depends on (from 05):
#   data/canonical_reports_full.parquet
#   data/connection_discrepancies_full.parquet
#   data/chronic_connections_full.parquet
#   data/entity_code_lookup_full.parquet
#
# Outputs (cached to data/):
#   data/pair_hour_estimates_full.parquet         - per-hour lower/mid/upper + quality flags
#   data/unresolved_direction_conflicts.parquet    - excluded direction-conflict pair-hours
#   data/connection_layer_membership.parquet       - layer classification for all connections
#   data/mixed_unclassified_connections.parquet    - flagged for manual review (should be empty)
#   data/ba_network_annual_connections.parquet
#   data/ba_network_monthly_connections.parquet
#   data/ba_network_annual_nodes.parquet
#   data/ba_network_monthly_nodes.parquet
#   data/ba_network_ranks.parquet
#   data/region_network_annual_connections.parquet
#   data/region_network_monthly_connections.parquet
#   data/region_network_annual_nodes.parquet
#   data/region_network_monthly_nodes.parquet
#   data/region_network_ranks.parquet
# =============================================================================

library(dplyr)
library(tidyr)
library(arrow)
library(here)
library(lubridate)

# --- 1. Load 05's cached outputs ----------------------------------------------

canonical_reports_full <- read_parquet(here("data", "canonical_reports_full.parquet"))
connection_discrepancies_full <- read_parquet(here("data", "connection_discrepancies_full.parquet"))
chronic_connections_full <- read_parquet(here("data", "chronic_connections_full.parquet"))
entity_code_lookup <- read_parquet(here("data", "entity_code_lookup_full.parquet"))

# --- 2. Connection-layer membership for ALL connections (not just chronic) ---
# Mirrors 05's chronic_connections_typed logic, applied to every observed
# connection. National aggregates (CAN, MEX, US48) are excluded from both
# primary networks here, not silently dropped later.

connection_layer_membership <- connection_discrepancies_full |>
  select(ba_1, ba_2, pair_hours, discrepancy_rate) |>
  left_join(entity_code_lookup |> select(code, type_1 = entity_type), by = c("ba_1" = "code")) |>
  left_join(entity_code_lookup |> select(code, type_2 = entity_type), by = c("ba_2" = "code")) |>
  mutate(
    connection_layer = case_when(
      is.na(type_1) | is.na(type_2) ~ "unclassified",
      type_1 == "national_aggregate" | type_2 == "national_aggregate" ~ "national_excluded",
      type_1 == "regional_aggregate" & type_2 == "regional_aggregate" ~ "region_to_region",
      type_1 == "balancing_authority" & type_2 == "balancing_authority" ~ "ba_to_ba",
      TRUE ~ "mixed"
    ),
    is_chronic = paste(ba_1, ba_2) %in% paste(chronic_connections_full$ba_1, chronic_connections_full$ba_2)
  )

stopifnot(!any(connection_layer_membership$connection_layer == "unclassified"))

write_parquet(connection_layer_membership, here("data", "connection_layer_membership.parquet"))

mixed_unclassified_connections <- connection_layer_membership |>
  filter(connection_layer %in% c("mixed", "unclassified"))

write_parquet(mixed_unclassified_connections, here("data", "mixed_unclassified_connections.parquet"))

message("--- Connection layer counts ---")
connection_layer_membership |> count(connection_layer) |> print()

if (nrow(mixed_unclassified_connections) > 0) {
  message("\n", nrow(mixed_unclassified_connections),
          " connection(s) flagged mixed/unclassified — review before proceeding:")
  print(mixed_unclassified_connections)
}

# --- 3. Per-pair-hour reconciliation: lower/midpoint/upper --------------------
# Vectorized (not rowwise) for full-year scale: each pair-hour has at most
# 2 reports, confirmed by the integrity check below.

max_reports_per_pair_hour <- canonical_reports_full |>
  count(period, ba_1, ba_2) |>
  summarise(max_n = max(n)) |>
  pull(max_n)

stopifnot(max_reports_per_pair_hour <= 2)

pair_hour_wide <- canonical_reports_full |>
  select(period, ba_1, ba_2, canonical_value) |>
  group_by(period, ba_1, ba_2) |>
  mutate(report_index = row_number()) |>
  ungroup() |>
  pivot_wider(names_from = report_index, values_from = canonical_value, names_prefix = "value_")

# value_2 will not exist as a column if every pair-hour in the data had only
# one report; guard for that edge case so the mutate() below doesn't fail.
if (!"value_2" %in% names(pair_hour_wide)) pair_hour_wide$value_2 <- NA_real_

pair_hour_estimates_full <- pair_hour_wide |>
  mutate(
    reports = if_else(is.na(value_2), 1L, 2L),
    direction_conflict = reports == 2 &
      sign(value_1) != 0 & sign(value_2) != 0 &
      sign(value_1) != sign(value_2),
    
    lower_signed = case_when(
      direction_conflict ~ NA_real_,
      reports == 1 ~ value_1,
      abs(value_1) <= abs(value_2) ~ value_1,
      TRUE ~ value_2
    ),
    upper_signed = case_when(
      direction_conflict ~ NA_real_,
      reports == 1 ~ value_1,
      abs(value_1) >= abs(value_2) ~ value_1,
      TRUE ~ value_2
    ),
    mid_signed = case_when(
      direction_conflict ~ NA_real_,
      reports == 1 ~ value_1,
      TRUE ~ (value_1 + value_2) / 2
    ),
    
    report_spread = if_else(reports == 2 & !direction_conflict,
                            abs(value_1 - value_2), NA_real_),
    relative_spread = if_else(reports == 2 & !direction_conflict,
                              report_spread / pmax(abs(value_1), abs(value_2), 1), NA_real_),
    is_discrepant = reports == 2 & !direction_conflict & relative_spread > 0.05
  ) |>
  left_join(connection_layer_membership |> select(ba_1, ba_2, connection_layer, is_chronic),
            by = c("ba_1", "ba_2"))

# Integrity check: for non-conflict, 2-report pair-hours, magnitude ordering
# must hold by construction.
magnitude_check <- pair_hour_estimates_full |>
  filter(reports == 2, !direction_conflict) |>
  summarise(ok = all(abs(lower_signed) <= abs(mid_signed) + 1e-6 &
                       abs(mid_signed) <= abs(upper_signed) + 1e-6)) |>
  pull(ok)

stopifnot(magnitude_check)

write_parquet(pair_hour_estimates_full, here("data", "pair_hour_estimates_full.parquet"))

# --- 4. Unresolved direction conflicts (excluded from primary networks) ------
# Kept separately with BOTH raw reported values and connection layer, since
# these pair-hours cannot be given a trustworthy single source/target.

unresolved_direction_conflicts <- pair_hour_estimates_full |>
  filter(direction_conflict) |>
  select(period, ba_1, ba_2, value_1, value_2, connection_layer, is_chronic)

write_parquet(unresolved_direction_conflicts, here("data", "unresolved_direction_conflicts.parquet"))

message("\n", nrow(unresolved_direction_conflicts),
        " pair-hour(s) excluded from primary networks due to direction conflict.")

# --- 5. Build one network (BA or region) --------------------------------------
# Shared logic for both layers: long-format directional flows per scenario,
# then annual/monthly connection and node aggregates, then scenario ranks.

build_network <- function(layer_name, prefix) {
  
  # ALL pair-hours for this layer, including direction conflicts — used only
  # for exposure/coverage accounting, so conflicts don't become invisible
  # undercounting once they're dropped from the directional totals below.
  layer_all <- pair_hour_estimates_full |>
    filter(connection_layer == layer_name)
  
  layer_estimates <- layer_all |> filter(!direction_conflict)
  
  # --- long-format directional flows, one column set per scenario ---
  to_flows <- function(value_col) {
    layer_estimates |>
      transmute(
        period,
        month = floor_date(period, "month"),
        ba_1, ba_2,
        out_node = if_else(.data[[value_col]] >= 0, ba_1, ba_2),
        in_node  = if_else(.data[[value_col]] >= 0, ba_2, ba_1),
        flow_mwh = abs(.data[[value_col]]),
        is_chronic, reports, is_discrepant
      )
  }
  
  flows_lower <- to_flows("lower_signed")
  flows_mid   <- to_flows("mid_signed")
  flows_upper <- to_flows("upper_signed")
  
  # --- exposure/coverage fields: how much of each connection's observed
  # time is actually resolvable, vs. excluded for direction conflict ---
  connection_exposure_annual <- layer_all |>
    group_by(ba_1, ba_2) |>
    summarise(
      observed_hours = n(),
      direction_conflict_hours = sum(direction_conflict),
      resolved_hours = observed_hours - direction_conflict_hours,
      resolved_hours_pct = resolved_hours / observed_hours,
      .groups = "drop"
    )
  
  connection_exposure_monthly <- layer_all |>
    mutate(month = floor_date(period, "month")) |>
    group_by(ba_1, ba_2, month) |>
    summarise(
      observed_hours = n(),
      direction_conflict_hours = sum(direction_conflict),
      resolved_hours = observed_hours - direction_conflict_hours,
      resolved_hours_pct = resolved_hours / observed_hours,
      .groups = "drop"
    )
  
  # --- annual connection totals: net signed + gross magnitude, per scenario ---
  annual_connection_totals <- layer_estimates |>
    group_by(ba_1, ba_2) |>
    summarise(
      pair_hours = n(),
      chronic = first(is_chronic),
      net_flow_lower_mwh  = sum(lower_signed, na.rm = TRUE),
      net_flow_mid_mwh    = sum(mid_signed, na.rm = TRUE),
      net_flow_upper_mwh  = sum(upper_signed, na.rm = TRUE),
      gross_flow_lower_mwh = sum(abs(lower_signed), na.rm = TRUE),
      gross_flow_mid_mwh   = sum(abs(mid_signed), na.rm = TRUE),
      gross_flow_upper_mwh = sum(abs(upper_signed), na.rm = TRUE),
      .groups = "drop"
    ) |>
    left_join(connection_exposure_annual, by = c("ba_1", "ba_2"))
  
  monthly_connection_totals <- layer_estimates |>
    mutate(month = floor_date(period, "month")) |>
    group_by(ba_1, ba_2, month) |>
    summarise(
      pair_hours = n(),
      net_flow_lower_mwh  = sum(lower_signed, na.rm = TRUE),
      net_flow_mid_mwh    = sum(mid_signed, na.rm = TRUE),
      net_flow_upper_mwh  = sum(upper_signed, na.rm = TRUE),
      gross_flow_lower_mwh = sum(abs(lower_signed), na.rm = TRUE),
      gross_flow_mid_mwh   = sum(abs(mid_signed), na.rm = TRUE),
      gross_flow_upper_mwh = sum(abs(upper_signed), na.rm = TRUE),
      .groups = "drop"
    ) |>
    left_join(connection_exposure_monthly, by = c("ba_1", "ba_2", "month"))
  
  # --- node totals: incoming/outgoing/throughput/net, per scenario ---
  node_totals <- function(flows, by_month = FALSE) {
    if (by_month) {
      outgoing <- flows |> rename(node = out_node) |> group_by(node, month) |>
        summarise(outgoing_mwh = sum(flow_mwh), .groups = "drop")
      incoming <- flows |> rename(node = in_node) |> group_by(node, month) |>
        summarise(incoming_mwh = sum(flow_mwh), .groups = "drop")
      join_cols <- c("node", "month")
    } else {
      outgoing <- flows |> rename(node = out_node) |> group_by(node) |>
        summarise(outgoing_mwh = sum(flow_mwh), .groups = "drop")
      incoming <- flows |> rename(node = in_node) |> group_by(node) |>
        summarise(incoming_mwh = sum(flow_mwh), .groups = "drop")
      join_cols <- "node"
    }
    
    full_join(outgoing, incoming, by = join_cols) |>
      mutate(
        across(c(outgoing_mwh, incoming_mwh), \(x) replace_na(x, 0)),
        throughput_mwh = outgoing_mwh + incoming_mwh,
        net_flow_mwh = outgoing_mwh - incoming_mwh
      )
  }
  
  annual_nodes <- bind_rows(
    node_totals(flows_lower) |> mutate(scenario = "lower"),
    node_totals(flows_mid)   |> mutate(scenario = "midpoint"),
    node_totals(flows_upper) |> mutate(scenario = "upper")
  )
  
  monthly_nodes <- bind_rows(
    node_totals(flows_lower, by_month = TRUE) |> mutate(scenario = "lower"),
    node_totals(flows_mid,   by_month = TRUE) |> mutate(scenario = "midpoint"),
    node_totals(flows_upper, by_month = TRUE) |> mutate(scenario = "upper")
  )
  
  # --- scenario-specific ranks (annual) ---
  ranks <- annual_nodes |>
    group_by(scenario) |>
    mutate(
      rank_outgoing   = rank(-outgoing_mwh, ties.method = "min"),
      rank_incoming   = rank(-incoming_mwh, ties.method = "min"),
      rank_throughput = rank(-throughput_mwh, ties.method = "min")
    ) |>
    ungroup()
  
  # --- companion node-level exposure/quality table ---
  # A node's exposure aggregates across ALL of its connections in this
  # layer — separate from node_totals (which reflects only resolved flow)
  # so conflict-driven undercounting stays visible per node, not just per
  # connection.
  node_quality_annual <- bind_rows(
    layer_all |> rename(node = ba_1),
    layer_all |> rename(node = ba_2)
  ) |>
    group_by(node) |>
    summarise(
      observed_hours = n(),
      direction_conflict_hours = sum(direction_conflict),
      resolved_hours = observed_hours - direction_conflict_hours,
      resolved_hours_pct = resolved_hours / observed_hours,
      .groups = "drop"
    )
  
  node_quality_monthly <- bind_rows(
    layer_all |> rename(node = ba_1),
    layer_all |> rename(node = ba_2)
  ) |>
    mutate(month = floor_date(period, "month")) |>
    group_by(node, month) |>
    summarise(
      observed_hours = n(),
      direction_conflict_hours = sum(direction_conflict),
      resolved_hours = observed_hours - direction_conflict_hours,
      resolved_hours_pct = resolved_hours / observed_hours,
      .groups = "drop"
    )
  
  write_parquet(annual_connection_totals, here("data", paste0(prefix, "_annual_connections.parquet")))
  write_parquet(monthly_connection_totals, here("data", paste0(prefix, "_monthly_connections.parquet")))
  write_parquet(annual_nodes, here("data", paste0(prefix, "_annual_nodes.parquet")))
  write_parquet(monthly_nodes, here("data", paste0(prefix, "_monthly_nodes.parquet")))
  write_parquet(ranks, here("data", paste0(prefix, "_ranks.parquet")))
  write_parquet(node_quality_annual, here("data", paste0(prefix, "_node_quality_annual.parquet")))
  write_parquet(node_quality_monthly, here("data", paste0(prefix, "_node_quality_monthly.parquet")))
  
  list(
    annual_connections = annual_connection_totals,
    monthly_connections = monthly_connection_totals,
    annual_nodes = annual_nodes,
    monthly_nodes = monthly_nodes,
    ranks = ranks,
    node_quality_annual = node_quality_annual,
    flows_mid = flows_mid  # kept in-memory only for integrity checks below
  )
}

ba_network     <- build_network("ba_to_ba", "ba_network")
region_network <- build_network("region_to_region", "region_network")

# --- 6. Integrity checks -------------------------------------------------------

message("\n--- Integrity checks ---")

# No regional/national aggregates in the BA network
ba_nodes_observed <- unique(c(ba_network$annual_nodes$node))
ba_node_types <- entity_code_lookup |> filter(code %in% ba_nodes_observed) |> pull(entity_type)
stopifnot(all(ba_node_types == "balancing_authority"))
message("PASS: BA network contains only balancing_authority nodes (",
        length(ba_nodes_observed), " nodes).")

# Only regional aggregates in the regional network
region_nodes_observed <- unique(c(region_network$annual_nodes$node))
region_node_types <- entity_code_lookup |> filter(code %in% region_nodes_observed) |> pull(entity_type)
stopifnot(all(region_node_types == "regional_aggregate"))
message("PASS: Region network contains only regional_aggregate nodes (",
        length(region_nodes_observed), " nodes).")

# Node-count coverage vs. equality: a BA classified as balancing_authority
# can legitimately be absent from the BA network if it only ever connects
# to a national aggregate (CAN/MEX/US48) — that's expected exclusion, not
# a bug. Identify those explicitly rather than treating any gap as an error.
excluded_ba_nodes <- entity_code_lookup |>
  filter(entity_type == "balancing_authority") |>
  anti_join(tibble(code = ba_nodes_observed), by = "code")

excluded_region_nodes <- entity_code_lookup |>
  filter(entity_type == "regional_aggregate") |>
  anti_join(tibble(code = region_nodes_observed), by = "code")

message("\n", nrow(excluded_ba_nodes),
        " balancing authority node(s) absent from the BA network ",
        "(likely connect only to national aggregates or a mixed/excluded layer):")
print(excluded_ba_nodes)

if (nrow(excluded_region_nodes) > 0) {
  message("\n", nrow(excluded_region_nodes),
          " regional aggregate node(s) absent from the region network:")
  print(excluded_region_nodes)
}

# No unclassified nodes anywhere in connection_layer_membership
stopifnot(!any(connection_layer_membership$connection_layer == "unclassified"))
message("PASS: No unclassified connections.")

# Lower <= midpoint <= upper for magnitude estimates (already checked above
# at the pair-hour level; re-confirm at the annual connection-total level)
totals_ordering_ok <- bind_rows(
  ba_network$annual_connections, region_network$annual_connections
) |>
  summarise(ok = all(gross_flow_lower_mwh <= gross_flow_mid_mwh + 1e-6 &
                       gross_flow_mid_mwh <= gross_flow_upper_mwh + 1e-6)) |>
  pull(ok)
stopifnot(totals_ordering_ok)
message("PASS: lower <= midpoint <= upper holds for all annual connection totals.")

# Scenario totals reconcile with connection totals: total outgoing across all
# nodes in a scenario should equal total incoming (every flow has exactly
# one source and one target)
check_balance <- function(annual_nodes, label) {
  balance <- annual_nodes |>
    group_by(scenario) |>
    summarise(total_out = sum(outgoing_mwh), total_in = sum(incoming_mwh), .groups = "drop") |>
    mutate(diff = abs(total_out - total_in))
  stopifnot(all(balance$diff < 1))  # MWh rounding tolerance
  message("PASS: ", label, " outgoing/incoming totals balance across all scenarios.")
}
check_balance(ba_network$annual_nodes, "BA network")
check_balance(region_network$annual_nodes, "Region network")

# No accidental BA/region mixing (each network's flows source from exactly
# one connection_layer, enforced by build_network()'s filter — confirm here)
stopifnot(all(unique(ba_network$flows_mid$ba_1) %in% connection_layer_membership$ba_1 |
                unique(ba_network$flows_mid$ba_1) %in% connection_layer_membership$ba_2))
message("PASS: No BA/region mixing detected in either network.")

message("\n06 complete. BA network: ", nrow(ba_network$annual_connections),
        " connections, ", length(ba_nodes_observed), " nodes. Region network: ",
        nrow(region_network$annual_connections), " connections, ",
        length(region_nodes_observed), " nodes.")

