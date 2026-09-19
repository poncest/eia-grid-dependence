# =============================================================================
# analysis/09_story_discovery.R
#
# =============================================================================
#
#
# SCOPE NOTE -----------------------------------------------------------------
# First substantive analysis script. Joins reconciled BA/region flows (06),
# stress classification (07), and node demand (08) to compute two
# dependence measures per node, under lower/midpoint/upper reconciliation
# scenarios, comparing stress hours vs. normal hours within season/hour
# strata (stratified standardization, per 07's design — NOT 1:1 matching).
#
# Measures, computed as RATIO OF SUMS within each stratum (NOT a mean of
# hourly ratios — averaging hourly ratios lets a near-zero demand hour
# dominate a whole stratum, producing Inf/-Inf; summing first avoids that):
#   gross_import_ratio    = sum(incoming_mwh) / sum(demand_mwh)
#   net_import_dependence = sum(incoming_mwh - outgoing_mwh) / sum(demand_mwh)
#
# This does NOT redo reconciliation (06), redefine stress (07), or re-pull
# demand (08) — it reuses their cached outputs. It does NOT select a final
# editorial story or design a chart — it produces a gated, ranked table of
# candidate findings for review.
#
# BA-network and region-network results are kept SEPARATE throughout, per
# 06's "never combine the layers" rule.
#
# Depends on:
#   data/pair_hour_estimates_full.parquet   (06)
#   data/entity_code_lookup_full.parquet    (05)
#   data/demand_stress_tagged.parquet       (07)
#   data/season_hour_strata_summary.parquet (07)
#   data/node_demand_full.parquet           (08)
#   data/node_demand_coverage.parquet       (08)
#
# Outputs:
#   data/demand_denominator_audit.parquet
#   data/node_hourly_flow_ba.parquet / _region.parquet
#   data/dependence_stratified_ba.parquet / _region.parquet  - final gated results
#   data/demand_exclusions.parquet
#   output/dependence_candidates_ranked.csv
# =============================================================================

library(dplyr)
library(tidyr)
library(arrow)
library(here)
library(lubridate)

dir.create(here("data"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("output"), recursive = TRUE, showWarnings = FALSE)

# --- 1. Load cached inputs -----------------------------------------------------

pair_hour_estimates_full <- read_parquet(here("data", "pair_hour_estimates_full.parquet"))
entity_code_lookup <- read_parquet(here("data", "entity_code_lookup_full.parquet"))
demand_stress_tagged <- read_parquet(here("data", "demand_stress_tagged.parquet"))
season_hour_strata_summary <- read_parquet(here("data", "season_hour_strata_summary.parquet"))
node_demand_full <- read_parquet(here("data", "node_demand_full.parquet"))
node_demand_coverage <- read_parquet(here("data", "node_demand_coverage.parquet"))

# --- 2. Usable demand nodes + demand-denominator audit -------------------------

usable_demand_nodes <- node_demand_coverage |>
  filter(coverage_status %in% c("COMPLETE", "NEAR_COMPLETE")) |>
  pull(code)

demand_exclusions <- node_demand_coverage |>
  filter(!coverage_status %in% c("COMPLETE", "NEAR_COMPLETE")) |>
  select(code, name, entity_type, coverage_status, exclusion_reason)

write_parquet(demand_exclusions, here("data", "demand_exclusions.parquet"))

message(length(usable_demand_nodes), " nodes usable for dependence analysis. ",
        nrow(demand_exclusions), " excluded (see demand_exclusions.parquet).")

# Audit the denominator itself, BEFORE building any ratio — a zero or
# negative demand hour breaks gross_import_ratio and net_import_dependence
# regardless of how clean the flow side is.
demand_denominator_audit <- node_demand_full |>
  left_join(demand_stress_tagged |> select(period, is_stress), by = "period") |>
  group_by(node) |>
  summarise(
    hours = n(),
    missing_hours = sum(is.na(value)),
    zero_hours = sum(value == 0, na.rm = TRUE),
    negative_hours = sum(value < 0, na.rm = TRUE),
    nonpositive_stress_hours = sum(value <= 0 & is_stress, na.rm = TRUE),
    minimum_positive_demand = suppressWarnings(min(value[value > 0], na.rm = TRUE)),
    p01_positive_demand = suppressWarnings(quantile(value[value > 0], 0.01, na.rm = TRUE)),
    median_demand = median(value, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(nonpositive_stress_hours), desc(zero_hours))

write_parquet(demand_denominator_audit, here("data", "demand_denominator_audit.parquet"))

message("\n--- Demand denominator audit: nodes with ANY zero/negative/missing demand ---")
demand_denominator_audit |>
  filter(zero_hours > 0 | negative_hours > 0 | missing_hours > 0) |>
  print(n = Inf, width = Inf)

# A valid-demand lookup: only hours with strictly positive demand are usable
# in any ratio's denominator. This is applied at the join step (section 4),
# not by setting individual hourly ratios to NA — since we no longer build
# hourly ratios at all (ratio-of-sums happens at the stratum level).
valid_demand_hours <- node_demand_full |>
  filter(!is.na(value), value > 0) |>
  select(node, period, demand_mwh = value)

# --- 3. Build hourly node-level flow, per layer, with 3-level flow_status ----

build_hourly_node_flow <- function(layer_name) {
  
  layer_all <- pair_hour_estimates_full |> filter(connection_layer == layer_name)
  
  # Per-node, per-hour: resolved vs. conflict-excluded connection counts,
  # so a partially-excluded hour is distinguishable from both a fully-
  # excluded hour and a fully-clean hour.
  connection_activity <- bind_rows(
    layer_all |> transmute(node = ba_1, period, direction_conflict),
    layer_all |> transmute(node = ba_2, period, direction_conflict)
  ) |>
    group_by(node, period) |>
    summarise(
      total_connections_active = n(),
      conflicted_connections = sum(direction_conflict),
      resolved_connections = total_connections_active - conflicted_connections,
      .groups = "drop"
    ) |>
    mutate(
      resolved_connection_pct = resolved_connections / total_connections_active,
      flow_status = case_when(
        resolved_connections == 0 & conflicted_connections > 0 ~ "FULLY_CONFLICT_EXCLUDED",
        resolved_connections > 0 & conflicted_connections > 0 ~ "PARTIALLY_CONFLICT_EXCLUDED",
        TRUE ~ "FULLY_OBSERVED"
      )
    )
  
  layer_resolved <- layer_all |> filter(!direction_conflict)
  
  to_flow_sums <- function(value_col, out_name, in_name) {
    outgoing <- layer_resolved |>
      transmute(
        node = if_else(.data[[value_col]] >= 0, ba_1, ba_2),
        period, val = abs(.data[[value_col]])
      ) |>
      group_by(node, period) |>
      summarise(!!out_name := sum(val), .groups = "drop")
    
    incoming <- layer_resolved |>
      transmute(
        node = if_else(.data[[value_col]] >= 0, ba_2, ba_1),
        period, val = abs(.data[[value_col]])
      ) |>
      group_by(node, period) |>
      summarise(!!in_name := sum(val), .groups = "drop")
    
    full_join(outgoing, incoming, by = c("node", "period"))
  }
  
  flows_lower <- to_flow_sums("lower_signed", "outgoing_lower_mwh", "incoming_lower_mwh")
  flows_mid   <- to_flow_sums("mid_signed", "outgoing_mid_mwh", "incoming_mid_mwh")
  flows_upper <- to_flow_sums("upper_signed", "outgoing_upper_mwh", "incoming_upper_mwh")
  
  connection_activity |>
    left_join(flows_lower, by = c("node", "period")) |>
    left_join(flows_mid, by = c("node", "period")) |>
    left_join(flows_upper, by = c("node", "period")) |>
    mutate(across(matches("_mwh$"),
                  \(x) if_else(flow_status == "FULLY_CONFLICT_EXCLUDED", NA_real_, coalesce(x, 0))))
}

node_hourly_flow_ba <- build_hourly_node_flow("ba_to_ba")
node_hourly_flow_region <- build_hourly_node_flow("region_to_region")

write_parquet(node_hourly_flow_ba, here("data", "node_hourly_flow_ba.parquet"))
write_parquet(node_hourly_flow_region, here("data", "node_hourly_flow_region.parquet"))

message("\n--- flow_status distribution, BA layer ---")
node_hourly_flow_ba |> count(flow_status) |>
  mutate(pct = round(100 * n / sum(n), 2)) |> print()
message("--- flow_status distribution, region layer ---")
node_hourly_flow_region |> count(flow_status) |>
  mutate(pct = round(100 * n / sum(n), 2)) |> print()

# Diagnostic: do partial exclusions concentrate in stress hours or specific
# nodes? A partial total is usable as a sensitivity input but is NOT
# equivalent to a fully observed node-hour — worth knowing where it clusters
# before deciding whether it's safe to ignore for the primary analysis.
partial_exclusion_diagnostic <- function(node_hourly_flow, layer_label) {
  node_hourly_flow |>
    filter(flow_status == "PARTIALLY_CONFLICT_EXCLUDED") |>
    left_join(demand_stress_tagged |> select(period, is_stress), by = "period") |>
    count(node, is_stress) |>
    mutate(layer = layer_label)
}

message("\n--- Partial-exclusion concentration (top 15 by count), BA layer ---")
partial_exclusion_diagnostic(node_hourly_flow_ba, "ba") |>
  arrange(desc(n)) |> slice_head(n = 15) |> print(n = Inf)

# --- 4. Join demand + stress tags (RAW sums only, ratio computed in step 5) --

build_dependence_hourly <- function(node_hourly_flow) {
  node_hourly_flow |>
    filter(node %in% usable_demand_nodes, flow_status == "FULLY_OBSERVED") |>
    inner_join(valid_demand_hours, by = c("node", "period")) |>
    left_join(
      demand_stress_tagged |> select(period, season, hour_of_day_utc, stratum, is_stress),
      by = "period"
    )
}

dependence_hourly_ba <- build_dependence_hourly(node_hourly_flow_ba)
dependence_hourly_region <- build_dependence_hourly(node_hourly_flow_region)

# --- 5. Stratified standardization: RATIO OF SUMS within each stratum -------
#
# Four measures per stratum, all derived from the same underlying sums:
#   gross_import_ratio    = sum(incoming) / sum(demand)
#   raw_net_import_ratio  = (sum(incoming) - sum(outgoing)) / sum(demand)
#                            [SIGNED — positive = net importer that stratum,
#                            negative = net exporter. This is what sign-
#                            crossing / exporter-to-importer detection uses.]
#   net_import_dependence = pmax(raw_net_import_ratio, 0)
#                            [primary "how import-dependent" measure —
#                            export periods contribute 0, not a negative]
#   net_export_intensity  = pmax(-raw_net_import_ratio, 0)
#                            [mirror measure for export-side analysis —
#                            import periods contribute 0]
#
# SPA prompted this split: its raw ratio (-24.9 to -31.5) is a real,
# correctly-computed number, but it describes export intensity relative to
# an unusually small reported demand (median 21 MWh/hr), not import
# dependence. Clamping keeps that phenomenon out of the dependence ranking
# without discarding SPA's real (and different) finding.

CLAIM_GATE_PRIMARY <- 0.95
CLAIM_GATE_WARNING <- 0.80
MIN_POSITIVE_DEMAND_COVERAGE <- 0.95
MIN_RESOLVED_FLOW_COVERAGE <- 0.90

standardize <- function(dependence_hourly, node_hourly_flow, layer_label) {
  
  scenarios <- c("lower", "mid", "upper")
  measures <- c("gross_import_ratio", "raw_net_import_ratio",
                "net_import_dependence", "net_export_intensity")
  results <- list()
  idx <- 1
  
  for (scenario in scenarios) {
    out_col <- paste0("outgoing_", scenario, "_mwh")
    in_col  <- paste0("incoming_", scenario, "_mwh")
    
    stratum_sums <- dependence_hourly |>
      group_by(node, stratum, is_stress) |>
      summarise(
        n_obs = n(),
        sum_in = sum(.data[[in_col]]),
        sum_out = sum(.data[[out_col]]),
        sum_demand = sum(demand_mwh),
        .groups = "drop"
      ) |>
      mutate(
        gross_import_ratio = sum_in / sum_demand,
        raw_net_import_ratio = (sum_in - sum_out) / sum_demand,
        net_import_dependence = pmax(raw_net_import_ratio, 0),
        net_export_intensity = pmax(-raw_net_import_ratio, 0)
      ) |>
      # Drop sum_in/sum_out/sum_demand before pivoting — see the fixed bug
      # note in earlier project history: leaving them in causes pivot_wider
      # to split each stratum into two rows (one stress-only, one
      # normal-only) via its implicit id_cols, silently breaking
      # eligible_stratum downstream.
      select(node, stratum, is_stress, n_obs, all_of(measures)) |>
      pivot_wider(
        id_cols = c(node, stratum),
        names_from = is_stress,
        values_from = c(n_obs, all_of(measures)),
        names_glue = "{.value}_{if_else(is_stress, 'stress', 'normal')}"
      ) |>
      mutate(across(starts_with("n_obs_"), \(x) coalesce(x, 0L))) |>
      left_join(season_hour_strata_summary |> select(stratum, stratum_weight), by = "stratum")
    
    for (measure in measures) {
      stress_col <- paste0(measure, "_stress")
      normal_col <- paste0(measure, "_normal")
      
      m <- stratum_sums |>
        mutate(eligible_stratum = n_obs_stress > 0 & n_obs_normal > 0 &
                 is.finite(.data[[stress_col]]) & is.finite(.data[[normal_col]]))
      
      node_summary <- m |>
        group_by(node) |>
        summarise(
          stress_observations = sum(n_obs_stress),
          normal_observations = sum(n_obs_normal),
          retained_stress_weight = sum(stratum_weight[eligible_stratum], na.rm = TRUE),
          weighted_stress_value = sum(stratum_weight[eligible_stratum] *
                                        .data[[stress_col]][eligible_stratum], na.rm = TRUE) /
            pmax(sum(stratum_weight[eligible_stratum], na.rm = TRUE), 1e-9),
          weighted_normal_value = sum(stratum_weight[eligible_stratum] *
                                        .data[[normal_col]][eligible_stratum], na.rm = TRUE) /
            pmax(sum(stratum_weight[eligible_stratum], na.rm = TRUE), 1e-9),
          .groups = "drop"
        ) |>
        mutate(weighted_difference = weighted_stress_value - weighted_normal_value,
               measure = measure, scenario = scenario, layer = layer_label)
      
      results[[idx]] <- node_summary
      idx <- idx + 1
    }
  }
  
  combined <- bind_rows(results)
  
  flow_coverage <- node_hourly_flow |>
    group_by(node) |>
    summarise(resolved_flow_coverage = mean(flow_status == "FULLY_OBSERVED"), .groups = "drop")
  
  demand_coverage <- demand_denominator_audit |>
    mutate(positive_demand_coverage = 1 - (zero_hours + negative_hours + missing_hours) / hours) |>
    select(node, positive_demand_coverage)
  
  combined |>
    left_join(flow_coverage, by = "node") |>
    left_join(demand_coverage, by = "node") |>
    mutate(
      finite_values = is.finite(weighted_stress_value) & is.finite(weighted_normal_value) &
        is.finite(weighted_difference),
      claim_status = case_when(
        !finite_values ~ "EXCLUDE",
        retained_stress_weight < CLAIM_GATE_WARNING ~ "EXCLUDE",
        coalesce(positive_demand_coverage, 0) < MIN_POSITIVE_DEMAND_COVERAGE ~ "EXCLUDE",
        coalesce(resolved_flow_coverage, 0) < MIN_RESOLVED_FLOW_COVERAGE ~ "EXCLUDE",
        retained_stress_weight >= CLAIM_GATE_PRIMARY ~ "PRIMARY",
        retained_stress_weight >= CLAIM_GATE_WARNING ~ "WARNING",
        TRUE ~ "EXCLUDE"
      )
    ) |>
    left_join(entity_code_lookup |> select(node = code, name, entity_type), by = "node") |>
    select(node, name, entity_type, layer, measure, scenario,
           weighted_stress_value, weighted_normal_value, weighted_difference,
           retained_stress_weight, stress_observations, normal_observations,
           resolved_flow_coverage, positive_demand_coverage, claim_status)
}

dependence_stratified_ba <- standardize(dependence_hourly_ba, node_hourly_flow_ba, "ba")
dependence_stratified_region <- standardize(dependence_hourly_region, node_hourly_flow_region, "region")

stopifnot(
  all(dependence_stratified_ba$retained_stress_weight >= 0 &
        dependence_stratified_ba$retained_stress_weight <= 1 + 1e-9),
  all(dependence_stratified_region$retained_stress_weight >= 0 &
        dependence_stratified_region$retained_stress_weight <= 1 + 1e-9)
)

write_parquet(dependence_stratified_ba, here("data", "dependence_stratified_ba.parquet"))
write_parquet(dependence_stratified_region, here("data", "dependence_stratified_region.parquet"))

message("\n--- Claim gate status counts, BA layer ---")
dependence_stratified_ba |> count(measure, scenario, claim_status) |> print(n = Inf)
message("\n--- Claim gate status counts, region layer ---")
dependence_stratified_region |> count(measure, scenario, claim_status) |> print(n = Inf)

# --- 6. Scenario stability -------------------------------------------------

check_scenario_stability <- function(dependence_stratified) {
  dependence_stratified |>
    group_by(node, measure) |>
    summarise(
      all_primary = all(claim_status == "PRIMARY"),
      sign_consistent = n_distinct(sign(weighted_difference)) == 1,
      scenario_stable = all_primary & sign_consistent,
      .groups = "drop"
    )
}

stability_ba <- check_scenario_stability(dependence_stratified_ba)
stability_region <- check_scenario_stability(dependence_stratified_region)

# --- 7. Demand context + four candidate tables -------------------------------
# Discovery output, not a final finding. Split by phenomenon, not just
# ranked by |weighted_difference| — SPA showed why: a large effect size on
# raw_net_import_ratio can mean "less extreme exporter", not "more import-
# dependent", and those are different stories.

demand_context <- node_demand_full |>
  group_by(node) |>
  summarise(
    annual_demand_mwh = sum(value, na.rm = TRUE),
    median_demand_mwh = median(value, na.rm = TRUE),
    maximum_demand_mwh = max(value, na.rm = TRUE),
    .groups = "drop"
  )

all_stable <- bind_rows(
  dependence_stratified_ba |>
    left_join(stability_ba, by = c("node", "measure")),
  dependence_stratified_region |>
    left_join(stability_region, by = c("node", "measure"))
) |>
  left_join(demand_context, by = "node")

primary_stable_mid <- all_stable |>
  filter(scenario == "mid", claim_status == "PRIMARY", scenario_stable)

# 1. Import dependence increasing under stress (raw ratio: normal->stress,
#    still on the import side or crossing into it)
dependence_increase_candidates <- primary_stable_mid |>
  filter(measure == "raw_net_import_ratio",
         weighted_stress_value > 0, weighted_difference > 0) |>
  arrange(desc(weighted_difference))

# 2. The sharpest subset of (1): net EXPORTERS normally, net IMPORTERS
#    under stress — a genuine sign crossing, not just "less negative"
exporter_to_importer_candidates <- dependence_increase_candidates |>
  filter(weighted_normal_value <= 0, weighted_stress_value > 0)

# 3. Import dependence DEcreasing under stress (mirror of #1 — worth
#    knowing which normally-import-dependent nodes become LESS so, which
#    could itself be a stress-response finding, e.g. shedding load or
#    activating local generation)
dependence_decrease_candidates <- primary_stable_mid |>
  filter(measure == "raw_net_import_ratio",
         weighted_normal_value > 0, weighted_difference < 0) |>
  arrange(weighted_difference)

# 4. Gross import exposure (magnitude of incoming flow relative to demand,
#    regardless of net direction) — a different question from net
#    dependence: a node can have high gross imports while also exporting
#    heavily (wheeling power through), which net measures alone would hide
gross_import_exposure_candidates <- primary_stable_mid |>
  filter(measure == "gross_import_ratio") |>
  arrange(desc(abs(weighted_difference)))

candidate_select_cols <- c("node", "name", "layer", "measure", "scenario",
                           "weighted_stress_value", "weighted_normal_value",
                           "weighted_difference", "retained_stress_weight",
                           "positive_demand_coverage", "resolved_flow_coverage",
                           "annual_demand_mwh", "median_demand_mwh", "maximum_demand_mwh")

write.csv(dependence_increase_candidates |> select(all_of(candidate_select_cols)),
          here("output", "dependence_increase_candidates.csv"), row.names = FALSE)
write.csv(exporter_to_importer_candidates |> select(all_of(candidate_select_cols)),
          here("output", "exporter_to_importer_candidates.csv"), row.names = FALSE)
write.csv(dependence_decrease_candidates |> select(all_of(candidate_select_cols)),
          here("output", "dependence_decrease_candidates.csv"), row.names = FALSE)
write.csv(gross_import_exposure_candidates |> select(all_of(candidate_select_cols)),
          here("output", "gross_import_exposure_candidates.csv"), row.names = FALSE)

# 5. Primary dependence ranking from the PURPOSE-BUILT clamped measure —
#    distinct from (1), which ranks by the signed raw ratio (appropriate
#    for direction-change stories) but is not the intended "how
#    import-dependent is this node" measure. This uses net_import_dependence
#    directly, where export-hour strata already contribute 0 rather than a
#    negative offset.
import_dependence_candidates <- bind_rows(dependence_stratified_ba, dependence_stratified_region) |>
  left_join(demand_context, by = "node") |>
  filter(measure == "net_import_dependence", scenario == "mid",
         claim_status == "PRIMARY") |>
  left_join(
    bind_rows(stability_ba, stability_region) |> filter(measure == "net_import_dependence"),
    by = c("node", "measure")
  ) |>
  filter(scenario_stable) |>
  arrange(desc(weighted_difference))

write.csv(import_dependence_candidates |> select(all_of(candidate_select_cols)),
          here("output", "import_dependence_candidates.csv"), row.names = FALSE)

message("\n--- (5) PRIMARY dependence ranking (net_import_dependence, purpose-built ",
        "clamped measure, PRIMARY + scenario-stable): top 15 ---")
import_dependence_candidates |> slice_head(n = 15) |>
  select(all_of(candidate_select_cols)) |> print(n = Inf, width = Inf)

# --- 8. Transition stability: does exporter-to-importer hold in EVERY ------
# scenario, not just midpoint? A positive weighted_difference at midpoint
# is not enough — the sign crossing itself (normal <= 0, stress > 0) must
# hold under lower AND upper reconciliation estimates too, or the "flip"
# claim isn't robust to how reciprocal disagreements get resolved.

transition_stability <- bind_rows(dependence_stratified_ba, dependence_stratified_region) |>
  filter(measure == "raw_net_import_ratio", claim_status == "PRIMARY") |>
  group_by(node, name, layer) |>
  summarise(
    scenarios = n_distinct(scenario),
    crosses_in_all_scenarios = all(weighted_normal_value <= 0 & weighted_stress_value > 0),
    increase_in_all_scenarios = all(weighted_difference > 0),
    maximum_normal_ratio = max(weighted_normal_value),
    minimum_stress_ratio = min(weighted_stress_value),
    minimum_difference = min(weighted_difference),
    .groups = "drop"
  )

write.csv(transition_stability, here("output", "transition_stability.csv"), row.names = FALSE)

message("\n--- Transition stability: exporter-to-importer candidates, all-scenario check ---")
transition_stability |>
  filter(node %in% exporter_to_importer_candidates$node) |>
  print(n = Inf, width = Inf)

message("\nNote: 'scenarios' below should read 3 for a node present in all three ",
        "reconciliation scenarios at PRIMARY status — fewer than 3 means the node ",
        "dropped to WARNING/EXCLUDE in at least one scenario, which itself is a ",
        "reason for caution even before checking sign-crossing.")

# --- 9. Near-threshold PRIMARY candidates (coverage close to the gate) -----
# A node just above 90%/95% passed the gate, but "passed" and "comfortably
# passed" are different confidence levels. Flag any PRIMARY candidate in
# the four discovery tables whose resolved_flow_coverage or
# positive_demand_coverage sits within 3 points of its threshold — TVA's
# 91.2% resolved_flow_coverage (gate: 90%) is the example that prompted
# this, but the check applies to all candidates, not just TVA.

NEAR_THRESHOLD_MARGIN <- 0.03

near_threshold_candidates <- bind_rows(
  dependence_increase_candidates, exporter_to_importer_candidates,
  dependence_decrease_candidates, gross_import_exposure_candidates,
  import_dependence_candidates
) |>
  distinct(node, measure, scenario, .keep_all = TRUE) |>
  filter(
    (resolved_flow_coverage - MIN_RESOLVED_FLOW_COVERAGE) < NEAR_THRESHOLD_MARGIN |
      (positive_demand_coverage - MIN_POSITIVE_DEMAND_COVERAGE) < NEAR_THRESHOLD_MARGIN
  )

if (nrow(near_threshold_candidates) > 0) {
  message("\n--- Near-threshold PRIMARY candidates (coverage within ",
          NEAR_THRESHOLD_MARGIN * 100, " points of a gate) — extra sensitivity review warranted ---")
  near_threshold_candidates |>
    select(node, name, measure, resolved_flow_coverage, positive_demand_coverage) |>
    print(n = Inf, width = Inf)
} else {
  message("\nNo PRIMARY candidates sit within ", NEAR_THRESHOLD_MARGIN * 100,
          " points of a coverage gate — all clear margin.")
}

message("\n--- Demand context: smallest reported demand nodes (review for denominator issues) ---")
demand_context |> arrange(median_demand_mwh) |> slice_head(n = 10) |> print(n = Inf, width = Inf)

message("\n--- (1) Dependence-increase candidates: top 15 ---")
dependence_increase_candidates |> slice_head(n = 15) |>
  select(all_of(candidate_select_cols)) |> print(n = Inf, width = Inf)

message("\n--- (2) Exporter-to-importer candidates: all ---")
exporter_to_importer_candidates |>
  select(all_of(candidate_select_cols)) |> print(n = Inf, width = Inf)

message("\n--- (3) Dependence-decrease candidates: top 15 ---")
dependence_decrease_candidates |> slice_head(n = 15) |>
  select(all_of(candidate_select_cols)) |> print(n = Inf, width = Inf)

message("\n--- (4) Gross import exposure candidates: top 15 ---")
gross_import_exposure_candidates |> slice_head(n = 15) |>
  select(all_of(candidate_select_cols)) |> print(n = Inf, width = Inf)

