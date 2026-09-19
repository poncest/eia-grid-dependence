# =============================================================================
# 11_corridor_attribution.R
#
# =============================================================================
# 11_corridor_attribution.R
#
# SCOPE NOTE -----------------------------------------------------------------
# For each ROBUST candidate (TPWR, SCL, AECI), decompose the validated
# node-level stress-vs-normal finding into its constituent CORRIDOR (single
# counterparty connection) contributions. This answers a structural
# question — is the finding driven by one dominant neighbor, a handful of
# meaningful ones, or broadly distributed across many — which determines
# chart geometry (focused flow map vs. ego-network/small-multiples vs.
# chord/Sankey summary). It does NOT select a final geometry or design a
# chart — that decision is made after reviewing this script's output.
#
# Method: the node-level raw_net_import_ratio difference is, by
# construction, a weighted sum over strata of (sum_in - sum_out)/sum_demand,
# where sum_in and sum_out are themselves sums over ALL of the node's
# connections. This script performs the identical stratified ratio-of-sums
# computation PER CONNECTION instead of summed across all of them, using
# the SAME hour-set, SAME stratum weights, and SAME demand denominator as
# the node-level analysis — so corridor contributions are directly
# comparable and should approximately sum to the node's validated total.
#
# CAVEAT (stated up front, not discovered after the fact): exact additivity
# assumes every connection has data in every stratum the node has data in
# overall. A connection with zero observations in a stratum where the node
# has data via OTHER connections will have that stratum excluded from ITS
# own eligible set, while still counting toward the node's total — so the
# sum of corridor contributions may not exactly equal the node's cached
# total from 09/10. This script reports both the corridor sum and the
# actual cached total so any gap is visible, not silently absorbed.
#
# Depends on:
#   data/pair_hour_estimates_full.parquet     (06)
#   data/node_hourly_flow_ba.parquet          (09 — for FULLY_OBSERVED filter)
#   data/entity_code_lookup_full.parquet      (05)
#   data/demand_stress_tagged.parquet         (07, top-5% baseline)
#   data/season_hour_strata_summary.parquet   (07, top-5% weights)
#   data/node_demand_full.parquet             (08)
#   data/dependence_stratified_ba.parquet     (09, for the actual cached totals)
#
# Outputs:
#   output/corridor_attribution_TPWR.csv
#   output/corridor_attribution_SCL.csv
#   output/corridor_attribution_AECI.csv
#   output/corridor_attribution_summary.csv   - dominance classification per node
# =============================================================================

library(dplyr)
library(tidyr)
library(arrow)
library(here)

dir.create(here("output"), recursive = TRUE, showWarnings = FALSE)

# --- 1. Load cached inputs -----------------------------------------------

pair_hour_estimates_full <- read_parquet(here("data", "pair_hour_estimates_full.parquet"))
node_hourly_flow_ba <- read_parquet(here("data", "node_hourly_flow_ba.parquet"))
entity_code_lookup <- read_parquet(here("data", "entity_code_lookup_full.parquet"))
demand_stress_tagged <- read_parquet(here("data", "demand_stress_tagged.parquet"))
season_hour_strata_summary <- read_parquet(here("data", "season_hour_strata_summary.parquet"))
node_demand_full <- read_parquet(here("data", "node_demand_full.parquet"))
dependence_stratified_ba <- read_parquet(here("data", "dependence_stratified_ba.parquet"))

valid_demand_hours <- node_demand_full |>
  filter(!is.na(value), value > 0) |> select(node, period, demand_mwh = value)

TARGET_NODES <- c("TPWR", "SCL", "AECI")

# --- 2. Per-connection stratified ratio-of-sums, restricted to hours where -
# the NODE's overall flow_status is FULLY_OBSERVED (same hour-set as the
# node-level analysis, for comparability — not each connection's own
# individually-resolved hours, which could differ and break additivity
# further).

attribute_node <- function(target_node, scenario = "mid") {
  
  fully_observed_hours <- node_hourly_flow_ba |>
    filter(node == target_node, flow_status == "FULLY_OBSERVED") |>
    select(period)
  
  value_col <- paste0(scenario, "_signed")  # lower_signed / mid_signed / upper_signed
  
  node_connections <- pair_hour_estimates_full |>
    filter(connection_layer == "ba_to_ba", ba_1 == target_node | ba_2 == target_node,
           !direction_conflict) |>
    semi_join(fully_observed_hours, by = "period") |>
    transmute(
      period,
      counterparty = if_else(ba_1 == target_node, ba_2, ba_1),
      # Signed FROM THIS NODE'S perspective: positive = import (flow toward
      # target_node), matching the sign convention used everywhere else.
      signed_value = if_else(ba_1 == target_node, -.data[[value_col]], .data[[value_col]])
    )
  
  dep_hourly <- node_connections |>
    inner_join(valid_demand_hours |> filter(node == target_node) |> select(-node),
               by = "period") |>
    inner_join(demand_stress_tagged |> select(period, stratum, is_stress), by = "period")
  
  stratum_sums <- dep_hourly |>
    group_by(counterparty, stratum, is_stress) |>
    summarise(n_obs = n(), sum_signed = sum(signed_value), sum_demand = sum(demand_mwh), .groups = "drop") |>
    mutate(raw_ratio = sum_signed / sum_demand) |>
    select(counterparty, stratum, is_stress, n_obs, raw_ratio) |>
    pivot_wider(id_cols = c(counterparty, stratum), names_from = is_stress,
                values_from = c(n_obs, raw_ratio),
                names_glue = "{.value}_{if_else(is_stress, 'stress', 'normal')}") |>
    mutate(across(starts_with("n_obs_"), \(x) coalesce(x, 0L))) |>
    left_join(season_hour_strata_summary |> select(stratum, stratum_weight), by = "stratum")
  
  corridor_summary <- stratum_sums |>
    mutate(eligible = n_obs_stress > 0 & n_obs_normal > 0 &
             is.finite(raw_ratio_stress) & is.finite(raw_ratio_normal)) |>
    group_by(counterparty) |>
    summarise(
      eligible_strata = sum(eligible),
      retained_weight = sum(stratum_weight[eligible], na.rm = TRUE),
      weighted_stress_value = sum(stratum_weight[eligible] * raw_ratio_stress[eligible], na.rm = TRUE) /
        pmax(sum(stratum_weight[eligible], na.rm = TRUE), 1e-9),
      weighted_normal_value = sum(stratum_weight[eligible] * raw_ratio_normal[eligible], na.rm = TRUE) /
        pmax(sum(stratum_weight[eligible], na.rm = TRUE), 1e-9),
      .groups = "drop"
    ) |>
    mutate(
      contribution = weighted_stress_value - weighted_normal_value,
      node = target_node
    ) |>
    left_join(entity_code_lookup |> select(counterparty = code, counterparty_name = name), by = "counterparty") |>
    arrange(desc(abs(contribution)))
  
  corridor_summary
}

# --- 3. Run for each target node, compare corridor sum to cached total -----

attribution_results <- list()
summary_rows <- list()

for (node in TARGET_NODES) {
  result <- attribute_node(node, scenario = "mid")
  
  cached_total <- dependence_stratified_ba |>
    filter(node == !!node, measure == "raw_net_import_ratio", scenario == "mid") |>
    pull(weighted_difference)
  
  corridor_sum <- sum(result$contribution, na.rm = TRUE)
  gap <- cached_total - corridor_sum
  abs_total <- sum(abs(result$contribution), na.rm = TRUE)
  
  # TWO different "share" concepts, kept separate rather than conflated:
  #   pct_of_net_change: contribution / signed corridor sum. Can exceed
  #     100% (or be negative) when corridors partly offset each other —
  #     this is correct and reveals offsetting relationships, but is NOT
  #     usable for a concentration classification on its own.
  #   pct_of_absolute_change: |contribution| / sum(|contribution|). This is
  #     the concentration measure — always sums to 100%, used for the
  #     ONE_DOMINANT / SEVERAL_MEANINGFUL / BROADLY_DISTRIBUTED call below.
  result <- result |>
    mutate(
      pct_of_net_change = contribution / corridor_sum,
      pct_of_absolute_change = abs(contribution) / abs_total,
      cumulative_pct_absolute = cumsum(pct_of_absolute_change)
    )
  
  attribution_results[[node]] <- result
  write.csv(result, here("output", paste0("corridor_attribution_", node, ".csv")), row.names = FALSE)
  
  message("\n--- Corridor attribution: ", node, " ---")
  message("Node's cached total weighted_difference (raw_net_import_ratio, mid): ", round(cached_total, 4))
  message("Sum of corridor contributions: ", round(corridor_sum, 4),
          " (gap: ", round(gap, 4), ", ", round(100 * abs(gap) / abs(cached_total), 1), "% of total)")
  
  result |>
    select(counterparty, counterparty_name, contribution, pct_of_net_change,
           pct_of_absolute_change, cumulative_pct_absolute, retained_weight, eligible_strata) |>
    print(n = Inf, width = Inf)
  
  # Dominance classification based on ABSOLUTE concentration, not the
  # signed net share — top1/top3 as fraction of total |contribution|,
  # which always falls between 0 and 1 regardless of offsetting corridors.
  top1_share <- result$pct_of_absolute_change[1]
  top3_share <- sum(result$pct_of_absolute_change[1:min(3, nrow(result))])
  
  classification <- case_when(
    top1_share >= 0.50 ~ "ONE_DOMINANT_CORRIDOR",
    top3_share >= 0.80 ~ "SEVERAL_MEANINGFUL_CORRIDORS",
    TRUE ~ "BROADLY_DISTRIBUTED"
  )
  
  summary_rows[[node]] <- tibble(
    node = node, cached_total = cached_total, corridor_sum = corridor_sum,
    gap = gap, gap_pct_of_total = abs(gap) / abs(cached_total),
    n_corridors = nrow(result), top1_corridor = result$counterparty[1],
    top1_share_absolute = top1_share, top3_share_absolute = top3_share, classification = classification
  )
}

corridor_attribution_summary <- bind_rows(summary_rows)
write.csv(corridor_attribution_summary, here("output", "corridor_attribution_summary.csv"), row.names = FALSE)

message("\n=== CORRIDOR ATTRIBUTION SUMMARY ===")
print(corridor_attribution_summary, width = Inf)

# --- 4. Scenario consistency of the LEADING corridor ------------------------
# The node-level claims already survived lower/mid/upper (axis 3 in 10),
# but that does not automatically prove the SAME corridor leads under every
# scenario, or that its sign stays consistent. Check directly rather than
# assume it carries over.

message("\n=== Leading-corridor scenario consistency ===")

scenario_leader_rows <- list()
for (node in TARGET_NODES) {
  for (scenario in c("lower", "mid", "upper")) {
    res <- attribute_node(node, scenario = scenario)
    top <- res |> slice_max(abs(contribution), n = 1)
    scenario_leader_rows[[paste(node, scenario)]] <- tibble(
      node = node, scenario = scenario, leading_corridor = top$counterparty[1],
      contribution = top$contribution[1], sign = sign(top$contribution[1])
    )
  }
}
scenario_leaders <- bind_rows(scenario_leader_rows)
write.csv(scenario_leaders, here("output", "corridor_attribution_scenario_consistency.csv"), row.names = FALSE)
print(scenario_leaders, n = Inf, width = Inf)

leader_consistency <- scenario_leaders |>
  group_by(node) |>
  summarise(
    same_leader_all_scenarios = n_distinct(leading_corridor) == 1,
    same_sign_all_scenarios = n_distinct(sign) == 1,
    .groups = "drop"
  )

message("\n--- Leading-corridor consistency across lower/mid/upper ---")
print(leader_consistency, n = Inf, width = Inf)

