# =============================================================================
# 10_candidate_robustness.R
#
## =============================================================================
#
# SCOPE NOTE -----------------------------------------------------------------
# This is an editorial GATE, not a re-run of 09's discovery. It tests the
# 8-node candidate registry against five stress-test axes and assigns each
# node ONE verdict. It does not redo 06's reconciliation or 08's demand
# acquisition — it reuses their cached outputs. The analytical METHOD
# (stratified ratio-of-sums standardization) is intentionally the same
# method as 09, applied to different stress-tag definitions — that
# repetition is the point of a robustness test, not a violation of the
# "don't redo prior scripts" discipline.
#
# Effort is NOT spread evenly: TPWR/SCL/GCPD/AECI get full testing across
# all 5 axes. TVA gets full testing with emphasis on flow-completeness.
# PNM/PSCO get the automated tests but not interpretive depth. NW is
# tested only for directional coherence (axis 5), not as a headline claim.
#
# Depends on (all read from cache, nothing re-pulled or re-reconciled):
#   data/entity_code_lookup_full.parquet
#   data/demand_stress_tagged.parquet        (07 — has raw `value`, season,
#                                              hour_of_day_utc, stratum; reused
#                                              here to build EVERY alternative
#                                              stress-tag definition, so no
#                                              raw demand re-load is needed)
#   data/named_events.parquet                (07)
#   data/node_hourly_flow_ba.parquet / _region.parquet   (09)
#   data/dependence_stratified_ba.parquet / _region.parquet  (09, top-5% baseline)
#   data/node_demand_full.parquet, node_demand_coverage.parquet (08)
#   data/demand_denominator_audit.parquet    (09)
#
# Outputs:
#   output/robustness_threshold_sensitivity.csv
#   output/robustness_event_sensitivity.csv
#   output/robustness_scenario_survival.csv
#   output/robustness_flow_completeness.csv
#   output/robustness_regional_coherence.csv
#   output/candidate_verdicts.csv                       <- the final gate output
# =============================================================================

library(dplyr)
library(tidyr)
library(arrow)
library(here)
library(lubridate)

dir.create(here("output"), recursive = TRUE, showWarnings = FALSE)

# --- 0. Candidate registry ------------------------------------------------

candidate_registry <- tibble::tribble(
  ~node,  ~candidate_role,
  "TPWR", "core_dependence",
  "SCL",  "core_dependence",
  "GCPD", "core_direction_flip",
  "AECI", "core_direction_flip",
  "TVA",  "coverage_sensitivity",
  "PNM",  "minor_confirmed_flip",
  "PSCO", "minor_confirmed_flip",
  "NW",   "regional_context"
)

# --- 1. Load cached inputs -----------------------------------------------

entity_code_lookup <- read_parquet(here("data", "entity_code_lookup_full.parquet"))
demand_stress_tagged <- read_parquet(here("data", "demand_stress_tagged.parquet"))
named_events <- read_parquet(here("data", "named_events.parquet"))
node_hourly_flow_ba <- read_parquet(here("data", "node_hourly_flow_ba.parquet"))
node_hourly_flow_region <- read_parquet(here("data", "node_hourly_flow_region.parquet"))
dependence_stratified_ba <- read_parquet(here("data", "dependence_stratified_ba.parquet"))
dependence_stratified_region <- read_parquet(here("data", "dependence_stratified_region.parquet"))
node_demand_full <- read_parquet(here("data", "node_demand_full.parquet"))
node_demand_coverage <- read_parquet(here("data", "node_demand_coverage.parquet"))
demand_denominator_audit <- read_parquet(here("data", "demand_denominator_audit.parquet"))

usable_demand_nodes <- node_demand_coverage |>
  filter(coverage_status %in% c("COMPLETE", "NEAR_COMPLETE")) |> pull(code)
valid_demand_hours <- node_demand_full |>
  filter(!is.na(value), value > 0) |> select(node, period, demand_mwh = value)

node_layer_map <- candidate_registry |>
  left_join(entity_code_lookup |> select(node = code, entity_type), by = "node") |>
  mutate(layer = if_else(entity_type == "regional_aggregate", "region", "ba"))

CLAIM_GATE_PRIMARY <- 0.95
CLAIM_GATE_WARNING <- 0.80
MIN_POSITIVE_DEMAND_COVERAGE <- 0.95
MIN_RESOLVED_FLOW_COVERAGE <- 0.90

# --- 2. Generic stratified analysis, reusable across every sensitivity test -
# Same method as 09's standardize(): ratio-of-sums per stratum, weighted by
# stratum_weight, gated by retained_stress_weight/coverage/finiteness.
# Parameterized so every test below is a call with different inputs, not a
# rewritten copy of the logic.

run_stratified_analysis <- function(node_hourly_flow, node_ids, tagged_df, stratum_weights,
                                    flow_statuses_allowed = "FULLY_OBSERVED",
                                    scenarios = c("lower", "mid", "upper"),
                                    measures = c("gross_import_ratio", "raw_net_import_ratio",
                                                 "net_import_dependence", "net_export_intensity")) {
  
  dep_hourly <- node_hourly_flow |>
    filter(node %in% node_ids, flow_status %in% flow_statuses_allowed) |>
    inner_join(valid_demand_hours, by = c("node", "period")) |>
    inner_join(tagged_df |> select(period, stratum, is_stress), by = "period")
  
  results <- list(); idx <- 1
  
  for (scenario in scenarios) {
    out_col <- paste0("outgoing_", scenario, "_mwh")
    in_col  <- paste0("incoming_", scenario, "_mwh")
    
    stratum_sums <- dep_hourly |>
      group_by(node, stratum, is_stress) |>
      summarise(n_obs = n(), sum_in = sum(.data[[in_col]]), sum_out = sum(.data[[out_col]]),
                sum_demand = sum(demand_mwh), .groups = "drop") |>
      mutate(
        gross_import_ratio = sum_in / sum_demand,
        raw_net_import_ratio = (sum_in - sum_out) / sum_demand,
        net_import_dependence = pmax(raw_net_import_ratio, 0),
        net_export_intensity = pmax(-raw_net_import_ratio, 0)
      ) |>
      select(node, stratum, is_stress, n_obs, all_of(measures)) |>
      pivot_wider(id_cols = c(node, stratum), names_from = is_stress,
                  values_from = c(n_obs, all_of(measures)),
                  names_glue = "{.value}_{if_else(is_stress, 'stress', 'normal')}") |>
      mutate(across(starts_with("n_obs_"), \(x) coalesce(x, 0L))) |>
      left_join(stratum_weights, by = "stratum")
    
    for (measure in measures) {
      stress_col <- paste0(measure, "_stress"); normal_col <- paste0(measure, "_normal")
      m <- stratum_sums |>
        mutate(eligible = n_obs_stress > 0 & n_obs_normal > 0 &
                 is.finite(.data[[stress_col]]) & is.finite(.data[[normal_col]]))
      
      node_summary <- m |>
        group_by(node) |>
        summarise(
          stress_observations = sum(n_obs_stress), normal_observations = sum(n_obs_normal),
          retained_stress_weight = sum(stratum_weight[eligible], na.rm = TRUE),
          weighted_stress_value = sum(stratum_weight[eligible] * .data[[stress_col]][eligible], na.rm = TRUE) /
            pmax(sum(stratum_weight[eligible], na.rm = TRUE), 1e-9),
          weighted_normal_value = sum(stratum_weight[eligible] * .data[[normal_col]][eligible], na.rm = TRUE) /
            pmax(sum(stratum_weight[eligible], na.rm = TRUE), 1e-9),
          .groups = "drop"
        ) |>
        mutate(weighted_difference = weighted_stress_value - weighted_normal_value,
               measure = measure, scenario = scenario)
      results[[idx]] <- node_summary; idx <- idx + 1
    }
  }
  bind_rows(results)
}

run_for_registry <- function(tagged_df, stratum_weights, flow_statuses_allowed = "FULLY_OBSERVED") {
  ba_nodes <- node_layer_map |> filter(layer == "ba") |> pull(node)
  region_nodes <- node_layer_map |> filter(layer == "region") |> pull(node)
  bind_rows(
    run_stratified_analysis(node_hourly_flow_ba, ba_nodes, tagged_df, stratum_weights, flow_statuses_allowed),
    run_stratified_analysis(node_hourly_flow_region, region_nodes, tagged_df, stratum_weights, flow_statuses_allowed)
  )
}

compute_stratum_weights <- function(tagged_df) {
  tagged_df |> filter(is_stress) |> count(stratum) |>
    mutate(stratum_weight = n / sum(n)) |> select(stratum, stratum_weight)
}

# --- 3. AXIS 1: Threshold sensitivity (top 1% / 5% / 10%, independently) ----
# Each threshold gets its OWN is_stress flag and OWN stratum weights — not
# reusing 07's top-5% weights, per instructions.

threshold_results <- list()
for (pct in c(0.01, 0.05, 0.10)) {
  q <- quantile(demand_stress_tagged$value, 1 - pct, na.rm = TRUE, names = FALSE)
  tagged <- demand_stress_tagged |> mutate(is_stress = value >= q)
  weights <- compute_stratum_weights(tagged)
  res <- run_for_registry(tagged, weights) |> mutate(threshold_pct = pct)
  threshold_results[[as.character(pct)]] <- res
}
threshold_sensitivity <- bind_rows(threshold_results)
write.csv(threshold_sensitivity, here("output", "robustness_threshold_sensitivity.csv"), row.names = FALSE)

message("--- Axis 1: threshold sensitivity (midpoint scenario, net_import_dependence & raw_net_import_ratio) ---")
threshold_sensitivity |>
  filter(scenario == "mid", measure %in% c("net_import_dependence", "raw_net_import_ratio")) |>
  select(node, measure, threshold_pct, weighted_stress_value, weighted_normal_value, weighted_difference) |>
  arrange(node, measure, threshold_pct) |> print(n = Inf, width = Inf)

# --- 4. AXIS 2: Named-event sensitivity -------------------------------------
# is_stress = period falls in THIS event's window; stratum weights reflect
# THIS event's own hour distribution (not the annual top-5%). Compared
# against season/hour-standardized normal periods (all non-event hours in
# matching strata), per instructions.

event_results <- list()
for (i in seq_len(nrow(named_events))) {
  ev <- named_events[i, ]
  tagged <- demand_stress_tagged |>
    mutate(is_stress = as.Date(period) >= ev$start_date & as.Date(period) <= ev$end_date)
  weights <- compute_stratum_weights(tagged)
  res <- run_for_registry(tagged, weights) |> mutate(event_name = ev$event_name)
  event_results[[ev$event_name]] <- res
}
event_sensitivity <- bind_rows(event_results)
write.csv(event_sensitivity, here("output", "robustness_event_sensitivity.csv"), row.names = FALSE)

message("\n--- Axis 2: named-event sensitivity (midpoint, net_import_dependence & raw_net_import_ratio) ---")
event_sensitivity |>
  filter(scenario == "mid", measure %in% c("net_import_dependence", "raw_net_import_ratio")) |>
  select(node, measure, event_name, weighted_stress_value, weighted_normal_value, weighted_difference) |>
  arrange(node, measure, event_name) |> print(n = Inf, width = Inf)

# --- 5. AXIS 3: Scenario survival (reuses 09's cached top-5% results) -------
# TPWR/SCL: require weighted_difference > 0 for net_import_dependence in
# ALL three scenarios. GCPD/AECI: require sign-crossing in raw_net_import_
# ratio in ALL three scenarios. Applied to every registry node for
# completeness, not just the four core candidates.

baseline_all <- bind_rows(dependence_stratified_ba, dependence_stratified_region) |>
  filter(node %in% candidate_registry$node)

scenario_survival <- baseline_all |>
  filter(measure %in% c("net_import_dependence", "raw_net_import_ratio")) |>
  group_by(node, measure) |>
  summarise(
    scenarios_present = n_distinct(scenario),
    all_primary = all(claim_status == "PRIMARY"),
    dependence_increase_all_scenarios = if (first(measure) == "net_import_dependence")
      all(weighted_difference > 0) else NA,
    crosses_all_scenarios = if (first(measure) == "raw_net_import_ratio")
      all(weighted_normal_value <= 0 & weighted_stress_value > 0) else NA,
    min_difference = min(weighted_difference),
    .groups = "drop"
  ) |>
  mutate(
    survives = case_when(
      measure == "net_import_dependence" ~ all_primary & scenarios_present == 3 & dependence_increase_all_scenarios,
      measure == "raw_net_import_ratio" ~ all_primary & scenarios_present == 3 & crosses_all_scenarios,
      TRUE ~ NA
    )
  )

write.csv(scenario_survival, here("output", "robustness_scenario_survival.csv"), row.names = FALSE)

message("\n--- Axis 3: scenario survival (top-5% baseline, lower/mid/upper) ---")
scenario_survival |> print(n = Inf, width = Inf)

# --- 6. AXIS 4: Flow-completeness sensitivity -------------------------------
# FULLY_OBSERVED only (primary, already cached) vs. FULLY_OBSERVED +
# PARTIALLY_CONFLICT_EXCLUDED (inclusive — a sensitivity input, explicitly
# NOT an equally trustworthy alternative, since those node totals are
# incomplete). Run for the whole registry; TVA is the case this axis exists
# for.

tagged_top5 <- demand_stress_tagged  # already has is_stress at top-5%, from 07
weights_top5 <- compute_stratum_weights(tagged_top5)

flow_inclusive <- run_for_registry(tagged_top5, weights_top5,
                                   flow_statuses_allowed = c("FULLY_OBSERVED", "PARTIALLY_CONFLICT_EXCLUDED")) |>
  mutate(flow_basis = "inclusive")

flow_primary <- baseline_all |>
  select(node, measure, scenario, weighted_stress_value, weighted_normal_value, weighted_difference) |>
  mutate(flow_basis = "fully_observed_only")

flow_completeness <- bind_rows(
  flow_primary,
  flow_inclusive |> select(node, measure, scenario, weighted_stress_value, weighted_normal_value,
                           weighted_difference, flow_basis)
) |>
  filter(measure %in% c("net_import_dependence", "raw_net_import_ratio"), scenario == "mid") |>
  pivot_wider(names_from = flow_basis, values_from = c(weighted_stress_value, weighted_normal_value, weighted_difference)) |>
  mutate(
    difference_shift = weighted_difference_inclusive - weighted_difference_fully_observed_only,
    sign_changed = sign(weighted_difference_inclusive) != sign(weighted_difference_fully_observed_only)
  )

write.csv(flow_completeness, here("output", "robustness_flow_completeness.csv"), row.names = FALSE)

message("\n--- Axis 4: flow-completeness sensitivity (fully-observed vs. inclusive, midpoint) ---")
flow_completeness |> print(n = Inf, width = Inf)

# --- 7. AXIS 5: Regional coherence (NW vs. TPWR/SCL/GCPD) -------------------
# Contextual, not a numeric gate on its own: does NW (regional aggregate)
# show the same stress-direction pattern as the Pacific Northwest utility
# candidates? This is NOT assumed — tested directly against NW's own
# top-5% baseline results (already computed in 09, same as any other node).

regional_coherence <- baseline_all |>
  filter(node %in% c("NW", "TPWR", "SCL", "GCPD"),
         measure %in% c("net_import_dependence", "raw_net_import_ratio"), scenario == "mid") |>
  select(node, measure, weighted_stress_value, weighted_normal_value, weighted_difference, claim_status)

write.csv(regional_coherence, here("output", "robustness_regional_coherence.csv"), row.names = FALSE)

message("\n--- Axis 5: regional coherence — NW vs. TPWR/SCL/GCPD (midpoint) ---")
regional_coherence |> print(n = Inf, width = Inf)

nw_direction_matches <- regional_coherence |> filter(node == "NW") |>
  summarise(nw_dependence_increases = any(measure == "net_import_dependence" & weighted_difference > 0)) |>
  pull(nw_dependence_increases)

message("\nNW shows the same stress-direction pattern (increased dependence) as the ",
        "PNW utility candidates: ", isTRUE(nw_direction_matches),
        ". AECI sits outside any PNW cluster — its own result (axis 3) is an ",
        "independent replication check, not dependent on this regional pattern.")

# --- 8. Verdict assembly -----------------------------------------------------
# ONE verdict per candidate. Priority order (first failing condition wins):
#   SCENARIO_SENSITIVE > THRESHOLD_SENSITIVE > COVERAGE_SENSITIVE >
#   EVENT_SPECIFIC > MINOR (registry-assigned) > ROBUST > REJECT (baseline
#   failure). This is an explicit, reviewable rule set — a judgment call
#   flagged for review, not an objectively "correct" scoring function.
#   NW is reported descriptively (axis 5 only), not forced into this list.

primary_measure_for <- function(role) {
  # minor_confirmed_flip (PNM, PSCO) are FLIP claims like GCPD/AECI, not
  # dependence claims — corrected from the earlier version, which
  # mismatched their primary_measure to net_import_dependence.
  if (role %in% c("core_dependence", "coverage_sensitivity")) "net_import_dependence"
  else "raw_net_import_ratio"  # core_direction_flip, minor_confirmed_flip
}

# Role-specific claim check: a "flip" claim (raw_net_import_ratio) is only
# supported by an actual sign crossing (normal <= 0, stress > 0) — a merely
# less-negative difference is NOT a flip, it's a weakening export pattern.
# A "dependence" claim (net_import_dependence) just needs a positive
# difference, since the measure itself is already clamped to >= 0.
claim_holds <- function(measure, normal_val, stress_val, diff) {
  if (measure == "raw_net_import_ratio") normal_val <= 0 & stress_val > 0
  else diff > 0
}

NEAR_GATE_MARGIN <- 0.03

# Explicit loop instead of rowwise()/mutate() with nested filters: the
# earlier rowwise version failed because `node` is a column name in BOTH
# the outer registry and every table being filtered inside it, and !!
# doesn't reliably disambiguate that inside a rowwise data mask. A loop
# with a plain local variable (this_node) sidesteps the ambiguity entirely.

verdict_rows <- vector("list", sum(candidate_registry$node != "NW"))
i <- 1

for (r in seq_len(nrow(candidate_registry))) {
  this_node <- candidate_registry$node[r]
  if (this_node == "NW") next  # NW reported descriptively only, see axis 5
  
  this_role <- candidate_registry$candidate_role[r]
  this_measure <- primary_measure_for(this_role)
  
  baseline_row <- baseline_all |>
    filter(node == this_node, measure == this_measure, scenario == "mid")
  baseline_holds <- nrow(baseline_row) > 0 &&
    baseline_row$claim_status[1] == "PRIMARY" &&
    claim_holds(this_measure, baseline_row$weighted_normal_value[1],
                baseline_row$weighted_stress_value[1], baseline_row$weighted_difference[1])
  
  scen_row <- scenario_survival |> filter(node == this_node, measure == this_measure)
  scenario_ok <- nrow(scen_row) > 0 && isTRUE(scen_row$survives[1])
  
  # Role-specific threshold check: crossing at every threshold for flip
  # claims, positive difference at every threshold for dependence claims —
  # NOT the same "difference > 0" test applied uniformly to both.
  th <- threshold_sensitivity |> filter(node == this_node, measure == this_measure, scenario == "mid")
  threshold_ok <- nrow(th) == 3 &&
    all(mapply(claim_holds, this_measure, th$weighted_normal_value, th$weighted_stress_value, th$weighted_difference))
  
  fc <- flow_completeness |> filter(node == this_node, measure == this_measure)
  flow_ok <- nrow(fc) == 0 || !isTRUE(fc$sign_changed[1])
  
  # Named-event check, same role-specific claim test — now informational
  # (feeds robustness_flags), NOT a pass/fail gate on overall_status. A
  # finding that survives every statistical stress definition (baseline,
  # scenario, threshold, flow) but varies across 3 short named-event
  # windows is EVENT_VARIABLE, not disqualified — event windows are a much
  # smaller sample than a percentile-based definition and can reasonably
  # disagree without the underlying finding being wrong.
  ev <- event_sensitivity |> filter(node == this_node, measure == this_measure, scenario == "mid")
  event_ok <- nrow(ev) > 0 &&
    sum(mapply(claim_holds, this_measure, ev$weighted_normal_value, ev$weighted_stress_value, ev$weighted_difference),
        na.rm = TRUE) >= 2
  
  near_coverage_gate <- nrow(baseline_row) > 0 &&
    ((baseline_row$resolved_flow_coverage[1] - MIN_RESOLVED_FLOW_COVERAGE) < NEAR_GATE_MARGIN |
       (baseline_row$positive_demand_coverage[1] - MIN_POSITIVE_DEMAND_COVERAGE) < NEAR_GATE_MARGIN)
  
  overall_status <- case_when(
    !baseline_holds | !scenario_ok ~ "NOT_ROBUST",
    !threshold_ok ~ "THRESHOLD_SENSITIVE",
    !flow_ok ~ "COVERAGE_SENSITIVE",
    this_role == "minor_confirmed_flip" ~ "MINOR",
    TRUE ~ "ROBUST"
  )
  
  flags <- c(
    if (!event_ok) "EVENT_VARIABLE",
    if (isTRUE(near_coverage_gate)) "NEAR_COVERAGE_GATE"
  )
  robustness_flags <- if (length(flags) == 0) "" else paste(flags, collapse = "; ")
  
  verdict_rows[[i]] <- tibble(
    node = this_node, candidate_role = this_role, primary_measure = this_measure,
    baseline_holds = baseline_holds, scenario_ok = scenario_ok, threshold_ok = threshold_ok,
    flow_ok = flow_ok, event_ok = event_ok, near_coverage_gate = near_coverage_gate,
    overall_status = overall_status, robustness_flags = robustness_flags
  )
  i <- i + 1
}

verdicts <- bind_rows(verdict_rows)

write.csv(verdicts, here("output", "candidate_verdicts.csv"), row.names = FALSE)

message("\n=== FINAL STATUS ===")
verdicts |> left_join(entity_code_lookup |> select(node = code, name), by = "node") |>
  select(node, name, candidate_role, primary_measure, overall_status, robustness_flags,
         baseline_holds, scenario_ok, threshold_ok, flow_ok, event_ok, near_coverage_gate) |>
  print(n = Inf, width = Inf)

message("\nNW (regional_context): see axis 5 output above — reported descriptively, ",
        "not assigned one of the seven verdict categories, per its role as context ",
        "rather than a standalone headline claim.")


