# =============================================================================
# analysis/03_reconciliation_sensitivity.R
# =============================================================================
#
# Purpose:
#   Test whether top-5 BA rankings (outgoing / incoming / throughput) survive
#   lower / midpoint / upper reconciliation estimates for reciprocal reports.
#
# Depends on (source before running):
#   source(here::here("analysis", "01_viability_audit.R"))
#   source(here::here("analysis", "02_reconciliation_audit.R"))
#
# Requires from 02:
#   canonical_reports        - period, ba_1, ba_2, canonical_value (signed,
#                               positive = ba_1 -> ba_2)
#   connection_discrepancies - pair_hours, discrepancy_rate per connection
#
# Exclusion logic:
#   - Direction conflicts are excluded at the PAIR-HOUR level (that hour is
#     dropped from all primary scenarios; other hours for the same
#     connection are retained).
#   - Chronic connections are excluded at the CONNECTION level, but ONLY in
#     the diagnostic "midpoint_excl_chronic" scenario, not the primary
#     lower/midpoint/upper scenarios.
#
# Outputs:
#   ba_totals, ba_ranks             - per-BA, per-scenario totals and ranks
#   outgoing/incoming/throughput_stability - PASS/WARN/FAIL verdicts
#   outgoing/incoming/throughput_tau       - Kendall tau on common top-5 set
#   chronic_contribution             - how much of each BA's midpoint total
#                                       comes from chronic-disagreement pairs
# =============================================================================

# SCOPE NOTE ---------------------------------------------------------------
#
# This script is a one-day diagnostic using July 15, 2025 data.
#
# Its purpose is to validate:
#   - reciprocal-report reconciliation;
#   - lower/midpoint/upper sensitivity estimates;
#   - direction-conflict exclusions;
#   - provisional chronic-connection classification; and
#   - rank-stability tests.
#
# Results from this script—including chronic-connection membership,
# BA percentages, rankings, and PASS/WARN verdicts—are not full-year
# findings and should not be cited outside this diagnostic context.
#
# analysis/04_full_year_acquisition.R will reclassify connections and
# rerun all substantive metrics using the complete 2025 dataset.

library(dplyr)
library(tidyr)

# --- 1. Define chronic connections (whole-connection exclusion, diagnostic scenario only) ---

chronic_connections <- connection_discrepancies |>
  filter(pair_hours >= 18, discrepancy_rate >= 0.75) |>
  distinct(ba_1, ba_2)

# --- 2. Build per-pair-hour lower/mid/upper estimates ---

pair_hour_estimates <- canonical_reports |>
  group_by(period, ba_1, ba_2) |>
  summarise(
    reports = n(),
    values = list(canonical_value),
    .groups = "drop"
  ) |>
  rowwise() |>
  mutate(
    signs = list(sign(unlist(values))),
    direction_conflict = reports == 2 &&
      length(unique(unlist(signs)[unlist(signs) != 0])) > 1,
    lower_signed  = if (direction_conflict) NA_real_ else
      unlist(values)[which.min(abs(unlist(values)))],
    upper_signed  = if (direction_conflict) NA_real_ else
      unlist(values)[which.max(abs(unlist(values)))],
    mid_signed    = if (direction_conflict) NA_real_ else mean(unlist(values))
  ) |>
  ungroup() |>
  select(period, ba_1, ba_2, reports, direction_conflict,
         lower_signed, mid_signed, upper_signed) |>
  mutate(
    is_chronic = paste(ba_1, ba_2) %in%
      paste(chronic_connections$ba_1, chronic_connections$ba_2)
  )

# Sanity check: direction-conflict count should match reconciliation_summary
stopifnot(sum(pair_hour_estimates$direction_conflict) ==
            sum(!is.na(pair_hour_estimates$reports) &
                  pair_hour_estimates$direction_conflict))

# --- 3. Pivot to long form: one row per (BA pair, period) with out/in roles ---

build_ba_flows <- function(data, value_col) {
  data |>
    filter(!direction_conflict) |>
    transmute(
      period,
      out_ba = if_else(.data[[value_col]] >= 0, ba_1, ba_2),
      in_ba  = if_else(.data[[value_col]] >= 0, ba_2, ba_1),
      flow_mwh = abs(.data[[value_col]]),
      is_chronic
    )
}

flows_lower <- build_ba_flows(pair_hour_estimates, "lower_signed")
flows_mid   <- build_ba_flows(pair_hour_estimates, "mid_signed")
flows_upper <- build_ba_flows(pair_hour_estimates, "upper_signed")
flows_mid_excl_chronic <- flows_mid |> filter(!is_chronic)

# --- 4. Aggregate to daily totals per BA, per scenario ---

summarize_ba_totals <- function(flows, scenario_name) {
  outgoing <- flows |> group_by(ba = out_ba) |>
    summarise(outgoing_mwh = sum(flow_mwh), .groups = "drop")
  incoming <- flows |> group_by(ba = in_ba) |>
    summarise(incoming_mwh = sum(flow_mwh), .groups = "drop")
  
  full_join(outgoing, incoming, by = "ba") |>
    mutate(
      across(c(outgoing_mwh, incoming_mwh), \(x) replace_na(x, 0)),
      throughput_mwh = outgoing_mwh + incoming_mwh,
      net_flow_mwh = outgoing_mwh - incoming_mwh,
      scenario = scenario_name
    )
}

ba_totals <- bind_rows(
  summarize_ba_totals(flows_lower, "lower"),
  summarize_ba_totals(flows_mid, "midpoint"),
  summarize_ba_totals(flows_upper, "upper"),
  summarize_ba_totals(flows_mid_excl_chronic, "midpoint_excl_chronic")
)

# --- 5. Rank BAs per scenario, per measure ---

ba_ranks <- ba_totals |>
  group_by(scenario) |>
  mutate(
    rank_outgoing   = rank(-outgoing_mwh, ties.method = "min"),
    rank_incoming   = rank(-incoming_mwh, ties.method = "min"),
    rank_throughput = rank(-throughput_mwh, ties.method = "min")
  ) |>
  ungroup()

top5 <- function(measure_rank_col) {
  ba_ranks |>
    filter(.data[[measure_rank_col]] <= 5,
           scenario %in% c("lower", "midpoint", "upper")) |>
    select(scenario, ba, rank = all_of(measure_rank_col))
}

# --- 6. Stability verdict: set overlap across lower/mid/upper ---

stability_verdict <- function(top5_table) {
  sets <- top5_table |> group_by(scenario) |>
    summarise(bas = list(sort(ba)), .groups = "drop")
  common <- Reduce(intersect, sets$bas)
  n_common <- length(common)
  
  leader <- top5_table |> filter(rank == 1) |> pull(ba)
  same_leader <- length(unique(leader)) == 1
  
  verdict <- case_when(
    n_common == 5 ~ "PASS",
    n_common == 4 ~ "WARN",
    TRUE ~ "FAIL"
  )
  
  list(
    verdict = verdict,
    common_bas = common,
    n_common = n_common,
    same_leader_across_scenarios = same_leader,
    leader_by_scenario = top5_table |> filter(rank == 1) |> select(scenario, ba)
  )
}

outgoing_stability   <- stability_verdict(top5("rank_outgoing"))
incoming_stability   <- stability_verdict(top5("rank_incoming"))
throughput_stability <- stability_verdict(top5("rank_throughput"))

# --- 7. Kendall rank correlation among common top-5 members ---

kendall_common <- function(top5_table, common_bas) {
  if (length(common_bas) < 3) return(NA_real_)  # too few pairs for a meaningful tau
  
  wide <- top5_table |>
    filter(ba %in% common_bas) |>
    pivot_wider(names_from = scenario, values_from = rank)
  
  cor(wide$lower, wide$upper, method = "kendall")
}

outgoing_tau   <- kendall_common(top5("rank_outgoing"), outgoing_stability$common_bas)
incoming_tau   <- kendall_common(top5("rank_incoming"), incoming_stability$common_bas)
throughput_tau <- kendall_common(top5("rank_throughput"), throughput_stability$common_bas)

# --- 8. Chronic-connection contribution diagnostics ---
# How much of each BA's midpoint incoming/outgoing total comes from
# connections flagged as chronic (pair_hours >= 18, discrepancy_rate >= 0.75)?

chronic_incoming <- flows_mid |>
  filter(is_chronic) |>
  group_by(ba = in_ba) |>
  summarise(chronic_incoming_mwh = sum(flow_mwh), .groups = "drop")

chronic_outgoing <- flows_mid |>
  filter(is_chronic) |>
  group_by(ba = out_ba) |>
  summarise(chronic_outgoing_mwh = sum(flow_mwh), .groups = "drop")

chronic_contribution <- ba_totals |>
  filter(scenario == "midpoint") |>
  left_join(chronic_incoming, by = "ba") |>
  left_join(chronic_outgoing, by = "ba") |>
  mutate(
    across(c(chronic_incoming_mwh, chronic_outgoing_mwh), \(x) replace_na(x, 0)),
    chronic_incoming_pct = chronic_incoming_mwh / pmax(incoming_mwh, 1),
    chronic_outgoing_pct = chronic_outgoing_mwh / pmax(outgoing_mwh, 1),
    
    # Chronic exposure measured against total throughput, not just one side
    # of the ledger — a BA can be 100% chronic on incoming while its
    # (larger, clean) outgoing volume still stabilizes its throughput rank.
    throughput_mwh = incoming_mwh + outgoing_mwh,
    chronic_throughput_mwh = chronic_incoming_mwh + chronic_outgoing_mwh,
    chronic_throughput_pct = chronic_throughput_mwh / pmax(throughput_mwh, 1)
  ) |>
  arrange(desc(chronic_throughput_pct))

# Which specific chronic connection(s) involve a given BA (descriptive only —
# does NOT trigger any exclusion rule). One corridor suggests a localized
# issue worth testing across 2025; several suggests a possible BA-level
# reporting pattern worth investigating in `04`.
chronic_connections_for <- function(ba_code) {
  chronic_connections |>
    filter(ba_1 == ba_code | ba_2 == ba_code) |>
    arrange(ba_1, ba_2)
}

midw_chronic_connections <- chronic_connections_for("MIDW")
pjm_chronic_connections  <- chronic_connections_for("PJM")

# --- 9. Report ---

stability_report <- list(
  outgoing   = outgoing_stability,
  incoming   = incoming_stability,
  throughput = throughput_stability
)

tau_report <- c(
  outgoing_tau   = outgoing_tau,
  incoming_tau   = incoming_tau,
  throughput_tau = throughput_tau
)

stability_report
tau_report

chronic_contribution |>
  select(ba, incoming_mwh, outgoing_mwh, throughput_mwh,
         chronic_incoming_pct, chronic_outgoing_pct, chronic_throughput_pct) |>
  print(n = 15, width = Inf)

midw_chronic_connections
pjm_chronic_connections
