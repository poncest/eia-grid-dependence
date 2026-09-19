# =============================================================================
# analysis/05_reconciliation_full_year.R
#
# =============================================================================
#
# SCOPE NOTE -----------------------------------------------------------------
# This script reruns the reciprocal-reconciliation and chronic-connection
# classification logic validated in 02/03 across the full 2025 dataset
# cached by 04. It does NOT compute rankings, stability tests, or any
# editorial finding — that is 06_ranking_sensitivity_full_year.R, which
# should consume this script's cached outputs rather than recomputing them.
#
# Depends on:
#   data-raw/interchange_2025-MM.parquet  x 12  (from 04, all COMPLETE)
#
# Outputs (cached to data/ for reuse by 06+):
#   data/canonical_reports_full.parquet          - reconciled long-format flows
#   data/connection_discrepancies_full.parquet    - per-connection summary stats
#   data/chronic_connections_full.parquet         - connections flagged chronic
#   data/chronic_node_counts_full.parquet         - chronic-connection count per node
#   data/entity_code_lookup_full.parquet          - code/name/entity_type for all 86 nodes
#   data/chronic_connections_typed_full.parquet   - chronic connections by layer
#   output/reconciliation_full_year_summary.csv   - headline reconciliation rates
# =============================================================================

library(dplyr)
library(tidyr)
library(arrow)
library(here)

dir.create(here("data"), recursive = TRUE, showWarnings = FALSE)

# --- 1. Load the 12 monthly caches explicitly --------------------------------
# Explicit file list (not a directory glob) so the leftover single-day
# diagnostic caches from 01/02 (interchange_2025-07-01.parquet,
# interchange_2025-07-15.parquet) are never accidentally swept in.

month_labels <- sprintf("2025-%02d", 1:12)
month_files <- here("data-raw", paste0("interchange_", month_labels, ".parquet"))

stopifnot(all(file.exists(month_files)))

interchange_full <- month_files |>
  purrr::map(read_parquet) |>
  bind_rows()

message("Loaded ", nrow(interchange_full), " rows across ",
        length(month_files), " months.")

# --- 2. Canonicalize direction (same logic as 02, full year) -----------------
# ba_1/ba_2 = alphabetically ordered pair; canonical_value is signed so that
# positive = flow from ba_1 to ba_2.

canonical_reports_full <- interchange_full |>
  mutate(
    ba_1 = pmin(fromba, toba),
    ba_2 = pmax(fromba, toba),
    canonical_value = if_else(fromba == ba_1, value, -value)
  )

rm(interchange_full)
gc(verbose = FALSE)

# --- 3. Reciprocal reconciliation audit (full year) ---------------------------
# Mirrors 02's reciprocal_audit: for each (period, fromba, toba) report, find
# whether the reverse report exists and how much they disagree.

reverse_reports <- canonical_reports_full |>
  transmute(
    period,
    lookup_from = toba,
    lookup_to = fromba,
    reciprocal_value = value
  )

reciprocal_audit_full <- canonical_reports_full |>
  select(period, fromba, toba, value) |>
  left_join(
    reverse_reports,
    by = c("period", "fromba" = "lookup_from", "toba" = "lookup_to")
  ) |>
  mutate(
    reciprocal_present = !is.na(reciprocal_value),
    reciprocal_gap = value + reciprocal_value,
    absolute_gap = abs(reciprocal_gap),
    relative_gap = absolute_gap / pmax(abs(value), abs(reciprocal_value), 1)
  )

reciprocal_summary_full <- reciprocal_audit_full |>
  summarise(
    observations = n(),
    reciprocal_coverage = mean(reciprocal_present),
    median_absolute_gap = median(absolute_gap[reciprocal_present], na.rm = TRUE),
    p95_absolute_gap = quantile(absolute_gap[reciprocal_present], 0.95,
                                na.rm = TRUE, names = FALSE),
    within_5_percent = mean(relative_gap[reciprocal_present] <= 0.05, na.rm = TRUE)
  )

rm(reverse_reports, reciprocal_audit_full)
gc(verbose = FALSE)

# --- 4. Per-connection discrepancy summary (full year) ------------------------
# Mirrors connection_discrepancies from 02, computed once per (ba_1, ba_2)
# across all 12 months rather than a single day.

canonical_signed <- canonical_reports_full |>
  group_by(period, ba_1, ba_2) |>
  summarise(
    reports = n(),
    values = list(canonical_value),
    .groups = "drop"
  )

connection_pair_hours <- canonical_signed |>
  rowwise() |>
  mutate(
    signs = list(sign(unlist(values))),
    direction_conflict = reports == 2 &&
      length(unique(unlist(signs)[unlist(signs) != 0])) > 1,
    flow_estimate = if (direction_conflict) NA_real_ else mean(unlist(values)),
    report_spread = if (reports == 2 && !direction_conflict)
      max(unlist(values)) - min(unlist(values)) else 0,
    relative_spread = if (reports == 2 && !direction_conflict)
      report_spread / pmax(max(abs(unlist(values))), 1) else 0,
    is_discrepant = reports == 2 && !direction_conflict && relative_spread > 0.05
  ) |>
  ungroup() |>
  select(period, ba_1, ba_2, reports, direction_conflict,
         flow_estimate, report_spread, relative_spread, is_discrepant)

connection_discrepancies_full <- connection_pair_hours |>
  group_by(ba_1, ba_2) |>
  summarise(
    pair_hours = n(),
    single_report_hours = sum(reports == 1),
    agreement_hours = sum(reports == 2 & !direction_conflict & !is_discrepant),
    discrepancy_hours = sum(is_discrepant),
    direction_conflict_hours = sum(direction_conflict),
    discrepancy_rate = discrepancy_hours / pair_hours,
    median_flow_mwh = median(abs(flow_estimate), na.rm = TRUE),
    median_report_spread = median(report_spread[reports == 2 & !direction_conflict],
                                  na.rm = TRUE),
    .groups = "drop"
  )

# --- 5. Chronic-connection classification (full-year thresholds) -------------
# The 03 thresholds (pair_hours >= 18, discrepancy_rate >= 0.75) were sized
# for a 24-hour sample. At full-year scale they need rescaling: a connection
# should have MEANINGFUL coverage (not just a handful of observed hours,
# possibly from a BA that only reported briefly) before its discrepancy_rate
# is trusted. MIN_PAIR_HOURS below is a starting point, not a validated
# choice — review against connection_discrepancies_full's distribution of
# pair_hours before treating chronic_connections_full as final.

MIN_PAIR_HOURS <- 1000   # roughly 6 weeks of hourly coverage; adjust after review
CHRONIC_DISCREPANCY_RATE <- 0.75

chronic_connections_full <- connection_discrepancies_full |>
  filter(pair_hours >= MIN_PAIR_HOURS, discrepancy_rate >= CHRONIC_DISCREPANCY_RATE) |>
  arrange(desc(discrepancy_rate))

# --- 6. Cache outputs for 06+ -------------------------------------------------

write_parquet(canonical_reports_full, here("data", "canonical_reports_full.parquet"))
write_parquet(connection_discrepancies_full,
              here("data", "connection_discrepancies_full.parquet"))
write_parquet(chronic_connections_full, here("data", "chronic_connections_full.parquet"))

# --- 7. Headline summary -------------------------------------------------------

full_year_direction_conflicts <- connection_discrepancies_full |>
  summarise(
    total_pair_hours = sum(pair_hours),
    total_direction_conflict_hours = sum(direction_conflict_hours),
    direction_conflict_pct = total_direction_conflict_hours / total_pair_hours
  )

reconciliation_summary_full <- bind_cols(
  reciprocal_summary_full,
  full_year_direction_conflicts
)

write.csv(reconciliation_summary_full,
          here("output", "reconciliation_full_year_summary.csv"),
          row.names = FALSE)

message("\n--- Full-year reconciliation summary ---")
print(reconciliation_summary_full, width = Inf)

message("\n--- Pair-hours distribution (informs MIN_PAIR_HOURS review) ---")
print(summary(connection_discrepancies_full$pair_hours))

message("\n--- Chronic connections (full-year thresholds) ---")
print(chronic_connections_full, n = Inf, width = Inf)

message(
  "\n", nrow(chronic_connections_full),
  " connection(s) classified chronic out of ",
  nrow(connection_discrepancies_full), " total connections observed in 2025."
)

# --- 8. Chronic exposure by node -------------------------------------------------
# How many of the 12 chronic connections touch each node (BA, region, or
# national aggregate) — reveals whether chronic disagreement is concentrated
# around specific reporting nodes (as it turns out to be: PJM and MIDA
# account for 9 of 12) or scattered.

chronic_node_counts <- chronic_connections_full |>
  count(node = ba_1, name = "as_ba_1") |>
  full_join(
    chronic_connections_full |> count(node = ba_2, name = "as_ba_2"),
    by = "node"
  ) |>
  mutate(
    across(c(as_ba_1, as_ba_2), ~ coalesce(.x, 0L)),
    chronic_connections = as_ba_1 + as_ba_2
  ) |>
  arrange(desc(chronic_connections), node)

write_parquet(chronic_node_counts, here("data", "chronic_node_counts_full.parquet"))

message("\n--- Chronic exposure by node ---")
print(chronic_node_counts, n = Inf)

# --- 9. Entity code-to-name lookup with entity-type classification ---------------
# EIA's Hourly Electric Grid Monitor defines 13 fixed REGIONAL aggregates,
# distinct from the ~60 individual balancing authorities. This is a closed,
# documented list (not inferred from naming patterns), per EIA:
#   https://www.thetrading.tools/grid-demand ; EIA Hourly Electric Grid Monitor
#
# Everything NOT in this list is treated as a genuine balancing authority —
# including large RTOs/ISOs like PJM, MISO, CISO, which EIA classifies as BAs
# despite spanning many states, distinct from the 13 region constructs.

EIA_REGIONS <- c(
  "CAL",  # California
  "CAR",  # Carolinas
  "CENT", # Central
  "FLA",  # Florida
  "MIDA", # Mid-Atlantic
  "MIDW", # Midwest
  "NE",   # New England
  "NY",   # New York
  "NW",   # Northwest
  "SE",   # Southeast
  "SW",   # Southwest
  "TEN",  # Tennessee
  "TEX"   # Texas
)

# Separate from the 13 regions above: cross-border/national aggregate nodes
# that also are not individual balancing authorities.
EIA_NATIONAL_AGGREGATES <- c("CAN", "MEX", "US48")

entity_code_lookup <- bind_rows(
  canonical_reports_full |> distinct(code = fromba, name = fromba_name),
  canonical_reports_full |> distinct(code = toba, name = toba_name)
) |>
  distinct(code, name) |>
  mutate(
    entity_type = case_when(
      code %in% EIA_REGIONS ~ "regional_aggregate",
      code %in% EIA_NATIONAL_AGGREGATES ~ "national_aggregate",
      TRUE ~ "balancing_authority"
    )
  ) |>
  arrange(entity_type, code)

write_parquet(entity_code_lookup, here("data", "entity_code_lookup_full.parquet"))

message("\n--- Full code-to-name-to-type lookup (", nrow(entity_code_lookup), " nodes) ---")
print(entity_code_lookup, n = Inf)

message("\n--- Entity type counts ---")
entity_code_lookup |> count(entity_type) |> print()

# --- 10. Split chronic connections by entity-type layer ----------------------
# Region-to-region and BA-to-BA chronic disagreement are different
# phenomena and should not be pooled into one explanation or one chart.
# A chord diagram / flow map mixing both node types would visually imply
# equivalent entities operating at the same aggregation level, which they
# do not. These two layers should be treated as separate primary/diagnostic
# views downstream, not combined into a single network.

chronic_connections_typed <- chronic_connections_full |>
  left_join(entity_code_lookup |> select(code, type_1 = entity_type),
            by = c("ba_1" = "code")) |>
  left_join(entity_code_lookup |> select(code, type_2 = entity_type),
            by = c("ba_2" = "code")) |>
  mutate(
    connection_layer = case_when(
      is.na(type_1) | is.na(type_2) ~ "unclassified",
      type_1 == "regional_aggregate" & type_2 == "regional_aggregate" ~ "region_to_region",
      type_1 == "balancing_authority" & type_2 == "balancing_authority" ~ "ba_to_ba",
      TRUE ~ "mixed"
    )
  )

stopifnot(!any(chronic_connections_typed$connection_layer == "unclassified"))

write_parquet(chronic_connections_typed, here("data", "chronic_connections_typed_full.parquet"))

message("\n--- Chronic connections by layer ---")
chronic_connections_typed |> count(connection_layer) |> print()
chronic_connections_typed |>
  select(ba_1, ba_2, connection_layer, discrepancy_rate) |>
  arrange(connection_layer, desc(discrepancy_rate)) |>
  print(n = Inf)

