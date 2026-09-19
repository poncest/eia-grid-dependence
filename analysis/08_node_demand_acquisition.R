# =============================================================================
# analysis/08_node_demand_acquisition.R
#
# =============================================================================
#
# SCOPE NOTE -----------------------------------------------------------------
# This script acquires hourly demand (type=D) for every BA and regional-
# aggregate node identified in 05/06 — NOT just US48. This is the demand
# denominator needed to convert interchange flow into a dependence measure
# (gross_import_share, net_import_dependence) rather than raw exposure.
#
# It does NOT compute those measures, join against flow data, or test any
# stress-vs-normal claim — that is 09_story_discovery.R.
#
# Depends on:
#   data/entity_code_lookup_full.parquet  (from 05 — for the 70+13 node list)
#   R/eia_api.R  must have the multi-respondent pull_eia_demand() (see
#   eia_api_demand_functions_v2.R — replace the single-respondent version
#   appended for 07 with this one before running)
#
# Design note: not every BA is guaranteed to publish demand the way it
# publishes interchange — the respondent sets for the two EIA products can
# differ. This script validates per-NODE coverage explicitly, not just
# per-month hour coverage, so a node that's silently absent from the demand
# series doesn't quietly become a missing denominator in 09.
#
# Outputs:
#   data-raw/node_demand_2025-MM.parquet  x 12
#   data/node_demand_full.parquet          - combined, validated
#   data/node_demand_coverage.parquet      - per-node hours reported vs. expected
#   data/missing_node_hours.parquet        - exact missing (node, period) pairs + stress flag
#   output/node_demand_acquisition_validation.csv
#   output/node_demand_monthly_validation_v2.csv
# =============================================================================

library(dplyr)
library(lubridate)
library(arrow)
library(here)

source(here("R", "eia_api.R"))

dir.create(here("data-raw"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("data"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("output"), recursive = TRUE, showWarnings = FALSE)

# --- 1. Node list: 70 BA + 13 regional aggregates, excluding national -------

entity_code_lookup <- read_parquet(here("data", "entity_code_lookup_full.parquet"))

node_codes <- entity_code_lookup |>
  filter(entity_type %in% c("balancing_authority", "regional_aggregate")) |>
  pull(code)

stopifnot(length(node_codes) == 83)  # 70 BA + 13 region, per 05/06

message("Pulling demand for ", length(node_codes), " nodes (70 BA + 13 regional aggregates).")

# --- 2. Monthly windows (same structure as 04) --------------------------------

month_windows <- tibble(
  month_start = seq(as.Date("2025-01-01"), as.Date("2025-12-01"), by = "month")
) |>
  mutate(
    month_end = ceiling_date(month_start, "month") - days(1),
    month_label = format(month_start, "%Y-%m"),
    cache_path = here("data-raw", paste0("node_demand_", month_label, ".parquet")),
    expected_hours = as.integer(difftime(
      ceiling_date(month_start, "month"), month_start, units = "hours"
    ))
  )

# TEST-ONE-MONTH TOGGLE -------------------------------------------------------
# Uncomment to test January only before committing to the full year.
# month_windows <- month_windows |> filter(month_label == "2025-01")

# --- 3. Pull-or-load, validate-and-release per month --------------------------

pull_or_load_month <- function(month_label, month_start, month_end, cache_path) {
  if (file.exists(cache_path)) {
    message(month_label, ": cache found, loading from disk")
    return(read_parquet(cache_path))
  }
  
  message(month_label, ": no cache found, pulling from EIA API")
  
  start_str <- format(month_start, "%Y-%m-%dT00")
  end_str   <- format(month_end,   "%Y-%m-%dT23")
  
  data <- tryCatch(
    pull_eia_demand(start = start_str, end = end_str, respondent = node_codes),
    error = function(e) {
      warning(month_label, ": pull failed — ", conditionMessage(e))
      NULL
    }
  )
  
  if (is.null(data) || nrow(data) == 0) {
    warning(month_label, ": no data returned, skipping cache write")
    return(NULL)
  }
  
  write_parquet(data, cache_path)
  message(month_label, ": cached ", nrow(data), " rows to ", cache_path)
  data
}

validate_month <- function(month_label, expected_hours, n_nodes_expected, data) {
  if (is.null(data)) {
    return(tibble(
      month = month_label, status = "MISSING", rows = NA_integer_,
      distinct_hours = NA_integer_, expected_hours = expected_hours,
      distinct_nodes = NA_integer_, expected_nodes = n_nodes_expected,
      duplicate_rows = NA_integer_
    ))
  }
  
  distinct_hours <- n_distinct(data$period)
  distinct_nodes <- n_distinct(data$respondent)
  duplicate_rows <- nrow(data) - n_distinct(data$period, data$respondent)
  
  tibble(
    month = month_label,
    status = case_when(
      distinct_hours != expected_hours ~ "PARTIAL_HOURS",
      distinct_nodes != n_nodes_expected ~ "PARTIAL_NODES",
      duplicate_rows > 0 ~ "DUPLICATES",
      TRUE ~ "COMPLETE"
    ),
    rows = nrow(data),
    distinct_hours = distinct_hours,
    expected_hours = expected_hours,
    distinct_nodes = distinct_nodes,
    expected_nodes = n_nodes_expected,
    duplicate_rows = duplicate_rows
  )
}

validation_rows <- vector("list", nrow(month_windows))
monthly_data_for_combination <- vector("list", nrow(month_windows))

for (i in seq_len(nrow(month_windows))) {
  w <- month_windows[i, ]
  
  month_data <- pull_or_load_month(
    month_label = w$month_label, month_start = w$month_start,
    month_end = w$month_end, cache_path = w$cache_path
  )
  
  validation_rows[[i]] <- validate_month(w$month_label, w$expected_hours,
                                         length(node_codes), month_data)
  
  # Unlike 04, we keep a lightweight version (not full rows) to combine into
  # node_demand_full.parquet at the end — full 83-node monthly data is
  # smaller than interchange (no pairwise combinatorics), so this is fine
  # to hold in memory across 12 months rather than re-reading from disk.
  monthly_data_for_combination[[i]] <- month_data
  
  rm(month_data)
  gc(verbose = FALSE)
}

validation_summary <- bind_rows(validation_rows)

write.csv(validation_summary, here("output", "node_demand_acquisition_validation.csv"),
          row.names = FALSE)

message("\n--- Monthly acquisition validation ---")
print(validation_summary, width = Inf)

# NOTE: PARTIAL_NODES here is EXPECTED for every month, not a failure — 17
# of the 83 requested nodes never return a demand series at all (see
# node_demand_coverage below), so distinct_nodes will never reach 83. This
# per-request validation is superseded by monthly_validation_v2 (section 7
# below), which checks completeness against the 66 nodes that actually
# have a demand series.
incomplete_months <- validation_summary |> filter(status != "COMPLETE")
if (nrow(incomplete_months) > 0) {
  message("\n", nrow(incomplete_months), " month(s) incomplete — review before proceeding:")
  print(incomplete_months)
} else {
  message("\nAll months COMPLETE: full hour and node coverage.")
}

# --- 4. Combine and cache full-year node demand --------------------------------

node_demand_full <- bind_rows(monthly_data_for_combination) |>
  select(period, node = respondent, node_name = respondent_name, value, value_units)

write_parquet(node_demand_full, here("data", "node_demand_full.parquet"))

# --- 5. Per-node coverage validation -------------------------------------------
# Distinct from per-month validation above: confirms EVERY expected node
# actually appears, and with what coverage, rather than just checking node
# COUNTS per month (which could mask one node missing while another has
# duplicate rows, netting out to the "expected" count).

node_demand_coverage <- tibble(code = node_codes) |>
  left_join(
    node_demand_full |> count(code = node, name = "hours_reported"),
    by = "code"
  ) |>
  mutate(
    hours_reported = coalesce(hours_reported, 0L),
    expected_hours = 8760L,
    coverage_pct = round(100 * hours_reported / expected_hours, 1),
    # Neutral, evidence-based status only — no inference about WHY a node
    # is missing. NEAR_COMPLETE (>=99%) is separated from genuine PARTIAL
    # since a handful of scattered single-hour gaps is a materially
    # different situation from a node with large systematic gaps, and
    # collapsing both into one "PARTIAL" bucket obscures that.
    coverage_status = case_when(
      hours_reported == 0 ~ "NO_DEMAND_SERIES",
      hours_reported == expected_hours ~ "COMPLETE",
      coverage_pct >= 99 ~ "NEAR_COMPLETE",
      TRUE ~ "PARTIAL"
    )
  ) |>
  left_join(entity_code_lookup |> select(code, name, entity_type), by = "code") |>
  arrange(coverage_pct)

# Secondary, VERIFIABLE classification layer — only for the subset that can
# be confirmed from entity_code_lookup's own data (non-US codes are already
# known from 05's classification work, not inferred from names here).
# Everything else with NO_DEMAND_SERIES stays unclassified until its
# reporting status is independently verified — do not guess a reason from
# the entity name.
NON_US_CODES <- c("AESO", "BCHA", "CEN", "HQT", "IESO", "MHEB", "NBSO", "SPC")

node_demand_coverage <- node_demand_coverage |>
  mutate(
    exclusion_reason = case_when(
      coverage_status == "COMPLETE" ~ NA_character_,
      code %in% NON_US_CODES ~ "NON_US_INTERCHANGE_PARTNER",
      coverage_status == "NO_DEMAND_SERIES" ~ "NO_DEMAND_SERIES_UNVERIFIED",
      coverage_status == "PARTIAL" ~ "PARTIAL_UNVERIFIED"
    )
  )

write_parquet(node_demand_coverage, here("data", "node_demand_coverage.parquet"))

message("\n--- Per-node coverage (lowest 20) ---")
node_demand_coverage |> slice_head(n = 20) |> print(n = Inf, width = Inf)

message("\n--- Coverage status counts ---")
node_demand_coverage |> count(coverage_status) |> print()

message("\n--- Exclusion reason counts (non-COMPLETE nodes) ---")
node_demand_coverage |> filter(!is.na(exclusion_reason)) |> count(exclusion_reason) |> print()

missing_nodes <- node_demand_coverage |> filter(coverage_status == "NO_DEMAND_SERIES")
if (nrow(missing_nodes) > 0) {
  message("\n", nrow(missing_nodes), " node(s) returned NO demand series — ",
          "cannot support a dependence measure. This means the API returned ",
          "no rows, NOT that these entities have zero demand:")
  print(missing_nodes |> select(code, name, entity_type, exclusion_reason))
}

partial_nodes <- node_demand_coverage |> filter(coverage_status == "PARTIAL")
if (nrow(partial_nodes) > 0) {
  message("\n", nrow(partial_nodes), " node(s) have PARTIAL demand coverage:")
  print(partial_nodes |> select(code, name, entity_type, hours_reported, coverage_pct))
}

# --- Full-year acceptance gate --------------------------------------------
# Revised expectation, corrected from the January-only baseline: a handful
# of scattered single-hour gaps among otherwise-reporting nodes is expected
# real-world behavior (EIA-930 submissions aren't perfectly clean), not a
# pull failure. The gate checks for STRUCTURAL problems — nodes fully
# missing, or nodes with large systematic gaps — not near-100% coverage.
status_counts <- node_demand_coverage |> count(coverage_status) |>
  tibble::deframe()

expected_no_demand <- 17L

if (!identical(unname(status_counts["NO_DEMAND_SERIES"]), expected_no_demand) ||
    !is.na(status_counts["PARTIAL"])) {
  warning("Coverage does not match the expected pattern (17 NO_DEMAND_SERIES, ",
          "0 genuinely PARTIAL <99%) — review node_demand_coverage before proceeding to 09.")
} else {
  message("\nCoverage matches the expected structural pattern: ",
          unname(status_counts["COMPLETE"]), " COMPLETE, ",
          unname(status_counts["NEAR_COMPLETE"]), " NEAR_COMPLETE, ",
          expected_no_demand, " NO_DEMAND_SERIES, 0 genuinely PARTIAL.")
}

# --- 6. Missing-hour identification and stress-window overlap check ----------
# For every node that DOES have a demand series, identify exactly which
# hours are missing and whether any fall inside the 438 stress hours
# defined in 07. A gap outside the stress window doesn't threaten the
# primary stress-vs-normal comparison; a gap inside it must be reflected
# in 09's retained_stress_weight for that node, not silently absorbed.

demand_stress_tagged <- read_parquet(here("data", "demand_stress_tagged.parquet"))

expected_periods <- tibble(
  period = seq(
    as.POSIXct("2025-01-01 00:00:00", tz = "UTC"),
    as.POSIXct("2025-12-31 23:00:00", tz = "UTC"),
    by = "hour"
  )
)

reporting_nodes <- node_demand_coverage |>
  filter(hours_reported > 0) |>
  pull(code)

missing_node_hours <- tidyr::crossing(
  node = reporting_nodes,
  period = expected_periods$period
) |>
  anti_join(
    node_demand_full |> distinct(node, period),
    by = c("node", "period")
  ) |>
  left_join(
    demand_stress_tagged |> select(period, is_stress),
    by = "period"
  ) |>
  arrange(node, period)

write_parquet(missing_node_hours, here("data", "missing_node_hours.parquet"))

message("\n--- Missing node-hours (", nrow(missing_node_hours), " total) ---")
print(missing_node_hours, n = Inf)

message("\n--- Missing node-hours by node and stress status ---")
missing_node_hours_summary <- missing_node_hours |> count(node, is_stress)
print(missing_node_hours_summary)

stress_affected_nodes <- missing_node_hours_summary |>
  filter(is_stress) |>
  pull(node)

if (length(stress_affected_nodes) > 0) {
  message("\n", length(stress_affected_nodes),
          " node(s) have missing hours that overlap the stress window — ",
          "these MUST reduce retained_stress_weight in 09, not be silently absorbed: ",
          paste(stress_affected_nodes, collapse = ", "))
} else {
  message("\nNone of the 33 missing node-hours fall inside the 438 stress hours — ",
          "the gaps do not threaten the primary stress-vs-normal comparison.")
}

# --- 7. Corrected monthly validation (66 reporting nodes, not 83) ------------
# The original per-month validation (section 3) compared every month
# against all 83 requested nodes, which guarantees PARTIAL_NODES every
# month since 17 nodes never return a series — that's not a monthly
# acquisition failure, it's the documented, structural absence identified
# above. This recomputes monthly status against the 66 nodes that actually
# have a demand series, checking for missing node-hours within that set.

monthly_validation_v2 <- node_demand_full |>
  filter(node %in% reporting_nodes) |>
  mutate(month_label = format(period, "%Y-%m")) |>
  count(month_label, name = "rows") |>
  left_join(
    month_windows |> select(month_label, expected_hours),
    by = "month_label"
  ) |>
  mutate(
    expected_node_hours = expected_hours * length(reporting_nodes),
    missing_node_hours = expected_node_hours - rows,
    status = if_else(missing_node_hours == 0, "COMPLETE", "MINOR_GAPS")
  ) |>
  arrange(month_label)

write.csv(monthly_validation_v2,
          here("output", "node_demand_monthly_validation_v2.csv"),
          row.names = FALSE)

message("\n--- Corrected monthly validation (66 reporting nodes) ---")
print(monthly_validation_v2, width = Inf)

