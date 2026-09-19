# =============================================================================
# analysis/07_demand_acquisition_and_stress_definition.R
#
# =============================================================================
#
# SCOPE NOTE -----------------------------------------------------------------
# This script acquires and validates hourly US48 (Lower-48 aggregate) demand
# for 2025, defines the system-wide top-5% stress threshold, and tags every
# hour with the season/hour-of-day stratum needed to build matched normal
# comparisons later. It does NOT select or test any dependence claim — that
# is 08_story_discovery.R, which should consume this script's cached outputs.
#
# Depends on:
#   R/eia_api.R must include pull_eia_demand() (see eia_api_demand_functions.R
#   — append that to eia_api.R before running this script)
#
# Design decisions made here, confirmed with the user:
#   - Respondent: US48 (system-wide), not per-BA — matches the "system-wide
#     top 5% of hours by total US48 demand" stress definition.
#   - Series: EIA's as-published hourly demand (type=D). There is no
#     separate "adjusted" facet in this endpoint; if a distinct adjusted
#     series is needed later, that's a different EIA product, not a
#     different facet here.
#   - Stress threshold: quantile-based (95th percentile of hourly demand),
#     not a fixed hour-count — ties can push slightly above/below exactly
#     5% of hours; the actual share flagged is checked in the integrity
#     section below.
#   - Season: meteorological (DJF/MAM/JJA/SON), not calendar quarter, since
#     heating/cooling-driven demand aligns with meteorological seasons.
#
# Outputs (cached to data/ and output/):
#   data-raw/demand_us48_2025.parquet       - raw pulled demand series
#   data/demand_stress_tagged.parquet       - full year, season/hour/stress tags
#   data/stress_hours.parquet               - just the flagged top-5% hours
#   data/season_hour_strata_summary.parquet - pool sizes, stratum_weight, and
#                                              1:1-matching feasibility per stratum
#   data/named_events.parquet               - sourced 2025 extreme-demand events
#   output/demand_acquisition_validation.csv
# =============================================================================

library(dplyr)
library(lubridate)
library(arrow)
library(here)

source(here("R", "eia_api.R"))

dir.create(here("data-raw"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("data"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("output"), recursive = TRUE, showWarnings = FALSE)

# --- 1. Pull or load full-year US48 demand ------------------------------------

demand_cache_path <- here("data-raw", "demand_us48_2025.parquet")

if (file.exists(demand_cache_path)) {
  message("Cache found, loading US48 demand from disk")
  demand_raw <- read_parquet(demand_cache_path)
} else {
  message("No cache found, pulling US48 demand from EIA API")
  demand_raw <- pull_eia_demand(
    start = "2025-01-01T00",
    end   = "2025-12-31T23",
    respondent = "US48"
  )
  write_parquet(demand_raw, demand_cache_path)
  message("Cached ", nrow(demand_raw), " rows to ", demand_cache_path)
}

# --- 2. Validate ---------------------------------------------------------------

expected_hours <- 8760L  # 2025 is not a leap year

distinct_hours <- n_distinct(demand_raw$period)
duplicate_rows <- nrow(demand_raw) - n_distinct(demand_raw$period)
missing_values <- sum(is.na(demand_raw$value))

validation_summary <- tibble(
  rows = nrow(demand_raw),
  distinct_hours = distinct_hours,
  expected_hours = expected_hours,
  hours_coverage_pct = round(100 * distinct_hours / expected_hours, 1),
  duplicate_rows = duplicate_rows,
  missing_values = missing_values,
  status = case_when(
    distinct_hours != expected_hours ~ "PARTIAL",
    duplicate_rows > 0 ~ "DUPLICATES",
    missing_values > 0 ~ "MISSING_VALUES",
    TRUE ~ "COMPLETE"
  )
)

write.csv(validation_summary, here("output", "demand_acquisition_validation.csv"),
          row.names = FALSE)

message("\n--- Demand acquisition validation ---")
print(validation_summary, width = Inf)

stopifnot(validation_summary$status == "COMPLETE")

# --- 3. Season/hour tagging + stress threshold ---------------------------------

meteorological_season <- function(month) {
  case_when(
    month %in% c(12, 1, 2) ~ "winter",
    month %in% c(3, 4, 5)  ~ "spring",
    month %in% c(6, 7, 8)  ~ "summer",
    month %in% c(9, 10, 11) ~ "fall"
  )
}

STRESS_PERCENTILE <- 0.95  # top 5% of hours by US48 demand

stress_threshold <- quantile(demand_raw$value, STRESS_PERCENTILE, na.rm = TRUE, names = FALSE)

demand_stress_tagged <- demand_raw |>
  select(period, respondent, value, value_units) |>
  mutate(
    # EIA hourly grid monitor periods are UTC (consistent with the
    # interchange data used throughout 01-06) — labeled explicitly here
    # since hour-of-day strata are meaningless without a stated timezone.
    month = month(period),
    hour_of_day_utc = hour(period),
    season = meteorological_season(month),
    stratum = paste(season, sprintf("%02d", hour_of_day_utc)),
    is_stress = value >= stress_threshold
  )

write_parquet(demand_stress_tagged, here("data", "demand_stress_tagged.parquet"))

stress_hours <- demand_stress_tagged |> filter(is_stress)
write_parquet(stress_hours, here("data", "stress_hours.parquet"))

actual_stress_pct <- round(100 * nrow(stress_hours) / nrow(demand_stress_tagged), 2)

message("\n--- Stress threshold ---")
message("95th percentile demand threshold: ", round(stress_threshold, 0), " ",
        unique(demand_stress_tagged$value_units))
message(nrow(stress_hours), " hours flagged as stress (",
        actual_stress_pct, "% of ", nrow(demand_stress_tagged), " total hours)")

# Sanity check: quantile-based flagging should land close to 5%, but exact
# ties or the quantile method could push it slightly off — flag if it's
# meaningfully off-target rather than silently trusting the quantile call.
if (abs(actual_stress_pct - 5) > 0.5) {
  warning("Stress hours are ", actual_stress_pct,
          "% of the year, notably different from the intended 5% target — review threshold method.")
}

# --- 4. Season/hour strata summary (for 08's stratified standardization) -----
# 08 will use this for stratified standardization, NOT 1:1 matching: several
# late-evening summer strata contain MORE stress hours than normal hours
# (e.g. summer 22:00 UTC: 60 stress vs. 32 normal), making one-to-one
# matching without replacement infeasible for those strata. Reusing normal
# hours to force 1:1 pairs would inflate the apparent comparison sample, so
# that approach is deliberately not used here.
#
# Instead: 08 should (1) compute stress vs. normal outcome means WITHIN each
# stratum, (2) weight each stratum by its share of the 438 total stress
# hours (stratum_weight below), (3) combine into a weighted overall
# estimate. This controls for season and hour-of-day without manufacturing
# duplicate observations. stratum_weight is provided here so 08 doesn't
# need to recompute it.

season_hour_strata_summary <- demand_stress_tagged |>
  group_by(stratum, season, hour_of_day_utc) |>
  summarise(
    total_hours = n(),
    stress_hours = sum(is_stress),
    normal_hours = sum(!is_stress),
    .groups = "drop"
  ) |>
  mutate(
    stratum_weight = stress_hours / sum(stress_hours),
    matching_feasible_1to1 = normal_hours >= stress_hours
  ) |>
  arrange(desc(stress_hours))

write_parquet(season_hour_strata_summary, here("data", "season_hour_strata_summary.parquet"))

message("\n--- Season/hour strata summary (top 10 by stress-hour count) ---")
season_hour_strata_summary |> slice_head(n = 10) |> print()

infeasible_1to1_strata <- season_hour_strata_summary |>
  filter(!matching_feasible_1to1, stress_hours > 0)

message("\n", nrow(infeasible_1to1_strata),
        " stratum/strata have MORE stress hours than normal hours ",
        "(1:1 matching without replacement is infeasible; use stratified ",
        "standardization instead):")
print(infeasible_1to1_strata |> select(stratum, stress_hours, normal_hours, stratum_weight))

empty_normal_pools <- season_hour_strata_summary |>
  filter(stress_hours > 0, normal_hours == 0)

if (nrow(empty_normal_pools) > 0) {
  warning(nrow(empty_normal_pools),
          " stratum/strata have stress hours but NO normal-hour pool to compare against:")
  print(empty_normal_pools)
} else {
  message("\nEvery stratum with a stress hour has at least one normal-hour observation ",
          "(though not necessarily enough for 1:1 matching — see above).")
}

# --- 5. Named extreme-demand events (2025) -------------------------------------
# Seeded from verified reporting, not assumed. These are candidates for
# later case-study cross-referencing against the pulled US48 series — NOT
# automatically treated as "the" stress periods; the quantile-based
# is_stress flag above is the primary, data-driven definition. Verify each
# event's dates actually show elevated US48 demand in demand_stress_tagged
# before using it as a case study in 08.
#
# Sources:
#   - Summer heat dome: Enel North America
#     (https://www.enelnorthamerica.com/insights/blogs/demand-response-june-2025-heatwave);
#     EIA Today in Energy, June 23 2025 PJM peak
#     (https://www.eia.gov/todayinenergy/detail.php?id=65604);
#     E&E News on MISO's June 30 decade-high and NYISO's July 2 peak
#     (https://www.eenews.net/articles/extreme-heat-pushed-electricity-demand-to-near-record-levels-2/)
#   - Winter arctic blast: Reuters, Jan 22 2025 PJM preliminary winter
#     record and TVA all-time peak
#     (https://www-web.itiger.com/news/2505703688)

named_events <- tribble(
  ~event_name,                  ~start_date,  ~end_date,    ~event_type, ~notes,
  "June 2025 Eastern heat dome", "2025-06-20", "2025-06-25", "summer_heat",
  "PJM peak 160,526-160,560 MW on June 23, highest since 2011",
  "Late June/early July 2025 heat wave (second wave)", "2025-06-28", "2025-07-04", "summer_heat",
  "MISO decade-high demand June 30; NYISO peak July 2; DOE emergency order for PJM June 30-July 3",
  "January 2025 arctic blast", "2025-01-20", "2025-01-22", "winter_cold",
  "PJM preliminary winter demand record (~145,000 MW) and TVA all-time peak (35,319 MW), Jan 22"
) |>
  mutate(start_date = as.Date(start_date), end_date = as.Date(end_date))

write_parquet(named_events, here("data", "named_events.parquet"))

message("\n--- Named events (seeded, verify against demand_stress_tagged before use) ---")
print(named_events, width = Inf)

# Quick cross-check: does each named event actually overlap the stress
# window as defined by the quantile threshold, in the ACTUAL pulled data?
named_event_stress_overlap <- named_events |>
  rowwise() |>
  mutate(
    hours_in_window = sum(as.Date(demand_stress_tagged$period) >= start_date &
                            as.Date(demand_stress_tagged$period) <= end_date),
    stress_hours_in_window = sum(as.Date(demand_stress_tagged$period) >= start_date &
                                   as.Date(demand_stress_tagged$period) <= end_date &
                                   demand_stress_tagged$is_stress),
    stress_share_pct = round(100 * stress_hours_in_window / hours_in_window, 1)
  ) |>
  ungroup()

message("\n--- Named-event / stress-window overlap check ---")
named_event_stress_overlap |>
  select(event_name, hours_in_window, stress_hours_in_window, stress_share_pct) |>
  print(width = Inf)

# IMPORTANT: the annual top-5% stress threshold is strongly summer-dominated
# (438 stress hours are overwhelmingly clustered in summer evening strata —
# see season_hour_strata_summary above). The January 2025 arctic blast is a
# well-documented, real winter grid-stress event and remains valid as a
# named-event case study, but only a fraction of its 72 hours meet the
# NATIONAL annual top-5% threshold (see stress_share_pct above). Do not
# describe its full window as "top-5% system stress" — that specific claim
# is only true for the subset of hours flagged is_stress == TRUE. A winter
# event can be a legitimate grid-stress case study on other grounds (PJM's
# own records, TVA's all-time peak) without meeting the US48-wide annual
# percentile threshold used for the primary, data-driven definition.

