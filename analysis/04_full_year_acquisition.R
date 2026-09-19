# =============================================================================
# analysis/04_full_year_acquisition.R
# =============================================================================
#
#
# SCOPE NOTE -----------------------------------------------------------------
# This script is acquisition-only. It retrieves and caches twelve monthly
# 2025 interchange datasets as Parquet files and validates row counts.
#
# It does NOT reconcile reciprocal reports, classify chronic connections,
# compute rankings, or produce any editorial finding. Those steps belong in
# later scripts (05_reconciliation_full_year.R, etc.) that operate on the
# cached files this script produces.
#
# Depends on:
#   R/eia_api.R  — must define a function that pulls interchange data for a
#                  given date range (referred to below as pull_eia_interchange;
#                  update the call if your actual function name/args differ)
#
# Outputs:
#   data-raw/interchange_2025-MM.parquet  — one file per month
#   output/acquisition_validation.csv     — expected vs. actual row counts
# =============================================================================

library(dplyr)
library(lubridate)
library(arrow)
library(here)

source(here("R", "eia_api.R"))

dir.create(here("data-raw"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("output"),   recursive = TRUE, showWarnings = FALSE)

# --- 1. Define the 12 monthly windows for 2025 ------------------------------

month_windows <- tibble(
  month_start = seq(as.Date("2025-01-01"), as.Date("2025-12-01"), by = "month")
) |>
  mutate(
    month_end = ceiling_date(month_start, "month") - days(1),
    month_label = format(month_start, "%Y-%m"),
    cache_path = here("data-raw", paste0("interchange_", month_label, ".parquet")),
    expected_hours = as.integer(difftime(
      ceiling_date(month_start, "month"), month_start, units = "hours"
    ))
  )

# TEST-ONE-MONTH TOGGLE -------------------------------------------------------
# Uncomment to run January only before committing to the full year. Confirm:
#   - the pull completes without throttling (~50 paginated requests)
#   - the Parquet file is written
#   - distinct_hours == 744, duplicate_rows == 0
#   - the cached file reloads correctly on a second run
#
# month_windows <- month_windows |> filter(month_label == "2025-01")


# --- 2. Pull-or-load: skip months already cached on disk --------------------

pull_or_load_month <- function(month_label, month_start, month_end, cache_path) {
  if (file.exists(cache_path)) {
    message(month_label, ": cache found, loading from disk")
    return(read_parquet(cache_path))
  }
  
  message(month_label, ": no cache found, pulling from EIA API")
  
  start_str <- format(month_start, "%Y-%m-%dT00")
  end_str   <- format(month_end,   "%Y-%m-%dT23")
  
  data <- tryCatch(
    pull_eia_interchange(start = start_str, end = end_str),
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

# --- 3. Row-count validation -------------------------------------------------
# Sanity checks per month:
#   - was any data returned at all?
#   - how many distinct hours does it cover vs. the expected hours in that
#     month? (a full month should have one row-set per hour, per pair)
#   - how many distinct BA-pairs appear?
#   - are there any duplicate (period, fromba, toba) rows?

validate_month <- function(month_label, expected_hours, data) {
  if (is.null(data)) {
    return(tibble(
      month = month_label,
      status = "MISSING",
      rows = NA_integer_,
      distinct_hours = NA_integer_,
      expected_hours = expected_hours,
      hours_coverage_pct = NA_real_,
      distinct_pairs = NA_integer_,
      duplicate_rows = NA_integer_
    ))
  }
  
  distinct_hours <- n_distinct(data$period)
  duplicate_rows <- nrow(data) - n_distinct(data$period, data$fromba, data$toba)
  
  tibble(
    month = month_label,
    status = case_when(
      distinct_hours != expected_hours ~ "PARTIAL",
      duplicate_rows > 0 ~ "DUPLICATES",
      TRUE ~ "COMPLETE"
    ),
    rows = nrow(data),
    distinct_hours = distinct_hours,
    expected_hours = expected_hours,
    hours_coverage_pct = round(100 * distinct_hours / expected_hours, 1),
    distinct_pairs = n_distinct(data$fromba, data$toba),
    duplicate_rows = duplicate_rows
  )
}

# --- 4. Run acquisition for all months, validate-and-release ----------------
# Each month is pulled/loaded, validated, and then dropped from memory before
# the next month starts. Only the validation ROW is retained per month — not
# the underlying data — so this stays lightweight regardless of how many
# months run in one session. Each month is independent, so a single failure
# doesn't block the rest; failures surface as MISSING/PARTIAL in the summary.

validation_rows <- vector("list", nrow(month_windows))

for (i in seq_len(nrow(month_windows))) {
  w <- month_windows[i, ]
  
  month_data <- pull_or_load_month(
    month_label = w$month_label,
    month_start = w$month_start,
    month_end   = w$month_end,
    cache_path  = w$cache_path
  )
  
  validation_rows[[i]] <- validate_month(w$month_label, w$expected_hours, month_data)
  
  rm(month_data)
  gc(verbose = FALSE)
}

validation_summary <- bind_rows(validation_rows)

write.csv(
  validation_summary,
  here("output", "acquisition_validation.csv"),
  row.names = FALSE
)

# --- 5. Report -----------------------------------------------------------------

validation_summary |> print(n = Inf)

incomplete_months <- validation_summary |>
  filter(status != "COMPLETE")

if (nrow(incomplete_months) > 0) {
  message(
    "\n", nrow(incomplete_months),
    " month(s) incomplete or missing — review before proceeding to reconciliation:"
  )
  print(incomplete_months |> select(month, status, hours_coverage_pct, duplicate_rows))
} else {
  message("\nAll ", nrow(month_windows),
          " selected month(s) acquired and pass basic validation.")
}

