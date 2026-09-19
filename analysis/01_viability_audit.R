# EIA interchange-data viability audit ------------------------------------
#
# Purpose:
#   1. Validate API extraction and hourly structure.
#   2. Measure coverage using unordered physical BA connections.
#   3. Measure the availability and agreement of reciprocal reports.
#
# This script does not reconcile conflicting reports.


# Packages ----------------------------------------------------------------

library(tidyverse)
library(here)

source(here("R", "eia_api.R"))


# Directories -------------------------------------------------------------

fs::dir_create(c(
  here("data-raw"),
  here("output")
))


# Audit samples -----------------------------------------------------------

audit_samples <- tribble(
  ~sample_id,    ~start,             ~end,               ~file_name,
  "2025-07-01",  "2025-07-01T00",    "2025-07-01T23",    "interchange_2025-07-01.parquet",
  "2025-07-15",  "2025-07-15T00",    "2025-07-15T23",    "interchange_2025-07-15.parquet"
)


# Retrieve or read cached data --------------------------------------------

load_interchange_sample <- function(
    sample_id,
    start,
    end,
    file_name
) {
  
  sample_path <- here("data-raw", file_name)
  
  if (file.exists(sample_path)) {
    
    interchange <- arrow::read_parquet(sample_path)
    
  } else {
    
    interchange <- pull_eia_interchange(
      start = start,
      end = end
    )
  }
  
  interchange <- interchange |>
    janitor::clean_names() |>
    mutate(
      period = as.POSIXct(period, tz = "UTC"),
      value = as.numeric(value)
    )
  
  # Keep cached files standardized.
  arrow::write_parquet(
    interchange,
    sample_path
  )
  
  interchange
}


# Viability-audit function ------------------------------------------------

audit_interchange_sample <- function(
    interchange,
    sample_label
) {
  
  expected_hour_count <- n_distinct(interchange$period)
  
  canonical <- interchange |>
    mutate(
      ba_1 = pmin(fromba, toba),
      ba_2 = pmax(fromba, toba)
    )
  
  # 1. Structural integrity ----------------------------------------------
  
  structural <- canonical |>
    summarise(
      sample_id = sample_label,
      rows = n(),
      hours = n_distinct(period),
      stored_directed_pairs = n_distinct(fromba, toba),
      physical_connections = n_distinct(ba_1, ba_2),
      physical_pair_hours = n_distinct(period, ba_1, ba_2),
      duplicate_directed_rows =
        n() - n_distinct(period, fromba, toba),
      reciprocal_pair_hours =
        n() - n_distinct(period, ba_1, ba_2),
      missing_values = sum(is.na(value)),
      negative_values = sum(value < 0, na.rm = TRUE),
      positive_values = sum(value > 0, na.rm = TRUE),
      zero_values = sum(value == 0, na.rm = TRUE)
    )
  
  # 2. Canonical physical-connection coverage ----------------------------
  
  connection_coverage <- canonical |>
    distinct(period, ba_1, ba_2) |>
    count(
      ba_1,
      ba_2,
      name = "hours_reported"
    ) |>
    mutate(
      sample_id = sample_label,
      expected_hours = expected_hour_count,
      complete = hours_reported == expected_hour_count,
      .before = 1
    )
  
  coverage_summary <- connection_coverage |>
    summarise(
      sample_id = sample_label,
      observed_connections = n(),
      complete_connections = sum(complete),
      incomplete_connections = sum(!complete),
      coverage_rate =
        sum(hours_reported) /
        (n() * expected_hour_count),
      minimum_hours = min(hours_reported),
      median_hours = median(hours_reported),
      maximum_hours = max(hours_reported)
    )
  
  incomplete_connections <- connection_coverage |>
    filter(!complete)
  
  # 3. Correct reciprocal-report audit -----------------------------------
  
  reverse_reports <- interchange |>
    transmute(
      period,
      lookup_from = toba,
      lookup_to = fromba,
      reciprocal_value = value
    )
  
  reciprocal_detail <- interchange |>
    left_join(
      reverse_reports,
      by = c(
        "period",
        "fromba" = "lookup_from",
        "toba" = "lookup_to"
      )
    ) |>
    mutate(
      sample_id = sample_label,
      reciprocal_present = !is.na(reciprocal_value),
      
      # Opposite perspectives should approximately cancel.
      reciprocal_gap = value + reciprocal_value,
      absolute_gap = abs(reciprocal_gap),
      
      relative_gap = absolute_gap /
        pmax(
          abs(value),
          abs(reciprocal_value),
          1
        )
    )
  
  reciprocal_summary <- reciprocal_detail |>
    summarise(
      sample_id = sample_label,
      observations = n(),
      reciprocal_coverage = mean(reciprocal_present),
      
      median_absolute_gap = median(
        absolute_gap[reciprocal_present],
        na.rm = TRUE
      ),
      
      p95_absolute_gap = quantile(
        absolute_gap[reciprocal_present],
        probs = 0.95,
        na.rm = TRUE,
        names = FALSE
      ),
      
      within_5_percent = mean(
        relative_gap[reciprocal_present] <= 0.05,
        na.rm = TRUE
      )
    )
  
  list(
    structural = structural,
    coverage_summary = coverage_summary,
    incomplete_connections = incomplete_connections,
    reciprocal_summary = reciprocal_summary
  )
}


# Run audits --------------------------------------------------------------

audit_results <- audit_samples |>
  mutate(
    interchange = pmap(
      list(sample_id, start, end, file_name),
      load_interchange_sample
    ),
    
    audit = map2(
      interchange,
      sample_id,
      audit_interchange_sample
    )
  )


# Combine results ---------------------------------------------------------

structural_audit <- audit_results$audit |>
  map_dfr("structural")

canonical_coverage_summary <- audit_results$audit |>
  map_dfr("coverage_summary")

incomplete_connections <- audit_results$audit |>
  map_dfr("incomplete_connections")

reciprocal_summary <- audit_results$audit |>
  map_dfr("reciprocal_summary")


# Save results ------------------------------------------------------------

write_csv(
  structural_audit,
  here("output", "structural_audit.csv")
)

write_csv(
  canonical_coverage_summary,
  here("output", "canonical_coverage_summary.csv")
)

write_csv(
  incomplete_connections,
  here("output", "incomplete_connections.csv")
)

write_csv(
  reciprocal_summary,
  here("output", "reciprocal_audit.csv")
)


# Console output ----------------------------------------------------------

list(
  structural = structural_audit,
  coverage = canonical_coverage_summary,
  incomplete = incomplete_connections,
  reciprocal = reciprocal_summary
)