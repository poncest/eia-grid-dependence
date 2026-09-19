# analysis/02_reconciliation_audit.R

# EIA reciprocal-report reconciliation audit ------------------------------
#
# Purpose:
#   1. Normalize reports to one canonical BA-pair orientation.
#   2. Identify single versus reciprocal reports.
#   3. Measure reciprocal agreement and discrepancies.
#   4. Determine whether discrepancies are limited to small flows.
#
# Important:
#   The mean of reciprocal reports is a candidate estimate used for
#   diagnostic purposes. This script does not establish it as the final
#   production reconciliation rule.


# Packages ----------------------------------------------------------------

library(tidyverse)
library(here)


# Configuration -----------------------------------------------------------

sample_id <- "2025-07-15"

sample_path <- here(
  "data-raw",
  "interchange_2025-07-15.parquet"
)

agreement_threshold <- 0.05


# Load audited sample ------------------------------------------------------

if (!file.exists(sample_path)) {
  stop(
    "The July 15 sample was not found. ",
    "Run analysis/01_viability_audit.R first.",
    call. = FALSE
  )
}

interchange <- arrow::read_parquet(sample_path) |>
  janitor::clean_names() |>
  mutate(
    period = as.POSIXct(period, tz = "UTC"),
    value = as.numeric(value)
  )


# Normalize to a canonical BA-pair orientation ----------------------------
#
# ba_1 and ba_2 identify the physical connection.
#
# A positive canonical_value indicates:
#   ba_1 -> ba_2
#
# A negative canonical_value indicates:
#   ba_2 -> ba_1

canonical_reports <- interchange |>
  mutate(
    ba_1 = pmin(fromba, toba),
    ba_2 = pmax(fromba, toba),
    
    canonical_value = if_else(
      fromba == ba_1,
      value,
      -value
    )
  )


# Construct one candidate record per physical pair-hour -------------------

candidate_reconciliation <- canonical_reports |>
  group_by(period, ba_1, ba_2) |>
  summarise(
    reports = n(),
    
    minimum_report = min(
      canonical_value,
      na.rm = TRUE
    ),
    
    maximum_report = max(
      canonical_value,
      na.rm = TRUE
    ),
    
    candidate_signed_flow = mean(
      canonical_value,
      na.rm = TRUE
    ),
    
    report_spread =
      maximum_report - minimum_report,
    
    relative_spread =
      report_spread /
      pmax(
        max(abs(canonical_value), na.rm = TRUE),
        1
      ),
    
    direction_conflict =
      reports > 1 &&
      minimum_report < 0 &&
      maximum_report > 0,
    
    .groups = "drop"
  ) |>
  mutate(
    candidate_flow_mwh = abs(candidate_signed_flow),
    
    candidate_source_ba = if_else(
      candidate_signed_flow >= 0,
      ba_1,
      ba_2
    ),
    
    candidate_target_ba = if_else(
      candidate_signed_flow >= 0,
      ba_2,
      ba_1
    ),
    
    reporting_quality = case_when(
      reports == 1 ~ "single report",
      
      relative_spread <= agreement_threshold ~
        "reciprocal agreement",
      
      TRUE ~
        "reciprocal discrepancy"
    )
  )


# 1. Overall reconciliation summary ---------------------------------------

reconciliation_summary <- candidate_reconciliation |>
  summarise(
    sample_id = sample_id,
    pair_hours = n(),
    
    single_report_n =
      sum(reporting_quality == "single report"),
    
    reciprocal_agreement_n =
      sum(reporting_quality == "reciprocal agreement"),
    
    reciprocal_discrepancy_n =
      sum(reporting_quality == "reciprocal discrepancy"),
    
    single_report_pct =
      mean(reporting_quality == "single report"),
    
    reciprocal_agreement_pct =
      mean(reporting_quality == "reciprocal agreement"),
    
    reciprocal_discrepancy_pct =
      mean(reporting_quality == "reciprocal discrepancy"),
    
    direction_conflict_n =
      sum(direction_conflict),
    
    direction_conflict_pct =
      mean(direction_conflict)
  )


# 2. Discrepancy magnitude profile ----------------------------------------

discrepancy_profile <- candidate_reconciliation |>
  filter(
    reporting_quality == "reciprocal discrepancy"
  ) |>
  summarise(
    sample_id = sample_id,
    discrepancies = n(),
    
    median_flow_mwh =
      median(candidate_flow_mwh, na.rm = TRUE),
    
    pct_under_50_mwh =
      mean(candidate_flow_mwh < 50, na.rm = TRUE),
    
    pct_under_100_mwh =
      mean(candidate_flow_mwh < 100, na.rm = TRUE),
    
    median_report_spread =
      median(report_spread, na.rm = TRUE),
    
    p95_report_spread =
      quantile(
        report_spread,
        probs = 0.95,
        na.rm = TRUE,
        names = FALSE
      ),
    
    median_relative_spread =
      median(relative_spread, na.rm = TRUE),
    
    p95_relative_spread =
      quantile(
        relative_spread,
        probs = 0.95,
        na.rm = TRUE,
        names = FALSE
      )
  )


# 3. Identify connections driving discrepancies --------------------------

connection_discrepancies <- candidate_reconciliation |>
  group_by(ba_1, ba_2) |>
  summarise(
    pair_hours = n(),
    
    single_report_hours =
      sum(reporting_quality == "single report"),
    
    agreement_hours =
      sum(reporting_quality == "reciprocal agreement"),
    
    discrepancy_hours =
      sum(reporting_quality == "reciprocal discrepancy"),
    
    discrepancy_rate =
      discrepancy_hours / pair_hours,
    
    direction_conflict_hours =
      sum(direction_conflict),
    
    median_flow_mwh =
      median(candidate_flow_mwh, na.rm = TRUE),
    
    median_report_spread =
      median(
        report_spread[
          reporting_quality == "reciprocal discrepancy"
        ],
        na.rm = TRUE
      ),
    
    .groups = "drop"
  ) |>
  arrange(
    desc(discrepancy_hours),
    desc(discrepancy_rate)
  )


# 4. Preserve direction-conflict cases for inspection ---------------------

direction_conflicts <- candidate_reconciliation |>
  filter(direction_conflict) |>
  arrange(
    desc(report_spread)
  )


# 5. Largest reciprocal discrepancies ------------------------------------

largest_discrepancies <- candidate_reconciliation |>
  filter(
    reporting_quality == "reciprocal discrepancy"
  ) |>
  arrange(
    desc(report_spread)
  ) |>
  slice_head(n = 50)


# Save audit outputs ------------------------------------------------------

write_csv(
  reconciliation_summary,
  here("output", "reconciliation_summary.csv")
)

write_csv(
  discrepancy_profile,
  here("output", "discrepancy_profile.csv")
)

write_csv(
  connection_discrepancies,
  here("output", "connection_discrepancies.csv")
)

write_csv(
  direction_conflicts,
  here("output", "direction_conflicts.csv")
)

write_csv(
  largest_discrepancies,
  here("output", "largest_reciprocal_discrepancies.csv")
)


# Console output ----------------------------------------------------------

list(
  reconciliation = reconciliation_summary,
  discrepancy_profile = discrepancy_profile,
  
  leading_connections = connection_discrepancies |>
    slice_head(n = 10),
  
  direction_conflicts = direction_conflicts |>
    slice_head(n = 10),
  
  largest_discrepancies = largest_discrepancies |>
    slice_head(n = 10)
)