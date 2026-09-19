# EIA demand functions (v2 — multi-respondent) -------------------------------
# Supersedes the single-respondent version: `respondent` now accepts a
# character vector. httr2's .multi = "explode" repeats the facet[] query key
# once per value, which is EIA API v2's convention for multi-value facets —
# this pulls demand for many nodes (e.g. all 83 BA + region codes) in one
# paginated request stream instead of one call per node.

eia_demand_page <- function(
    start,
    end,
    respondent = "US48",
    offset = 0,
    page_size = 5000,
    api_key = Sys.getenv("EIA_API_KEY")
) {
  
  if (!nzchar(api_key)) {
    stop("EIA_API_KEY was not found.", call. = FALSE)
  }
  
  endpoint <- paste0(
    "https://api.eia.gov/v2/",
    "electricity/rto/region-data/data/"
  )
  
  httr2::request(endpoint) |>
    httr2::req_url_query(
      api_key = api_key,
      frequency = "hourly",
      `data[0]` = "value",
      `facets[type][]` = "D",
      `facets[respondent][]` = respondent,
      start = start,
      end = end,
      offset = offset,
      length = page_size,
      `sort[0][column]` = "period",
      `sort[0][direction]` = "asc",
      .multi = "explode"
    ) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE)
}


pull_eia_demand <- function(
    start,
    end,
    respondent = "US48",
    page_size = 5000
) {
  
  first_page <- eia_demand_page(
    start = start,
    end = end,
    respondent = respondent,
    offset = 0,
    page_size = page_size
  )
  
  total_rows <- as.integer(first_page$response$total)
  
  if (total_rows == 0) {
    return(tibble::tibble())
  }
  
  pages <- list(
    tibble::as_tibble(first_page$response$data)
  )
  
  if (total_rows > page_size) {
    
    remaining_offsets <- seq(
      from = page_size,
      to = total_rows - 1,
      by = page_size
    )
    
    remaining_pages <- purrr::map(
      remaining_offsets,
      \(page_offset) {
        eia_demand_page(
          start = start,
          end = end,
          respondent = respondent,
          offset = page_offset,
          page_size = page_size
        )$response$data |>
          tibble::as_tibble()
      }
    )
    
    pages <- c(pages, remaining_pages)
  }
  
  result <- dplyr::bind_rows(pages) |>
    janitor::clean_names() |>
    dplyr::mutate(
      period = lubridate::ymd_h(period),
      value = as.numeric(value)
    )
  
  if (nrow(result) != total_rows) {
    stop(
      "Pagination failure: expected ",
      total_rows,
      " rows but retrieved ",
      nrow(result),
      ".",
      call. = FALSE
    )
  }
  
  result
}