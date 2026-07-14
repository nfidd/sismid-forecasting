#' Download vintage (issue-versioned) FluView ILINet data
#'
#' Pulls the full backfill/issue history of weighted ILI (wILI) for the US
#' National level and the 10 HHS regions from the Delphi Epidata API, covering
#' the five forecastable seasons in the sandbox hub (2015/16-2019/20).
#'
#' We request, for each season, every *issue* (data version) in the range
#' EW40-EW30 (through early July, so the season's ~1-July "final" values are
#' captured), together with all *epiweeks* of observations back to the start of
#' the series (200335). This lets us reconstruct, for any forecast made
#' with last-observed week XX, exactly what the data looked like in real time:
#' the snapshot `issue == XX` (released the Friday of week XX+1, i.e. before the
#' historical FluSight deadline of Monday of week XX+2).
#'
#' The object is saved *as returned by epidatr* (row-bound across calls); all
#' downstream reshaping (tscv objects, versioned exploration object, hub
#' target-data) derives from this raw snapshot.

library("epidatr")
library("purrr")
library("dplyr")

options(timeout = 600)

locations <- c(
  "nat",
  "hhs1", "hhs2", "hhs3", "hhs4", "hhs5",
  "hhs6", "hhs7", "hhs8", "hhs9", "hhs10"
)

## Forecastable seasons, keyed by start year. For start year Y the origin weeks
## run EW42(Y)-EW18(Y+1); we pull issues over a slightly wider EW40-EW20 window
## so every origin's snapshot is present with a small margin.
start_years <- 2015:2019

season_ranges <- map(start_years, function(y) {
  list(
    season   = paste0(y, "/", y + 1),
    issues   = epirange(y * 100 + 40, (y + 1) * 100 + 30),
    epiweeks = epirange(200335, (y + 1) * 100 + 30)
  )
})

fetch_season <- function(rng) {
  message("Downloading season ", rng$season, " ...")
  locations |>
    map(function(loc) {
      pub_fluview(
        regions  = loc,
        epiweeks = rng$epiweeks,
        issues   = rng$issues
      )
    }) |>
    list_rbind()
}

flu_data_hhs_vintage_raw <- season_ranges |>
  map(fetch_season) |>
  list_rbind()

dir.create("data-raw", showWarnings = FALSE)
saveRDS(flu_data_hhs_vintage_raw, "data-raw/flu_data_hhs_vintage_raw.rds")

## ---- coverage report ------------------------------------------------------
message("\nSaved ", nrow(flu_data_hhs_vintage_raw), " rows to ",
        "data-raw/flu_data_hhs_vintage_raw.rds")

coverage <- flu_data_hhs_vintage_raw |>
  mutate(season = dplyr::case_when(
    issue >= as.Date("2015-09-01") & issue < as.Date("2016-09-01") ~ "2015/2016",
    issue >= as.Date("2016-09-01") & issue < as.Date("2017-09-01") ~ "2016/2017",
    issue >= as.Date("2017-09-01") & issue < as.Date("2018-09-01") ~ "2017/2018",
    issue >= as.Date("2018-09-01") & issue < as.Date("2019-09-01") ~ "2018/2019",
    issue >= as.Date("2019-09-01") & issue < as.Date("2020-09-01") ~ "2019/2020",
    TRUE ~ NA_character_
  )) |>
  group_by(season, region) |>
  summarise(n_issues = dplyr::n_distinct(issue),
            first_issue = min(issue), last_issue = max(issue),
            .groups = "drop")

print(coverage, n = Inf)
