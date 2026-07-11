#' Build vintage data objects from the raw issue-history download
#'
#' Consumes data-raw/flu_data_hhs_vintage_raw.rds (produced by
#' build_flu_vintage.R) and the finalized `flu_data_hhs` object, and creates:
#'
#'   * flu_data_hhs_tscv_season1 .. season5 : expanding-window time-series
#'     cross-validation tsibbles (one per forecastable season) whose wILI values
#'     reflect the data *as it was available in real time* at each forecast
#'     origin. For a split with last-observed week XX (origin_date D, the
#'     Saturday ending XX), the snapshot is `issue == XX` (the FluView release
#'     published the Friday of week XX+1, before the historical FluSight
#'     submission deadline of Monday of week XX+2). Weeks older than that
#'     release's rolling revision window were already finalized, so they are
#'     filled from `flu_data_hhs`.
#'
#'   * flu_data_hhs_versions : a long, versioned table (location, origin_date,
#'     as_of, wili) giving every reported version of each week's wILI, for
#'     in-session exploration of reporting backfill/revisions.
#'
#' Assumption: a week not republished in release XX equals its finalized value
#' (true for weeks outside FluView's ~1-year revision window). Deep-history
#' weeks are therefore finalized, which is standard for retrospective flu
#' forecasting.

library("dplyr")
library("purrr")
library("tsibble")

raw <- readRDS("data-raw/flu_data_hhs_vintage_raw.rds")
load("data/flu_data_hhs.rda") # finalized: location, origin_date, wili

reg2loc <- c(
  nat = "US National",
  hhs1 = "HHS Region 1", hhs2 = "HHS Region 2", hhs3 = "HHS Region 3",
  hhs4 = "HHS Region 4", hhs5 = "HHS Region 5", hhs6 = "HHS Region 6",
  hhs7 = "HHS Region 7", hhs8 = "HHS Region 8", hhs9 = "HHS Region 9",
  hhs10 = "HHS Region 10"
)

finalized <- flu_data_hhs |>
  tibble::as_tibble() |>
  mutate(origin_date = as.Date(origin_date))

## The "final" version of a week is the last one published by ~1 July of its
## season's end year (weeks in Aug-Dec belong to the season ending next year).
## Later cross-season re-baselines are excluded, so versioned data shows only
## within-season reporting revisions and ends at each season's finalized value.
season_end_year <- function(d) {
  d <- as.Date(d)
  ifelse(as.integer(format(d, "%m")) >= 8,
         as.integer(format(d, "%Y")) + 1L, as.integer(format(d, "%Y")))
}
final_as_of <- function(d) as.Date(sprintf("%d-07-01", season_end_year(d)))

## ---- versioned exploration object -----------------------------------------
## as_of = the actual publication date of that version (hubverse convention:
## "the date target data were reported"). origin_date = Saturday ending the
## observed epiweek, matching flu_data_hhs.
flu_data_hhs_versions <- raw |>
  transmute(
    location = unname(reg2loc[region]),
    origin_date = as.Date(epiweek) + 6,
    as_of = as.Date(release_date),
    wili = wili
  ) |>
  filter(as_of <= final_as_of(origin_date)) |>
  arrange(location, origin_date, as_of)

## Deep training history: weeks before the vintage era (pre-2015/16 season) were
## long settled by the time forecasting begins, so there is no meaningful
## within-window backfill to record for them. We carry them as a single baseline
## version -- their finalized value, i.e. the data a forecaster had at the start
## of the 2015 season -- so the full 2003-onward series is available for training
## while the forecastable-era weeks keep their revision histories.
baseline_as_of <- as.Date("2015-10-01")
deep_history <- finalized |>
  filter(origin_date < min(flu_data_hhs_versions$origin_date)) |>
  transmute(location, origin_date, as_of = baseline_as_of, wili)

flu_data_hhs_versions <- bind_rows(deep_history, flu_data_hhs_versions) |>
  arrange(location, origin_date, as_of)

## ---- vintage tscv builder -------------------------------------------------
## For each origin D (Saturday), snapshot = issue == (D - 6) recent weeks,
## coalesced over finalized deep history; tagged with an integer .split id.
build_tscv <- function(origins) {
  origins <- as.Date(origins)
  missing_issue <- setdiff(as.character(origins - 6),
                           as.character(unique(as.Date(raw$issue))))
  if (length(missing_issue)) {
    stop("No issue in raw for origin(s): ",
         paste(as.character(as.Date(missing_issue) + 6), collapse = ", "))
  }

  imap(origins, function(D, i) {
    vint <- raw |>
      filter(as.Date(issue) == D - 6) |>
      transmute(location = unname(reg2loc[region]),
                origin_date = as.Date(epiweek) + 6,
                wili_v = wili)
    finalized |>
      filter(origin_date <= D) |>
      left_join(vint, by = c("location", "origin_date")) |>
      mutate(wili = coalesce(wili_v, wili), .split = i) |>
      select(location, origin_date, wili, .split)
  }) |>
    list_rbind() |>
    as_tsibble(index = origin_date, key = c(location, .split))
}

season_origins <- list(
  seq(as.Date("2015-10-17"), as.Date("2016-05-07"), by = 7),
  seq(as.Date("2016-10-22"), as.Date("2017-05-06"), by = 7),
  seq(as.Date("2017-10-21"), as.Date("2018-05-05"), by = 7),
  seq(as.Date("2018-10-20"), as.Date("2019-05-04"), by = 7),
  seq(as.Date("2019-10-19"), as.Date("2020-05-02"), by = 7)
)

flu_data_hhs_tscv_season1 <- build_tscv(season_origins[[1]])
flu_data_hhs_tscv_season2 <- build_tscv(season_origins[[2]])
flu_data_hhs_tscv_season3 <- build_tscv(season_origins[[3]])
flu_data_hhs_tscv_season4 <- build_tscv(season_origins[[4]])
flu_data_hhs_tscv_season5 <- build_tscv(season_origins[[5]])

## ---- save shipped data objects --------------------------------------------
usethis::use_data(flu_data_hhs_versions, overwrite = TRUE)
usethis::use_data(flu_data_hhs_tscv_season1, overwrite = TRUE)
usethis::use_data(flu_data_hhs_tscv_season2, overwrite = TRUE)
usethis::use_data(flu_data_hhs_tscv_season3, overwrite = TRUE)
usethis::use_data(flu_data_hhs_tscv_season4, overwrite = TRUE)
usethis::use_data(flu_data_hhs_tscv_season5, overwrite = TRUE)

## ---- quick sanity report --------------------------------------------------
message("\nn origins per season: ",
        paste(lengths(season_origins), collapse = ", "))
message("season1 tscv: ", nrow(flu_data_hhs_tscv_season1), " rows, ",
        dplyr::n_distinct(flu_data_hhs_tscv_season1$.split), " splits")
message("versions object: ", nrow(flu_data_hhs_versions), " rows")

## Demonstrate backfill actually differs across splits for the origin week:
demo <- flu_data_hhs_tscv_season1 |>
  tibble::as_tibble() |>
  filter(location == "US National", origin_date == as.Date("2016-01-16")) |>
  arrange(.split) |>
  select(.split, origin_date, wili)
message("\nwILI for week ending 2016-01-16 as seen in successive splits ",
        "(backfill; NA before that week is in a split):")
print(utils::head(demo, 6))
