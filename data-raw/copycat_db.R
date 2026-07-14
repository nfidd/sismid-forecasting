#' Build the copycat historical trajectory database
#'
#' Downloads historical ILINet data (US National, the 10 HHS regions, and the
#' states + DC) from the Delphi Epidata API via epidatr for every influenza
#' season BEFORE the 2015/2016 season, and converts each (location, season)
#' trajectory into the scale-free representation the copycat method matches on:
#' a GAM-smoothed log weekly growth rate (`pred`) and its standard error
#' (`pred_se`), indexed by week-of-season.
#'
#' The method (after Fox / epiENGAGE `sjfox/copycat`) never standardises the
#' magnitude of a trajectory -- it works entirely in log weekly-change space,
#' `log((value[t+1] + 1) / (value[t] + 1))`, so trajectories from locations on
#' very different scales are directly comparable, and forecasts are re-anchored
#' to the current series' most recent value.
#'
#' Simplification vs. the original: the original `value` is wILI x %positive
#' ("ILI+"), which needs extra NREVSS lab data; here `value` is wILI directly
#' (falling back to unweighted %ILI for states, which have no weighted value).
#' The method only uses growth shape, so this is a minor change.

library("epidatr")
library("dplyr")
library("tidyr")
library("lubridate")
library("mgcv")

options(timeout = 600)

locs <- c("nat", paste0("hhs", 1:10), tolower(state.abb), "dc")

raw <- pub_fluview(regions = locs, epiweeks = epirange(199740, 201539))

flu <- raw |>
  transmute(location = region,
            epiweek = as.Date(epiweek),
            value = dplyr::coalesce(wili, ili)) |>
  filter(!is.na(value)) |>
  mutate(mmwr_year = lubridate::epiyear(epiweek),
         mmwr_week = lubridate::epiweek(epiweek),
         resp_season = ifelse(mmwr_week >= 40, mmwr_year, mmwr_year - 1)) |>
  filter(resp_season <= 2014) |>            # strictly before the 2015/2016 season
  group_by(location, resp_season) |>
  arrange(epiweek, .by_group = TRUE) |>
  mutate(resp_season_week = row_number()) |>
  ungroup()

## GAM-smoothed log weekly growth rate for one (location, season) trajectory,
## padding the ends so the smoother behaves at the season boundaries.
get_spline <- function(season_weeks, value) {
  padding <- 4
  nv <- c(rep(head(value, 1), padding), value, rep(tail(value, 1), padding))
  nw <- c(rev(min(season_weeks) - seq_len(padding)),
          season_weeks,
          max(season_weeks) + seq_len(padding))
  wc <- dplyr::lead(nv + 1) / (nv + 1)
  wc <- ifelse(is.na(wc), 1, wc)
  mod <- mgcv::gam(log(wc) ~ s(nw, k = 13))
  pr <- stats::predict(mod, se.fit = TRUE)
  tibble::tibble(resp_season_week = nw,
                 pred = as.numeric(pr$fit),
                 pred_se = as.numeric(pr$se.fit)) |>
    dplyr::filter(resp_season_week %in% season_weeks)
}

copycat_db <- flu |>
  group_by(location, resp_season) |>
  filter(n() >= 30) |>                       # only reasonably complete seasons
  arrange(epiweek, .by_group = TRUE) |>
  reframe(get_spline(resp_season_week, value)) |>
  ungroup()

usethis::use_data(copycat_db, overwrite = TRUE)

message("copycat_db: ", nrow(copycat_db), " rows | ",
        dplyr::n_distinct(paste(copycat_db$location, copycat_db$resp_season)),
        " trajectories | ", dplyr::n_distinct(copycat_db$location), " locations | seasons ",
        min(copycat_db$resp_season), "-", max(copycat_db$resp_season))
