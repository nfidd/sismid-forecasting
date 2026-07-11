## Pre-computed CRPS scores for the advanced forecast evaluation session.
##
## Backtests three models (random walk, ARIMA(2,1,0), ARIMA(2,0,0) + Fourier)
## for three HHS regions over eight forecast dates in the 2018/19 season, using
## the same fourth-root transform and generate() workflow as the hub playground
## session, and scores each forecast unit with the CRPS. The session loads the
## result with data(advanced_scores) so it can open on the modelling idea rather
## than re-running time-series cross-validation.

library("nfidd.forecasting")
library("dplyr")
library("tsibble")
library("fable")
library("scoringutils")

set.seed(406) # for Ted Williams

fourth_root <- function(x) x^0.25
inv_fourth_root <- function(x) x^4
my_fourth_root <- new_transformation(fourth_root, inv_fourth_root)

data(flu_data_hhs)

sel_locations <- c("HHS Region 1", "HHS Region 2", "HHS Region 3")

flu_sub <- flu_data_hhs |> filter(location %in% sel_locations)

init_n <- flu_sub |>
  filter(location == "HHS Region 1", origin_date <= as.Date("2018-12-01")) |>
  nrow()

flu_tscv <- flu_sub |>
  filter(origin_date <= as.Date("2019-03-16")) |>
  tsibble::stretch_tsibble(.init = init_n, .step = 2, .id = ".split")

cv_samples <- flu_tscv |>
  model(
    rw       = RW(my_fourth_root(wili)),
    arima210 = ARIMA(my_fourth_root(wili) ~ pdq(2, 1, 0)),
    fourier  = ARIMA(my_fourth_root(wili) ~ pdq(2, 0, 0) + fourier(period = "year", K = 3))
  ) |>
  generate(h = 4, times = 500, bootstrap = TRUE) |>
  group_by(.split, .model, location, .rep) |>
  mutate(horizon = row_number()) |>
  ungroup() |>
  as_tibble()

truth <- flu_data_hhs |> as_tibble() |>
  filter(location %in% sel_locations) |>
  select(location, target_end_date = origin_date, observed = wili)

fc_samples <- cv_samples |>
  rename(target_end_date = origin_date, predicted = .sim, model = .model) |>
  left_join(truth, by = c("location", "target_end_date")) |>
  mutate(origin_date = target_end_date - horizon * 7L) |>
  filter(!is.na(observed))

advanced_scores <- fc_samples |>
  as_forecast_sample(
    forecast_unit = c("model", "location", "horizon", "origin_date"),
    observed = "observed", predicted = "predicted", sample_id = ".rep"
  ) |>
  score(metrics = list(crps = crps_sample)) |>
  as_tibble() |>
  select(model, location, horizon, origin_date, crps)

usethis::use_data(advanced_scores, overwrite = TRUE, compress = "xz")
