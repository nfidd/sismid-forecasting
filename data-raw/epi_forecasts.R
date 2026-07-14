# Generate the epidemiologically-motivated forecasting backtest data.
#
# This reproduces exactly the backtest from
# sessions/epi-motivated-forecasting.qmd, but runs the expensive MCMC once
# here and saves the results as package data so the teaching session can
# load them instead of refitting 60 EpiNow2 models at render time. Running
# the fits here also makes the ARIMA baseline deterministic.
#
# Produces two package data objects:
#   epi_forecasts - sample-level backtest predictions, equalised to 200
#                   samples per forecast unit
#   epi_reported  - the full 170-day simulated reported-case series
#
# Runtime is a few minutes because of the MCMC.

library("nfidd.forecasting")
library("EpiNow2")
library("fable")
library("dplyr")
library("tidyr")
library("tibble")
library("purrr")
library("tsibble")

set.seed(7321)

# Fourth-root transformation used for the ARIMA baseline, matching earlier
# sessions.
fourth_root <- function(x) x^0.25
inv_fourth_root <- function(x) x^4
my_fourth_root <- new_transformation(fourth_root, inv_fourth_root)

# Shared inference configuration for every EpiNow2 fit.
stan_config <- stan_opts(
  method = "sampling", backend = "rstan",
  chains = 2, warmup = 300, samples = 1000
)

# Simulate a single outbreak to forecast.
gen_time <- fix_parameters(example_generation_time)
delays <- fix_parameters(example_incubation_period + example_reporting_delay)
pop_size <- 100000
dates <- seq.Date(as.Date("2023-01-01"), by = "day", length.out = 170)
R <- data.frame(date = dates, R = 1.25)

sim <- simulate_infections(
  R = R,
  initial_infections = 20,
  generation_time = gt_opts(gen_time),
  delays = delay_opts(delays),
  obs = obs_opts(family = "negbin", dispersion = Fixed(0.1)),
  pop = Fixed(pop_size),
  pop_period = "all"
)

reported <- sim |>
  as_tibble() |>
  filter(variable == "reported_cases") |>
  transmute(date, confirm = value)

# The full simulated series, saved for the session to reuse.
epi_reported <- reported

# EpiNow2 fitting uses the exported package helper fit_epinow2(), which
# applies the same generation time, delays, negative binomial observations,
# no day-of-week effect, and 14-day horizon used to simulate the data.

# ARIMA baseline helper returning sample-level predictions.
forecast_arima_samples <- function(train, horizon = 14, times = 1000) {
  fit <- train |>
    as_tsibble(index = date) |>
    model(ar2 = ARIMA(my_fourth_root(confirm)))
  generate(fit, h = horizon, times = times) |>
    as_tibble() |>
    transmute(
      forecast_date = max(train$date),
      date,
      horizon = as.integer(date - max(train$date)),
      sample = as.integer(as.factor(.rep)),
      predicted = pmax(.sim, 0),
      model = "arima"
    )
}

# The four EpiNow2 models compared in the backtest.
epi_models <- list(
  rw_nopop   = rt_opts(rw = 7),
  const_pop  = rt_opts(
    pop = Fixed(pop_size), future = "latest", pop_period = "all"
  ),
  rw_pop     = rt_opts(
    rw = 7, pop = Fixed(pop_size), future = "latest", pop_period = "all"
  ),
  rw_pop_est = rt_opts(
    rw = 7, pop = Normal(mean = pop_size, sd = pop_size / 2),
    future = "latest", pop_period = "all"
  )
)

# Twelve weekly forecast dates spanning growth, peak, and decline.
forecast_dates <- seq.Date(
  from = as.Date("2023-01-18"), to = as.Date("2023-04-05"), by = "week"
)

# Fit all four EpiNow2 models plus ARIMA at a single forecast date.
backtest_one <- function(fdate) {
  train <- reported |> filter(date <= fdate)
  en2 <- map_dfr(names(epi_models), function(m) {
    fit <- fit_epinow2(epi_models[[m]], train, stan = stan_config)
    get_predictions(fit, format = "sample") |>
      as_tibble() |>
      filter(horizon > 0) |>
      transmute(
        forecast_date = fdate, date, horizon,
        sample = as.integer(sample), predicted, model = m
      )
  })
  arm <- forecast_arima_samples(train)
  bind_rows(en2, arm)
}

all_forecasts <- map_dfr(forecast_dates, backtest_one) |>
  filter(horizon > 0, !is.na(predicted))

# Equalise to 200 samples per forecast unit so scoringutils does not warn
# about uneven sample counts, and renumber samples 1..200.
target_n <- 200L

unit_counts <- all_forecasts |>
  count(model, forecast_date, date, horizon)
stopifnot(min(unit_counts$n) >= target_n)

epi_forecasts <- all_forecasts |>
  group_by(model, forecast_date, date, horizon) |>
  slice_sample(n = target_n) |>
  mutate(sample = row_number()) |>
  ungroup() |>
  transmute(
    model,
    forecast_date,
    date,
    horizon = as.integer(horizon),
    sample = as.integer(sample),
    predicted
  )

usethis::use_data(epi_forecasts, overwrite = TRUE)
usethis::use_data(epi_reported, overwrite = TRUE)
