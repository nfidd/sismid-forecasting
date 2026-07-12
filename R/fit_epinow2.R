#' Fit a renewal model with EpiNow2 using the course defaults
#'
#' Wraps [EpiNow2::estimate_infections()] with the settings used throughout
#' the course: the example generation time and reporting delays, negative
#' binomial observations with no day-of-week effect, no Gaussian process, and
#' a 14-day forecast horizon. Only the reproduction number model (`rt`) and the
#' data need to be supplied for a standard fit.
#'
#' @param rt An `rt_opts` object describing the reproduction number model, as
#'   returned by [EpiNow2::rt_opts()].
#' @param data A data frame of observations with `date` and `confirm` columns,
#'   as expected by [EpiNow2::estimate_infections()].
#' @param generation_time A fixed generation time distribution. Defaults to the
#'   example generation time supplied with EpiNow2.
#' @param delays Fixed reporting delays. Defaults to the sum of the example
#'   incubation period and reporting delay supplied with EpiNow2.
#' @param horizon Integer number of days to forecast ahead. Defaults to 14.
#' @param stan Stan sampler settings, as returned by [EpiNow2::stan_opts()].
#'   Defaults to two chains of MCMC with the rstan backend.
#'
#' @return An `EpiNow2` `estimate_infections` object.
#'
#' @examples
#' \dontrun{
#' library(nfidd.forecasting)
#' data <- data.frame(
#'   date = seq(as.Date("2023-01-01"), by = "day", length.out = 60),
#'   confirm = rpois(60, 100)
#' )
#' fit <- fit_epinow2(EpiNow2::rt_opts(rw = 7), data)
#' }
#'
#' @export
fit_epinow2 <- function(rt, data,
  generation_time = EpiNow2::fix_parameters(
    EpiNow2::example_generation_time
  ),
  delays = EpiNow2::fix_parameters(
    EpiNow2::example_incubation_period + EpiNow2::example_reporting_delay
  ),
  horizon = 14,
  stan = EpiNow2::stan_opts(
    method = "sampling", backend = "rstan",
    chains = 2, warmup = 300, samples = 1000
  )
) {
  EpiNow2::estimate_infections(
    data,
    generation_time = EpiNow2::gt_opts(generation_time),
    delays = EpiNow2::delay_opts(delays),
    obs = EpiNow2::obs_opts(family = "negbin", week_effect = FALSE),
    rt = rt, gp = NULL,
    forecast = EpiNow2::forecast_opts(horizon = horizon),
    stan = stan
  )
}
