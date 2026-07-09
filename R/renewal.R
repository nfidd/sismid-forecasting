#' A semi-mechanistic renewal-equation forecast model for fable
#'
#' RENEWAL defines a semi-mechanistic forecasting model for use inside a
#' fabletools model() call, like NAIVE() or ARIMA(). It estimates the
#' time-varying reproduction number from the data using the renewal equation
#' (via EpiEstim), then simulates incidence forward: the reproduction number
#' takes a random walk on the log scale and observations are drawn from a
#' negative-binomial distribution. The model expects case counts
#' (non-negative integers), not rates or percentages.
#'
#' @param formula A model formula. The response should be a column of case
#'   counts.
#' @param gi Numeric vector giving the generation-interval probability mass
#'   function, starting at interval 0.
#' @param window Integer width of the sliding window used to estimate the
#'   reproduction number.
#' @param sigma_rw Standard deviation of the Gaussian random walk on the log
#'   reproduction number used to project it into the future.
#' @param n_sim Number of simulated trajectories used to form the forecast
#'   distribution.
#' @param ... Further arguments passed to the training function.
#'
#' @return A fable model definition producing objects of class RENEWAL.
#' @importFrom rlang enquo
#' @export
RENEWAL <- function(formula, ...) {
  md <- fabletools::new_model_class(
    "RENEWAL",
    train = train_renewal,
    specials = fabletools::new_specials()
  )
  fabletools::new_model_definition(md, !!rlang::enquo(formula), ...)
}

# Internal training function called by fabletools::model().
train_renewal <- function(.data, specials, gi = c(0, 0.65, 0.30, 0.05),
                          window = 4L, sigma_rw = 0.1, n_sim = 2000, ...) {
  mv <- tsibble::measured_vars(.data)
  y <- .data[[mv]]
  n <- length(y)
  t_start <- 2:(n - window + 1)
  t_end <- t_start + (window - 1)
  rt <- suppressMessages(EpiEstim::estimate_R(
    y,
    method = "non_parametric_si",
    config = EpiEstim::make_config(list(
      si_distr = gi, t_start = t_start, t_end = t_end
    ))
  ))
  dat <- data.frame(y = y, t = seq_len(n))
  theta <- tryCatch(
    suppressWarnings(
      MASS::glm.nb(y ~ splines::ns(t, df = 10), data = dat)$theta
    ),
    error = function(e) 50
  )
  structure(
    list(
      y = y, gi = gi, rt = rt, theta = theta,
      sigma_rw = sigma_rw, n_sim = n_sim
    ),
    class = "RENEWAL"
  )
}

# Simulate one renewal trajectory of length h from a fitted RENEWAL object.
.renewal_path <- function(object, h) {
  w <- object$gi[-1]
  K <- length(w)
  r0 <- EpiEstim::sample_posterior_R(object$rt, 1)
  # reproduction-number random walk on the log scale
  rtj <- exp(log(r0) + cumsum(stats::rnorm(h, 0, object$sigma_rw)))
  series <- object$y
  out <- numeric(h)
  for (k in seq_len(h)) {
    foi <- sum(rev(utils::tail(series, K)) * w) # renewal force of infection
    out[k] <- stats::rnbinom(1, size = object$theta, mu = rtj[k] * foi)
    series <- c(series, out[k])
  }
  out
}

#' @importFrom fabletools forecast
#' @method forecast RENEWAL
#' @export
forecast.RENEWAL <- function(object, new_data, specials = NULL, ...) {
  h <- nrow(new_data)
  sims <- replicate(object$n_sim, .renewal_path(object, h)) # h x n_sim
  distributional::dist_sample(lapply(seq_len(h), function(k) sims[k, ]))
}

#' @importFrom fabletools generate
#' @method generate RENEWAL
#' @export
generate.RENEWAL <- function(x, new_data, specials = NULL, ...) {
  new_data |>
    dplyr::group_by(.data$.rep) |>
    dplyr::mutate(.sim = .renewal_path(x, dplyr::n())) |>
    dplyr::ungroup()
}

#' @importFrom stats fitted
#' @method fitted RENEWAL
#' @export
fitted.RENEWAL <- function(object, ...) {
  y <- object$y
  n <- length(y)
  w <- object$gi[-1]
  K <- length(w)
  lambda <- rep(NA_real_, n)
  for (t in seq_len(n)) if (t > K) lambda[t] <- sum(rev(y[(t - K):(t - 1)]) * w)
  out <- rep(NA_real_, n)
  R <- object$rt$R
  med <- R[["Median(R)"]]
  for (i in seq_along(R$t_end)) {
    tt <- R$t_end[i]
    if (!is.na(lambda[tt])) out[tt] <- med[i] * lambda[tt]
  }
  out
}

#' @importFrom stats residuals
#' @method residuals RENEWAL
#' @export
residuals.RENEWAL <- function(object, ...) object$y - fitted.RENEWAL(object)

#' @importFrom fabletools model_sum
#' @method model_sum RENEWAL
#' @export
model_sum.RENEWAL <- function(x) sprintf("RENEWAL(theta=%.0f)", x$theta)
