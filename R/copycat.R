#' A "method of analogues" (copycat) forecast model for fable
#'
#' COPYCAT defines a probabilistic analog forecasting model for use inside a
#' fabletools model() call, like NAIVE() or ARIMA(). It forecasts by matching
#' the recent growth pattern of the current season against a fixed library of
#' historical trajectories (the analog "database") and reusing the subsequent
#' growth of the closest matches, re-anchored to the most recent observed value.
#' It is a reimplementation of the copycat method of Fox / epiENGAGE
#' (\url{https://github.com/sjfox/copycat}).
#'
#' The method works entirely in **log weekly-growth space**, so it is
#' scale-free: the analog library can pool locations on very different scales,
#' and forecasts are re-anchored to the current series. It therefore operates
#' directly on a rate such as weighted ILI percentage (no counts required).
#'
#' The model needs a season-week-indexed library of smoothed log weekly growth
#' rates; the bundled [copycat_db] (US ILINet, all locations, seasons before
#' 2015/16) is used by default.
#'
#' @param formula The response to forecast, named on its own, e.g.
#'   `COPYCAT(wili)`. The formula structure is deliberately minimal: give the
#'   bare response column, on its natural scale (a rate such as weighted ILI
#'   percentage). Unlike `ARIMA()`, there are **no right-hand-side terms or
#'   specials** -- COPYCAT uses only the response series and the analogue
#'   library, so a formula like `COPYCAT(wili ~ ...)` gains nothing (anything
#'   after `~` is ignored). Do **not** wrap the response in a transformation
#'   (e.g. avoid `COPYCAT(log(wili))` or a fourth-root): the method already
#'   models growth on the log scale internally and re-anchors to the observed
#'   level, so an outer transformation is unnecessary and can distort the result.
#' @param db A trajectory library with columns `location`, `resp_season`,
#'   `resp_season_week`, `pred` (smoothed log weekly growth rate) and `pred_se`.
#'   If `NULL` (the default), the bundled [copycat_db] is used.
#' @param recent_weeks_touse Number of most-recent weekly changes used to match
#'   the current season against the library.
#' @param resp_week_range Allow analogs to match within +/- this many weeks of
#'   the current week-of-season (0 = exact week alignment).
#' @param top_matches Number of best-matching analogs to resample from.
#' @param error_exponentiation How strongly closer analogues are favoured when
#'   resampling. Each analogue has a *match distance* (how far its past growth is
#'   from the current season's recent growth; smaller = closer). Analogues are
#'   drawn with probability proportional to 1 / (match distance) raised to this
#'   power, so larger values concentrate the forecast on the very closest
#'   analogues (sharper, less spread).
#' @param min_allowed_weight The smallest match distance any analogue is treated
#'   as having. Because analogues are resampled more often the closer they are
#'   (see `error_exponentiation`), an almost-perfect match -- one whose distance
#'   is near zero -- would otherwise be picked nearly every time and collapse the
#'   forecast onto a single trajectory. Treating any distance below this value as
#'   equal to it caps how much the single best analogue can dominate, keeping the
#'   set of trajectories diverse. (In the code this distance is stored in a
#'   column named `weight`, where smaller means a better match.)
#' @param n_sim Number of simulated trajectories used to form the forecast
#'   distribution.
#' @param ... Further arguments passed to the training function.
#'
#' @return A fable model definition producing objects of class COPYCAT.
#'
#' @examples
#' \dontrun{
#' data(flu_data)
#' # fit and forecast alongside other fable models
#' flu_data |>
#'   dplyr::filter(epiweek <= as.Date("2017-12-30")) |>
#'   fabletools::model(copycat = COPYCAT(wili)) |>
#'   fabletools::forecast(h = 8)
#' }
#' @importFrom rlang enquo .data
#' @export
COPYCAT <- function(formula, db = NULL, recent_weeks_touse = 5L,
                    resp_week_range = 1L, top_matches = 100L,
                    error_exponentiation = 2, min_allowed_weight = 0.02,
                    n_sim = 1000L, ...) {
  md <- fabletools::new_model_class(
    "COPYCAT",
    train = train_copycat,
    specials = fabletools::new_specials()
  )
  fabletools::new_model_definition(
    md, !!rlang::enquo(formula),
    db = db, recent_weeks_touse = recent_weeks_touse,
    resp_week_range = resp_week_range, top_matches = top_matches,
    error_exponentiation = error_exponentiation,
    min_allowed_weight = min_allowed_weight, n_sim = n_sim, ...
  )
}

# Internal training function called by fabletools::model().
train_copycat <- function(.data, specials, db = NULL,
                          recent_weeks_touse = 5L, resp_week_range = 1L,
                          top_matches = 100L, error_exponentiation = 2,
                          min_allowed_weight = 0.02, n_sim = 1000L, ...) {
  if (is.null(db)) db <- get("copycat_db")
  mv <- tsibble::measured_vars(.data)
  y <- .data[[mv]]
  dates <- as.Date(.data[[tsibble::index_var(.data)]])

  # current partial season: growth features indexed by week-of-season
  yr <- lubridate::epiyear(dates)
  wk <- lubridate::epiweek(dates)
  season <- ifelse(wk >= 40, yr, yr - 1)
  keep <- season == max(season)
  ord <- order(dates[keep])
  cur_val <- y[keep][ord]
  curr <- data.frame(
    resp_season_week = seq_along(cur_val),
    value = cur_val,
    curr_weekly_change = log(dplyr::lead(cur_val + 1) / (cur_val + 1))
  )

  structure(
    list(
      y = y, n = length(y), curr = curr,
      most_recent_week = max(curr$resp_season_week),
      most_recent_value = utils::tail(cur_val, 1),
      db = db, recent_weeks_touse = recent_weeks_touse,
      resp_week_range = resp_week_range, top_matches = top_matches,
      error_exponentiation = error_exponentiation,
      min_allowed_weight = min_allowed_weight, n_sim = n_sim
    ),
    class = "COPYCAT"
  )
}

# Simulate coherent copycat trajectories: returns a tibble (id, horizon,
# forecast), where each `id` is ONE resampled analog compounded forward -- i.e.
# a single multi-step trajectory borrowed from one DB curve. forecast() reduces
# this to per-horizon marginals; generate() keeps whole trajectories.
.copycat_sim <- function(object, h) {
  o <- object
  fallback <- tibble::tibble(id = 1L, horizon = seq_len(h),
                             forecast = o$most_recent_value)

  cleaned <- o$curr[!is.na(o$curr$curr_weekly_change),
                    c("resp_season_week", "curr_weekly_change")]
  cleaned <- utils::tail(cleaned, o$recent_weeks_touse)
  if (nrow(cleaned) == 0) return(fallback)

  if (o$resp_week_range != 0) {
    shifts <- c(-(seq_len(o$resp_week_range)), 0, seq_len(o$resp_week_range))
    matching <- cleaned |>
      dplyr::mutate(week_change = list(shifts)) |>
      tidyr::unnest("week_change") |>
      dplyr::mutate(resp_season_week = .data$resp_season_week + .data$week_change) |>
      dplyr::filter(.data$resp_season_week > 0)
  } else {
    matching <- dplyr::mutate(cleaned, week_change = 0L)
  }

  db2 <- o$db |>
    tidyr::complete(tidyr::nesting(!!!rlang::syms(c("location", "resp_season"))),
                    resp_season_week = min(o$db$resp_season_week):max(o$db$resp_season_week)) |>
    dplyr::group_by(.data$location, .data$resp_season) |>
    dplyr::arrange(.data$resp_season_week, .by_group = TRUE) |>
    tidyr::fill("pred", "pred_se") |>
    dplyr::ungroup()

  traj_temp <- db2 |>
    dplyr::inner_join(matching, by = "resp_season_week",
                      relationship = "many-to-many") |>
    dplyr::group_by(.data$week_change, .data$location, .data$resp_season) |>
    dplyr::filter(dplyr::n() == nrow(cleaned) | dplyr::n() >= 4) |>
    dplyr::summarize(
      weight = sum((.data$pred - .data$curr_weekly_change)^2) / dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::filter(!is.na(.data$weight))
  if (nrow(traj_temp) == 0) return(fallback)

  trajectories <- traj_temp |>
    dplyr::mutate(weight = pmax(.data$weight, o$min_allowed_weight)) |>
    dplyr::arrange(.data$weight) |>
    dplyr::slice(seq_len(min(o$top_matches, dplyr::n()))) |>
    dplyr::slice_sample(n = o$n_sim, replace = TRUE,
                        weight_by = 1 / .data$weight^o$error_exponentiation) |>
    dplyr::mutate(id = dplyr::row_number()) |>
    dplyr::select("id", "location", "resp_season", "week_change")

  paths <- trajectories |>
    dplyr::left_join(
      tidyr::nest(db2, data = c("resp_season_week", "pred", "pred_se")),
      by = c("location", "resp_season")
    ) |>
    tidyr::unnest("data") |>
    dplyr::mutate(resp_season_week = .data$resp_season_week - .data$week_change) |>
    dplyr::filter(.data$resp_season_week %in%
                    o$most_recent_week:(o$most_recent_week + h - 1)) |>
    dplyr::group_by(.data$id) |>
    dplyr::arrange(.data$resp_season_week, .by_group = TRUE) |>
    dplyr::mutate(
      # independent per-week lognormal growth around the analog's smoothed rate
      weekly_change = exp(stats::rnorm(dplyr::n(), .data$pred, .data$pred_se)),
      forecast = o$most_recent_value * cumprod(.data$weekly_change),
      horizon = dplyr::row_number()
    ) |>
    dplyr::ungroup() |>
    dplyr::select("id", "horizon", "forecast")

  if (nrow(paths) == 0) fallback else paths
}

#' @importFrom fabletools forecast
#' @method forecast COPYCAT
#' @export
forecast.COPYCAT <- function(object, new_data, specials = NULL, ...) {
  h <- nrow(new_data)
  sim <- .copycat_sim(object, h)
  samples <- split(sim$forecast, sim$horizon)
  distributional::dist_sample(lapply(seq_len(h), function(k) {
    s <- samples[[as.character(k)]]
    if (is.null(s) || length(s) == 0) object$most_recent_value else s
  }))
}

#' @importFrom fabletools generate
#' @method generate COPYCAT
#' @export
generate.COPYCAT <- function(x, new_data, specials = NULL, ...) {
  h <- max(dplyr::count(new_data, .data$.rep)$n)
  sim <- .copycat_sim(x, h)
  ord <- order(sim$id, sim$horizon)
  traj <- split(sim$forecast[ord], sim$id[ord])
  traj <- traj[vapply(traj, length, integer(1)) == h]  # keep whole trajectories
  if (length(traj) == 0) traj <- list(rep(x$most_recent_value, h))
  reps <- unique(new_data$.rep)
  pick <- stats::setNames(
    sample.int(length(traj), length(reps), replace = TRUE),
    as.character(reps)
  )
  # each .rep is assigned ONE coherent analog trajectory
  new_data |>
    dplyr::group_by(.data$.rep) |>
    dplyr::mutate(.sim = traj[[pick[[as.character(.data$.rep[1])]]]][dplyr::row_number()]) |>
    dplyr::ungroup()
}

#' @importFrom stats fitted
#' @method fitted COPYCAT
#' @export
fitted.COPYCAT <- function(object, ...) rep(NA_real_, object$n)

#' @importFrom stats residuals
#' @method residuals COPYCAT
#' @export
residuals.COPYCAT <- function(object, ...) rep(NA_real_, object$n)

#' @importFrom fabletools model_sum
#' @method model_sum COPYCAT
#' @export
model_sum.COPYCAT <- function(x) {
  sprintf("COPYCAT[%d analogs]",
          nrow(dplyr::distinct(x$db, .data$location, .data$resp_season)))
}
