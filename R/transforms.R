#' Fourth-root variance-stabilising transformation
#'
#' Helper transformations used throughout the course to stabilise the variance
#' of case and wILI time series before fitting `fable` models. `my_fourth_root`
#' is a [fabletools::new_transformation()], so `fable` can invert it
#' automatically when producing forecasts back on the natural scale, e.g.
#' `ARIMA(my_fourth_root(wili))`.
#'
#' @param x A numeric vector.
#' @return `fourth_root()` and `inv_fourth_root()` return numeric vectors.
#'   `my_fourth_root` is a `transformation` object for use inside model
#'   formulae.
#' @examples
#' fourth_root(16)    # 2
#' inv_fourth_root(2) # 16
#' @name fourth_root
#' @export
fourth_root <- function(x) x^0.25

#' @rdname fourth_root
#' @export
inv_fourth_root <- function(x) x^4

#' @rdname fourth_root
#' @importFrom fabletools new_transformation
#' @export
my_fourth_root <- new_transformation(fourth_root, inv_fourth_root)
