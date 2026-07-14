test_that("auto-ARIMA selects a model rather than a null model", {
  # The epi-motivated-forecasting session fits ARIMA() without a pdq()
  # specification, so fable runs a KPSS unit-root test to pick the order.
  # That test needs urca. Without it fable returns a null model, generate()
  # yields all-NA samples, and the ARIMA forecast silently disappears from
  # the session plots instead of erroring.
  skip_if_not_installed("fable")
  skip_if_not_installed("tsibble")

  set.seed(7321)
  train <- tibble::tibble(
    date = seq(as.Date("2023-01-01"), by = "day", length.out = 60),
    confirm = round(
      exp(seq(log(50), log(3000), length.out = 60)) * runif(60, 0.9, 1.1)
    )
  )

  fit <- train |>
    tsibble::as_tsibble(index = date) |>
    fabletools::model(auto = fable::ARIMA(confirm))

  expect_false(fabletools::is_null_model(fit$auto[[1]]))

  sims <- fabletools::generate(fit, h = 7, times = 5)
  expect_false(any(is.na(sims$.sim)))
})
