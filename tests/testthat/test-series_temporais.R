test_that("rnp_ts_decomposicao funciona", {
  x <- ts(rnorm(100) + rep(1:4, 25), frequency = 4)
  res <- rnp_ts_decomposicao(x)
  expect_type(res, "list")
  expect_true("componentes" %in% names(res))
  expect_s3_class(res$componentes, "tbl_df")
})

test_that("rnp_ts_acf_pacf funciona", {
  res <- rnp_ts_acf_pacf(rnorm(100), lag.max = 20, plot = FALSE)
  expect_type(res, "list")
  expect_true("acf" %in% names(res))
  expect_true("pacf" %in% names(res))
  expect_s3_class(res$acf, "tbl_df")
})

test_that("rnp_ts_teste_estacionariedade funciona", {
  skip_if_not_installed("forecast")
  skip_if_not_installed("tseries")
  x <- rnorm(100)
  res <- rnp_ts_teste_estacionariedade(x, test = "kpss")
  expect_s3_class(res, "tbl_df")
  expect_true("estacionaria" %in% names(res))
})

test_that("rnp_ts_arima funciona", {
  skip_if_not_installed("forecast")
  x <- ts(rnorm(100), frequency = 12)
  res <- rnp_ts_arima(x)
  expect_type(res, "list")
  expect_true("modelo" %in% names(res))
  expect_true("resumo" %in% names(res))
})
