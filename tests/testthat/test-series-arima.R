test_that("rnp_arima reproduz stats::arima", {
  r <- rnp_arima(lh, ordem = c(1, 0, 0))
  ref <- arima(lh, order = c(1, 0, 0))
  expect_equal(r$coeficientes$estimativa[1], round(unname(ref$coef["ar1"]), 4),
               tolerance = 1e-3)
  expect_equal(r$modelo$aic, round(ref$aic, 4), tolerance = 1e-2)
})

test_that("rnp_sarima ajusta modelo sazonal", {
  r <- rnp_sarima(AirPassengers, c(0, 1, 1), c(0, 1, 1), 12)
  ref <- arima(AirPassengers, order = c(0, 1, 1),
               seasonal = list(order = c(0, 1, 1), period = 12))
  expect_equal(r$modelo$aic, round(ref$aic, 4), tolerance = 1e-2)
  expect_true("sma1" %in% r$coeficientes$termo)
})

test_that("rnp_auto_arima seleciona ordem por criterio", {
  r <- rnp_auto_arima(lh, max_p = 2, max_d = 1, max_q = 2)
  expect_true(nrow(r$selecao) >= 1)
  expect_true(is.finite(r$modelo$aicc))
})

test_that("rnp_ts_previsao retorna previsao e grafico", {
  m <- rnp_arima(lh, c(1, 0, 0))
  p <- rnp_ts_previsao(m, h = 6)
  expect_equal(nrow(p$previsao), 6L)
  expect_true(all(p$previsao$ic_inf < p$previsao$ic_sup))
  expect_s3_class(p$grafico, "ggplot")
})

test_that("rnp_ts_adf detecta estacionariedade", {
  set.seed(1)
  estac <- rnorm(200)                       # ruido branco -> estacionaria
  passeio <- cumsum(rnorm(200))             # passeio aleatorio -> nao-estacionaria
  expect_true(rnp_ts_adf(estac)$estacionaria)
  expect_false(rnp_ts_adf(passeio)$estacionaria)
})

test_that("rnp_ts_kpss coerente com ADF", {
  set.seed(2)
  estac <- rnorm(200)
  passeio <- cumsum(rnorm(200))
  expect_true(rnp_ts_kpss(estac)$estacionaria)
  expect_false(rnp_ts_kpss(passeio)$estacionaria)
})

test_that("rnp_ts_ccf reproduz stats::ccf", {
  r <- rnp_ts_ccf(as.numeric(mdeaths), as.numeric(fdeaths), lag_max = 10)
  ref <- ccf(as.numeric(mdeaths), as.numeric(fdeaths), lag.max = 10, plot = FALSE)
  expect_equal(r$tabela$ccf, round(as.numeric(ref$acf), 4))
  expect_s3_class(r$grafico, "ggplot")
})

test_that("rnp_ts_var ajusta e testa Granger", {
  df <- data.frame(m = as.numeric(mdeaths), f = as.numeric(fdeaths))
  r <- rnp_ts_var(df, p = 2)
  expect_true(all(c("causa", "efeito", "p_valor") %in% names(r$granger)))
  expect_equal(nrow(r$granger), 2L)  # 2 series -> 2 pares
})

test_that("rnp_ts_garch estima persistencia em (0,1)", {
  set.seed(3)
  n <- 600; e <- rnorm(n); s2 <- numeric(n); s2[1] <- 1; y <- numeric(n)
  for (t in 2:n) { s2[t] <- 0.05 + 0.1 * y[t-1]^2 + 0.85 * s2[t-1]; y[t] <- sqrt(s2[t]) * e[t] }
  r <- rnp_ts_garch(y)
  expect_true(r$persistencia > 0 && r$persistencia < 1)
  expect_length(r$volatilidade, n)
})

test_that("rnp_ts_residuos diagnostica ruido branco", {
  r <- rnp_ts_residuos(rnp_arima(lh, c(1, 0, 0)))
  expect_true("ljung-box" %in% r$teste)
  expect_true(all(c("estatistica", "p_valor") %in% names(r)))
})
