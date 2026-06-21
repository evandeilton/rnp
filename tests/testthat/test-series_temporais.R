test_that("acf_cpp/pacf_cpp batem com stats::acf/pacf", {
  set.seed(1)
  x <- as.numeric(arima.sim(list(ar = 0.6), 200))
  ref_acf <- as.numeric(acf(x, lag.max = 15, plot = FALSE)$acf)
  expect_equal(as.numeric(acf_cpp(x, 15L)), ref_acf, tolerance = 1e-8)
  ref_pacf <- as.numeric(pacf(x, lag.max = 15, plot = FALSE)$acf)
  expect_equal(as.numeric(pacf_cpp(x, 15L)), ref_pacf, tolerance = 1e-6)
})

test_that("media_movel_cpp e ewma_cpp coerentes", {
  x <- as.numeric(1:10)
  mm <- as.numeric(media_movel_cpp(x, 3L, FALSE))  # retroativa
  expect_equal(mm[3], mean(x[1:3]))
  e <- as.numeric(ewma_cpp(x, 0.5))
  expect_equal(e[1], x[1])
  expect_equal(e[2], 0.5 * x[2] + 0.5 * x[1])
})

test_that("rnp_ts_decomposicao retorna componentes", {
  x <- ts(rnorm(100) + rep(1:4, 25), frequency = 4)
  res <- rnp_ts_decomposicao(x)
  expect_s3_class(res$componentes, "tbl_df")
})

test_that("rnp_media_movel e rnp_suavizacao_exponencial", {
  mm <- rnp_media_movel(as.numeric(AirPassengers), k = 12)
  expect_true("media_movel" %in% names(mm))
  sv <- rnp_suavizacao_exponencial(as.numeric(AirPassengers), alpha = 0.3)
  expect_equal(sv$suavizada[1], AirPassengers[1])
})

test_that("rnp_ts_acf e rnp_ts_pacf retornam tibble com bandas", {
  a <- rnp_ts_acf(as.numeric(AirPassengers), lag_max = 20)
  expect_true(all(c("lag", "acf", "lim_inf", "lim_sup") %in% names(a)))
  expect_equal(a$acf[1], 1)
  p <- rnp_ts_pacf(as.numeric(AirPassengers), lag_max = 20)
  expect_equal(nrow(p), 20L)
})

test_that("rnp_ts_diferenciacao reduz o comprimento corretamente", {
  x <- as.numeric(AirPassengers)
  d <- rnp_ts_diferenciacao(x, d = 1, D = 1, s = 12)
  expect_equal(length(d), length(x) - 12 - 1)
})

test_that("rnp_ts_ljung_box concorda com Box.test", {
  set.seed(1); x <- rnorm(200)
  r <- rnp_ts_ljung_box(x, lag = 10)
  ref <- Box.test(x, lag = 10, type = "Ljung-Box")
  expect_equal(r$p_valor, round(ref$p.value, 4))
})

test_that("rnp_ts_holt_winters e rnp_ts_periodograma", {
  hw <- rnp_ts_holt_winters(AirPassengers)
  expect_true(hw$parametros$alpha >= 0 && hw$parametros$alpha <= 1)
  pg <- rnp_ts_periodograma(as.numeric(AirPassengers))
  expect_true(all(c("frequencia", "periodo", "densidade") %in% names(pg)))
})

test_that("graficos de serie retornam ggplot", {
  expect_s3_class(rnp_grafico_serie(as.numeric(AirPassengers)), "ggplot")
  expect_s3_class(rnp_grafico_acf(as.numeric(AirPassengers), "acf"), "ggplot")
  expect_s3_class(rnp_grafico_acf(as.numeric(AirPassengers), "pacf"), "ggplot")
})
