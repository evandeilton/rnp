test_that("rnp_teste_qui_quadrado: independencia e aderencia batem com stats", {
  ind <- suppressWarnings(rnp_teste_qui_quadrado(mtcars$cyl, mtcars$gear))
  ref <- suppressWarnings(chisq.test(table(mtcars$cyl, mtcars$gear)))
  expect_equal(ind$estatistica, round(unname(ref$statistic), 4))
  expect_true("v_cramer" %in% names(ind))

  ad <- rnp_teste_qui_quadrado(c(20, 30, 50), p = c(0.25, 0.25, 0.5))
  ref2 <- chisq.test(c(20, 30, 50), p = c(0.25, 0.25, 0.5))
  expect_equal(ad$estatistica, round(unname(ref2$statistic), 4))
})

test_that("rnp_teste_aderencia nao rejeita normal verdadeira", {
  set.seed(1)
  r <- rnp_teste_aderencia(rnorm(500), "norm", mean = 0, sd = 1, k = 10)
  expect_gt(r$p_valor, 0.05)
})

test_that("rnp_teste_ks: uma e duas amostras", {
  set.seed(1)
  r1 <- rnp_teste_ks(rnorm(200), dist = "pnorm", mean = 0, sd = 1)
  expect_gt(r1$p_valor, 0.05)
  r2 <- rnp_teste_ks(rnorm(80), rnorm(80, 2))
  expect_lt(r2$p_valor, 0.05)
})

test_that("rnp_teste_proporcoes e rnp_teste_binomial", {
  pp <- rnp_teste_proporcoes(c(45, 55), c(100, 100))
  expect_s3_class(pp, "tbl_df")
  bb <- rnp_teste_binomial(8, 10, p0 = 0.5)
  expect_equal(bb$p_valor, round(binom.test(8, 10, 0.5)$p.value, 4))
})

test_that("rnp_poder_teste e rnp_tamanho_amostra_teste sao coerentes", {
  pw <- rnp_poder_teste(efeito = 0.8, n = 30, tipo = "duas")
  expect_s3_class(pw$grafico, "ggplot")
  expect_true(pw$poder$poder > 0.7)
  # tamanho de amostra para d=0.5, poder 0.8, duas amostras ~ 64 por grupo
  ta <- rnp_tamanho_amostra_teste(efeito = 0.5, poder = 0.8, tipo = "duas")
  expect_true(ta$n >= 60 && ta$n <= 70)
  expect_gte(ta$poder_obtido, 0.8)
})

test_that("rnp_teste_normalidade: 3 metodos coerentes", {
  set.seed(1)
  x <- rnorm(300)
  for (mt in c("shapiro", "jarque_bera", "anderson_darling")) {
    expect_gt(rnp_teste_normalidade(x, mt)$p_valor, 0.05)
  }
  set.seed(2)
  y <- rexp(300)  # assimetrica
  expect_lt(rnp_teste_normalidade(y, "jarque_bera")$p_valor, 0.05)
})

test_that("rnp_teste_grubbs detecta outlier", {
  set.seed(1)
  r <- rnp_teste_grubbs(c(rnorm(40), 10))
  expect_lt(r$p_valor, 0.05)
  expect_equal(r$valor_extremo, 10, tolerance = 0.5)
})

test_that("rnp_teste_runs e rnp_teste_sinais", {
  set.seed(1)
  r <- rnp_teste_runs(rnorm(200))
  expect_gt(r$p_valor, 0.05)  # aleatorio
  # sequencia perfeitamente alternada -> nao aleatoria
  alt <- rep(c(1, -1), 50)
  expect_lt(rnp_teste_runs(alt)$p_valor, 0.05)

  # x - 5 = (0, 1, 2, -1, 3, 4): o zero sai; 4 positivos, 1 negativo
  sg <- rnp_teste_sinais(c(5, 6, 7, 4, 8, 9), mu = 5)
  expect_equal(sg$n_positivos, 4)
  expect_equal(sg$n_negativos, 1)
})
