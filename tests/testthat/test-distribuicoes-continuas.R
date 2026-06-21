test_that("wrappers continuos batem com stats", {
  expect_equal(rnp_distribuicao_normal("d", x = 0), dnorm(0))
  expect_equal(rnp_distribuicao_normal("p", q = 1.96), pnorm(1.96))
  expect_equal(rnp_distribuicao_exponencial("p", q = 1, taxa = 0.5), pexp(1, 0.5))
  expect_equal(rnp_distribuicao_gama("d", x = 2, forma = 2, taxa = 1), dgamma(2, 2, 1))
  expect_equal(rnp_distribuicao_beta("d", x = 0.5, a = 2, b = 2), dbeta(0.5, 2, 2))
  expect_equal(rnp_distribuicao_uniforme("p", q = 0.5), punif(0.5))
  expect_equal(rnp_distribuicao_t("q", p = 0.975, gl = 10), qt(0.975, 10))
  expect_equal(rnp_distribuicao_qui_quadrado("q", p = 0.95, gl = 3), qchisq(0.95, 3))
  expect_equal(rnp_distribuicao_f("q", p = 0.95, gl1 = 3, gl2 = 20), qf(0.95, 3, 20))
  expect_equal(rnp_distribuicao_lognormal("d", x = 1), dlnorm(1))
  expect_equal(rnp_distribuicao_weibull("p", q = 1, forma = 1.5, escala = 1),
               pweibull(1, 1.5, 1))
})

test_that("wrappers validam parametros invalidos", {
  expect_error(rnp_distribuicao_normal("d", x = 0, dp = -1))
  expect_error(rnp_distribuicao_gama("d", x = 1, forma = -2))
  expect_error(rnp_distribuicao_t("d", x = 0, gl = 0))
  expect_error(rnp_distribuicao_uniforme("d", x = 0, min = 1, max = 0))
})

test_that("multinomial: massa e amostra", {
  expect_equal(
    rnp_distribuicao_multinomial("d", tamanho = 10, probs = c(.2, .3, .5),
                                 x = c(2, 3, 5)),
    dmultinom(c(2, 3, 5), 10, c(.2, .3, .5)))
  set.seed(1)
  am <- rnp_distribuicao_multinomial("r", tamanho = 10, probs = c(.2, .3, .5), n = 4)
  expect_equal(dim(am), c(4L, 3L))
  expect_true(all(rowSums(am) == 10))
  expect_error(rnp_distribuicao_multinomial("d", tamanho = 10,
                                            probs = c(.2, .3), x = c(1, 2, 3)))
})

test_that("rnp_grafico_distribuicao retorna ggplot (continua e discreta)", {
  expect_s3_class(rnp_grafico_distribuicao("norm", mean = 0, sd = 1), "ggplot")
  expect_s3_class(rnp_grafico_distribuicao("binom", size = 10, prob = 0.3), "ggplot")
})

test_that("rnp_ajuste_distribuicao recupera parametros", {
  set.seed(123)
  x <- rexp(2000, rate = 0.5)
  r <- rnp_ajuste_distribuicao(x, "exp")
  taxa <- r$parametros$estimativa[r$parametros$parametro == "rate"]
  expect_equal(taxa, 0.5, tolerance = 0.05)
  expect_true(r$qualidade$ks_estatistica < 0.1)

  set.seed(7)
  y <- rnorm(2000, 10, 2)
  rn <- rnp_ajuste_distribuicao(y, "norm")
  expect_equal(rn$parametros$estimativa[1], 10, tolerance = 0.2)
  expect_true(is.finite(rn$qualidade$aic))
})

test_that("rnp_ajuste_distribuicao rejeita dados invalidos", {
  expect_error(rnp_ajuste_distribuicao(c(-1, 2, 3), "exp"))
})
