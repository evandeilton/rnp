test_that("rnp_emv recupera parametros da normal", {
  set.seed(1)
  x <- rnorm(500, 5, 2)
  ll <- function(th) sum(dnorm(x, th[1], th[2], log = TRUE))
  r <- rnp_emv(ll, inicio = c(0, 1), nomes = c("media", "dp"))
  expect_equal(r$estimativas$estimativa[1], mean(x), tolerance = 0.01)
  expect_equal(r$estimativas$estimativa[2], sqrt(mean((x - mean(x))^2)),
               tolerance = 0.05)
  expect_true(r$ajuste$convergiu)
  expect_true(all(r$estimativas$erro_padrao > 0))
})

test_that("rnp_metodo_momentos bate com formulas conhecidas", {
  set.seed(1)
  x <- rgamma(2000, shape = 2, rate = 1)
  r <- rnp_metodo_momentos(x, "gamma")
  m <- mean(x); v <- var(x)
  expect_equal(r$estimativa[r$parametro == "shape"], round(m^2 / v, 4))
  expect_equal(r$estimativa[r$parametro == "rate"], round(m / v, 4))
})

test_that("rnp_informacao_fisher da EP proximo do analitico (normal)", {
  set.seed(1)
  x <- rnorm(400, 0, 1)
  ll <- function(th) sum(dnorm(x, th[1], th[2], log = TRUE))
  r <- rnp_informacao_fisher(ll, c(mean(x), sd(x)))
  # EP da media ~ sigma/sqrt(n)
  expect_equal(r$erros_padrao[1], sd(x) / sqrt(length(x)), tolerance = 0.01)
})

test_that("rnp_log_verossimilhanca retorna ggplot", {
  set.seed(1); x <- rpois(100, 3)
  g <- rnp_log_verossimilhanca(function(l) sum(dpois(x, l, log = TRUE)),
                               intervalo = c(1, 6))
  expect_s3_class(g, "ggplot")
})

test_that("rnp_ic_verossimilhanca contem a EMV", {
  set.seed(1); x <- rpois(200, 3)
  ll <- function(l) sum(dpois(x, l, log = TRUE))
  ic <- rnp_ic_verossimilhanca(ll, emv = mean(x), intervalo = c(0.5, 8))
  expect_true(ic$limite_inferior < mean(x) && ic$limite_superior > mean(x))
  expect_true(ic$limite_inferior < 3 && ic$limite_superior > 3)
})

test_that("rnp_teste_razao_veross concorda com anova(lm) em qui-quadrado", {
  m0 <- lm(mpg ~ wt, mtcars)
  m1 <- lm(mpg ~ wt + hp, mtcars)
  r <- rnp_teste_razao_veross(m1, m0)
  expect_equal(r$gl, 1)
  expect_lt(r$p_valor, 0.05)  # hp e significativo
})

test_that("rnp_teste_wald reproduz summary do glm", {
  fit <- glm(am ~ mpg, mtcars, family = binomial())
  r <- rnp_teste_wald(fit)
  co <- summary(fit)$coefficients
  expect_equal(r$z, round(unname(co[, 3]), 4))
})

test_that("rnp_teste_score detecta H0 falsa", {
  set.seed(1); x <- rpois(200, 3)
  ll <- function(l) sum(dpois(x, l, log = TRUE))
  r_falso <- rnp_teste_score(ll, theta0 = 2)
  expect_lt(r_falso$p_valor, 0.05)
  r_certo <- rnp_teste_score(ll, theta0 = mean(x))
  expect_gt(r_certo$p_valor, 0.5)
})

test_that("rnp_bayes_conjugada: Beta-Binomial e Gama-Poisson", {
  bb <- rnp_bayes_conjugada("beta_binomial", priori = c(a = 1, b = 1),
                            dados = list(sucessos = 8, n = 10))
  # posterior Beta(9, 3): media = 9/12 = 0.75
  expect_equal(bb$media_post[1], 0.75)
  expect_equal(bb$valor[bb$parametro == "a"], 9)

  gp <- rnp_bayes_conjugada("gama_poisson", priori = c(a = 2, b = 1),
                            dados = list(soma = 30, n = 10))
  # posterior Gamma(32, 11): media = 32/11
  expect_equal(gp$media_post[1], round(32 / 11, 4))
})
