test_that("rnp_glm reproduz glm (poisson e binomial)", {
  r <- rnp_glm(carb ~ hp + wt, mtcars, familia = "poisson")
  ref <- glm(carb ~ hp + wt, mtcars, family = poisson())
  expect_equal(r$coeficientes$estimativa, unname(round(coef(ref), 4)),
               tolerance = 1e-3)
  expect_equal(r$modelo$aic, round(AIC(ref), 4), tolerance = 1e-2)

  rb <- rnp_glm(am ~ mpg + wt, mtcars, familia = "binomial")
  refb <- glm(am ~ mpg + wt, mtcars, family = binomial())
  expect_equal(rb$coeficientes$estimativa, unname(round(coef(refb), 4)),
               tolerance = 1e-3)
})

test_that("rnp_glm_diagnosticos detecta superdispersao", {
  d <- rnp_glm_diagnosticos(glm(Days ~ Sex + Age, MASS::quine, family = poisson()),
                            grafico = TRUE)
  expect_true("dispersao" %in% d$testes$medida)
  disp <- d$testes$valor[d$testes$medida == "dispersao"]
  expect_gt(disp, 1.5)   # quine tem forte superdispersao
  expect_s3_class(d$grafico, "ggplot")
})

test_that("rnp_binomial_negativa reproduz MASS::glm.nb", {
  r <- rnp_binomial_negativa(Days ~ Sex + Age, MASS::quine)
  ref <- MASS::glm.nb(Days ~ Sex + Age, MASS::quine)
  expect_equal(r$theta, round(ref$theta, 4), tolerance = 1e-2)
  expect_equal(r$coeficientes$estimativa[1], unname(round(coef(ref)[1], 4)),
               tolerance = 1e-2)
})

test_that("rnp_regressao_ordinal reproduz MASS::polr", {
  r <- rnp_regressao_ordinal(Sat ~ Infl + Type, MASS::housing,
                             pesos = MASS::housing$Freq)
  ref <- MASS::polr(Sat ~ Infl + Type, MASS::housing, weights = Freq)
  expect_equal(r$coeficientes$estimativa,
               unname(round(coef(ref), 4)), tolerance = 1e-2)
  expect_true(nrow(r$limiares) >= 1)
})

test_that("rnp_modelo_misto ajusta lme com ICC", {
  r <- rnp_modelo_misto(distance ~ age, nlme::Orthodont,
                        aleatorio = ~ 1 | Subject)
  expect_true("age" %in% r$efeitos_fixos$termo)
  icc <- r$variancia$variancia[r$variancia$componente == "ICC"]
  expect_true(icc > 0 && icc < 1)
})

test_that("rnp_gam reproduz mgcv::gam", {
  set.seed(1)
  d <- data.frame(x = runif(200)); d$y <- sin(2 * pi * d$x) + rnorm(200, 0, 0.3)
  r <- rnp_gam(y ~ s(x), d)
  ref <- mgcv::gam(y ~ s(x), data = d)
  expect_equal(r$modelo$r2_ajustado, round(summary(ref)$r.sq, 4), tolerance = 1e-2)
  expect_gt(r$suaves$edf, 1)            # termo nao-linear
  expect_lt(r$suaves$p_valor, 0.05)
})

test_that("rnp_grafico_efeitos retorna ggplot", {
  set.seed(1)
  d <- data.frame(x = runif(200)); d$y <- sin(2 * pi * d$x) + rnorm(200, 0, 0.3)
  expect_s3_class(rnp_grafico_efeitos(rnp_gam(y ~ s(x), d)), "ggplot")
})
