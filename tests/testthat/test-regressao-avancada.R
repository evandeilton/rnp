test_that("ridge_cpp com lambda=0 reproduz OLS", {
  set.seed(1)
  X <- cbind(1, matrix(rnorm(60), 20, 3))
  y <- as.numeric(X %*% c(2, 1, -1, 0.5) + rnorm(20))
  expect_equal(as.numeric(ridge_cpp(X, y, 0)), as.numeric(solve(t(X) %*% X, t(X) %*% y)),
               tolerance = 1e-8)
})

test_that("enet_cd_cpp (ridge puro, alpha=0) aproxima solucao fechada", {
  set.seed(2)
  Z <- scale(matrix(rnorm(100), 50, 2)) * sqrt(49 / 50)  # ~ (1/n)sum z^2 = 1
  Z <- apply(Z, 2, function(c) c / sqrt(mean(c^2)))
  y <- as.numeric(Z %*% c(1, -2) + rnorm(50, sd = 0.1)); y <- y - mean(y)
  b <- as.numeric(enet_cd_cpp(Z, y, lambda = 0, alpha = 1, 10000L, 1e-9))
  # lambda 0 -> OLS sem intercepto
  expect_equal(b, as.numeric(solve(t(Z) %*% Z, t(Z) %*% y)), tolerance = 1e-4)
})

test_that("rnp_regressao_ridge ~ OLS quando lambda pequeno", {
  ols <- coef(lm(mpg ~ wt + hp, mtcars))
  rg <- rnp_regressao_ridge(mpg ~ wt + hp, mtcars, lambda = 1e-6)
  expect_equal(rg$estimativa, unname(round(ols, 4)), tolerance = 1e-2)
})

test_that("rnp_regressao_lasso zera coeficientes com lambda grande", {
  rl <- rnp_regressao_lasso(mpg ~ wt + hp + disp + drat + qsec, mtcars,
                            lambda = 5)
  n_zero <- sum(rl$estimativa[rl$termo != "(Intercept)"] == 0)
  expect_gt(n_zero, 0)
})

test_that("rnp_elastic_net retorna coeficientes", {
  re <- rnp_elastic_net(mpg ~ wt + hp + disp, mtcars, lambda = 0.3, alpha = 0.5)
  expect_true("(Intercept)" %in% re$termo)
  expect_equal(nrow(re), 4L)
})

test_that("rnp_regressao_polinomial ajusta e calcula R2", {
  r <- rnp_regressao_polinomial(dist ~ speed, cars, grau = 2)
  expect_equal(nrow(r$coeficientes), 3L)
  expect_gt(r$modelo$r2, 0.6)
})

test_that("rnp_regressao_ponderada bate com lm(weights=)", {
  w <- 1 / mtcars$wt
  ref <- coef(lm(mpg ~ wt + hp, mtcars, weights = w))
  r <- rnp_regressao_ponderada(mpg ~ wt + hp, mtcars, pesos = w)
  expect_equal(r$estimativa, unname(round(ref, 4)), tolerance = 1e-3)
})

test_that("rnp_regressao_stepwise seleciona modelo", {
  r <- rnp_regressao_stepwise(mpg ~ wt + hp + disp + drat, mtcars)
  expect_s3_class(r$formula_final, "formula")
  expect_true(nrow(r$coeficientes) >= 1L)
})

test_that("rnp_regressao_poisson bate com glm", {
  ref <- glm(carb ~ hp + wt, mtcars, family = poisson())
  r <- rnp_regressao_poisson(carb ~ hp + wt, mtcars)
  expect_equal(r$coeficientes$estimativa, unname(round(coef(ref), 4)),
               tolerance = 1e-3)
})

test_that("rnp_vif bate com calculo manual", {
  fit <- lm(mpg ~ wt + hp + disp, mtcars)
  r <- rnp_vif(fit)
  X <- model.matrix(fit)[, -1]
  vif_wt <- 1 / (1 - summary(lm(X[, "wt"] ~ X[, c("hp", "disp")]))$r.squared)
  expect_equal(r$vif[r$termo == "wt"], round(vif_wt, 4), tolerance = 1e-3)
})

test_that("rnp_anova_modelos e rnp_predicao funcionam", {
  m1 <- lm(mpg ~ wt, mtcars); m2 <- lm(mpg ~ wt + hp, mtcars)
  a <- rnp_anova_modelos(m1, m2)
  expect_s3_class(a, "tbl_df")
  p <- rnp_predicao(m2, mtcars[1:3, ], tipo = "confianca")
  expect_equal(nrow(p), 3L)
  expect_true(all(p$limite_inferior < p$ajuste & p$ajuste < p$limite_superior))
})

test_that("rnp_grafico_residuos retorna lista de ggplots", {
  g <- rnp_grafico_residuos(lm(mpg ~ wt + hp, mtcars))
  expect_named(g, c("residuo_ajustado", "qq", "escala_locacao", "residuo_leverage"))
  expect_s3_class(g$qq, "ggplot")
})

test_that("rnp_regressao_robusta proxima do OLS em dados limpos", {
  set.seed(1)
  d <- data.frame(x = rnorm(100), y = NA)
  d$y <- 1 + 2 * d$x + rnorm(100, sd = 0.5)
  rb <- rnp_regressao_robusta(y ~ x, d, metodo = "huber")
  ols <- coef(lm(y ~ x, d))
  expect_equal(rb$coeficientes$estimativa, unname(round(ols, 4)), tolerance = 0.1)
})

test_that("rnp_regressao_robusta resiste a outliers", {
  set.seed(1)
  d <- data.frame(x = 1:50)
  d$y <- 2 + 0.5 * d$x + rnorm(50, sd = 0.5)
  d$y[50] <- 200  # outlier severo
  rb <- rnp_regressao_robusta(y ~ x, d, metodo = "bisquare")
  ols <- coef(lm(y ~ x, d))
  # robusta fica mais perto da inclinacao verdadeira (0.5) que o OLS
  expect_lt(abs(rb$coeficientes$estimativa[2] - 0.5),
            abs(ols[2] - 0.5))
})

test_that("rnp_regressao_nao_linear recupera parametros", {
  set.seed(1)
  df <- data.frame(x = 1:20)
  df$y <- 3 * exp(0.2 * df$x) + rnorm(20, sd = 0.5)
  r <- rnp_regressao_nao_linear(y ~ a * exp(b * x), df,
                                inicio = list(a = 1, b = 0.1))
  expect_equal(r$estimativa[r$parametro == "b"], 0.2, tolerance = 0.05)
})

test_that("rnp_box_cox retorna lambda no intervalo", {
  r <- rnp_box_cox(mpg ~ wt + hp, mtcars)
  expect_true(r$lambda >= -2 && r$lambda <= 2)
  expect_s3_class(r$grafico, "ggplot")
})

test_that("rnp_regressao_multinomial ajusta iris", {
  r <- rnp_regressao_multinomial(Species ~ Sepal.Length + Petal.Length, iris)
  expect_equal(r$referencia, "setosa")
  # 2 classes nao-referencia x 3 termos (intercepto + 2 preditores)
  expect_equal(nrow(r$coeficientes), 6L)
})
