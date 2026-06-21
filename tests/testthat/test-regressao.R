test_that("rnp_correlacao_teste funciona", {
  res <- rnp_correlacao_teste(mtcars$mpg, mtcars$wt)
  expect_s3_class(res, "tbl_df")
  expect_true("correlacao" %in% names(res))
  expect_true("p_valor" %in% names(res))
  expect_true("ic_inf" %in% names(res))
  expect_true("ic_sup" %in% names(res))
  expect_true(res$correlacao < 0)
})

test_that("rnp_correlacao_teste com diferentes metodos", {
  res1 <- rnp_correlacao_teste(mtcars$mpg, mtcars$wt, method = "pearson")
  res2 <- rnp_correlacao_teste(mtcars$mpg, mtcars$wt, method = "spearman")
  res3 <- rnp_correlacao_teste(mtcars$mpg, mtcars$wt, method = "kendall")
  expect_s3_class(res1, "tbl_df")
  expect_s3_class(res2, "tbl_df")
  expect_s3_class(res3, "tbl_df")
})

test_that("rnp_correlacao_parcial funciona", {
  res <- rnp_correlacao_parcial(mtcars$mpg, mtcars$hp, mtcars$wt)
  expect_s3_class(res, "tbl_df")
  expect_true("correlacao_parcial" %in% names(res))
})

test_that("rnp_regressao funciona", {
  res <- rnp_regressao(mpg ~ wt + hp, mtcars)
  expect_type(res, "list")
  expect_true("coeficientes" %in% names(res))
  expect_true("modelo" %in% names(res))
  expect_s3_class(res$coeficientes, "tbl_df")
  expect_true("r2" %in% names(res$modelo))
})

test_that("rnp_regressao_diagnosticos funciona", {
  fit <- lm(mpg ~ wt + hp, mtcars)
  res <- rnp_regressao_diagnosticos(fit)
  expect_type(res, "list")
  expect_true("pontos" %in% names(res))
  expect_true("testes" %in% names(res))
  expect_s3_class(res$pontos, "tbl_df")
  expect_true("cooks_d" %in% names(res$pontos))
})

test_that("rnp_logistic funciona", {
  res <- rnp_logistic(am ~ mpg + wt, mtcars)
  expect_type(res, "list")
  expect_true("coeficientes" %in% names(res))
  expect_true("odds_ratio" %in% names(res$coeficientes))
})

test_that("rnp_matriz_confusao funciona", {
  obs <- sample(c("Sim", "Nao"), 100, TRUE)
  pred <- sample(c("Sim", "Nao"), 100, TRUE)
  res <- rnp_matriz_confusao(obs, pred, positivo = "Sim")
  expect_type(res, "list")
  expect_true("matriz" %in% names(res))
  expect_true("metricas" %in% names(res))
  expect_true("sensibilidade" %in% names(res$metricas))
})

test_that("rnp_curva_roc funciona", {
  obs <- sample(0:1, 100, TRUE)
  esc <- runif(100)
  res <- rnp_curva_roc(obs, esc, positivo = 1)
  expect_type(res, "list")
  expect_true("curva" %in% names(res))
  expect_true("auc" %in% names(res))
  expect_true(res$auc >= 0 && res$auc <= 1)
})

test_that("rnp_roc_auc retorna escalar", {
  obs <- sample(0:1, 100, TRUE)
  esc <- runif(100)
  auc <- rnp_roc_auc(obs, esc, positivo = 1)
  expect_true(is.numeric(auc))
  expect_true(auc >= 0 && auc <= 1)
})
