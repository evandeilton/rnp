skip_if_not_installed("parsnip")
skip_if_not_installed("rsample")
skip_if_not_installed("tune")
skip_if_not_installed("workflows")
skip_if_not_installed("recipes")
skip_if_not_installed("yardstick")

test_that("rnp_ml_particao e rnp_ml_cv criam objetos rsample", {
  sp <- rnp_ml_particao(iris, prop = 0.75, estrato = "Species")
  expect_s3_class(sp, "rsplit")
  expect_equal(nrow(rsample::training(sp)) + nrow(rsample::testing(sp)), 150L)
  cv <- rnp_ml_cv(iris, v = 5, estrato = "Species")
  expect_equal(nrow(cv), 5L)
})

test_that("rnp_ml_receita aplica passos", {
  rec <- rnp_ml_receita(Species ~ ., iris, passos = c("normalizar"))
  prep <- recipes::prep(rec)
  baked <- recipes::bake(prep, new_data = NULL)
  # apos normalizar, media ~ 0
  expect_lt(abs(mean(baked$Sepal.Length)), 1e-6)
})

test_that("rnp_ml_arvore ajusta e avalia (rpart)", {
  skip_if_not_installed("rpart")
  sp <- rnp_ml_particao(iris, estrato = "Species")
  fit <- rnp_ml_ajustar(rnp_ml_arvore("classificacao"), Species ~ ., sp)
  acc <- fit$metricas$.estimate[fit$metricas$.metric == "accuracy"]
  expect_gt(acc, 0.85)
  expect_s3_class(fit$modelo, "workflow")
})

test_that("rnp_ml_regularizada ajusta (glmnet)", {
  skip_if_not_installed("glmnet")
  sp <- rnp_ml_particao(mtcars, prop = 0.7)
  fit <- rnp_ml_ajustar(rnp_ml_regularizada("regressao", penalidade = 0.1),
                        mpg ~ ., sp)
  expect_true("rmse" %in% fit$metricas$.metric)
})

test_that("rnp_ml_prever e rnp_ml_importancia (rpart)", {
  skip_if_not_installed("rpart")
  sp <- rnp_ml_particao(iris, estrato = "Species")
  fit <- rnp_ml_ajustar(rnp_ml_arvore("classificacao"), Species ~ ., sp)
  pred <- rnp_ml_prever(fit$modelo, iris[1:5, ], tipo = "classe")
  expect_equal(nrow(pred), 5L)
  imp <- rnp_ml_importancia(fit$modelo)
  expect_true("variavel" %in% names(imp))
  expect_true(all(diff(imp$importancia) <= 1e-8))  # ordem decrescente
})

test_that("rnp_ml_tunagem busca hiperparametros (rpart)", {
  skip_if_not_installed("rpart")
  skip_if_not_installed("dials")
  sp <- rnp_ml_arvore("classificacao", custo_complexidade = hardhat::tune())
  r <- rnp_ml_tunagem(sp, Species ~ ., rnp_ml_cv(iris, v = 3), grade = 4)
  expect_true(nrow(r$melhores) >= 1)
  expect_s3_class(r$melhor_param, "tbl_df")
})

test_that("rnp_ml_comparar avalia varios modelos", {
  skip_if_not_installed("rpart")
  specs <- list(arvore = rnp_ml_arvore("classificacao"))
  cmp <- rnp_ml_comparar(specs, Species ~ ., rnp_ml_cv(iris, v = 3))
  expect_true("modelo" %in% names(cmp$tabela))
  expect_s3_class(cmp$grafico, "ggplot")
})
