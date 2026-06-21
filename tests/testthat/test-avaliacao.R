test_that("rnp_metricas_classificacao bate com calculo manual (binario)", {
  obs <- factor(c("sim","sim","nao","nao","sim","nao"))
  pred <- factor(c("sim","nao","nao","nao","sim","sim"))
  r <- rnp_metricas_classificacao(obs, pred, positivo = "sim")
  val <- setNames(r$valor, r$metrica)
  # TP=2, FP=1, FN=1, TN=2
  expect_equal(unname(val["precisao"]), round(2/3, 4))
  expect_equal(unname(val["revocacao"]), round(2/3, 4))
  expect_equal(unname(val["acuracia"]), round(4/6, 4))
})

test_that("rnp_metricas_classificacao multiclasse usa macro", {
  r <- rnp_metricas_classificacao(iris$Species,
                                  sample(iris$Species))
  expect_true("f1_macro" %in% r$metrica)
})

test_that("rnp_metricas_regressao bate com formulas", {
  set.seed(1); y <- rnorm(50); p <- y + rnorm(50, 0, 0.3)
  r <- rnp_metricas_regressao(y, p)
  val <- setNames(r$valor, r$metrica)
  expect_equal(unname(val["rmse"]), round(sqrt(mean((y - p)^2)), 4))
  expect_equal(unname(val["mae"]), round(mean(abs(y - p)), 4))
})

test_that("rnp_curva_lift e rnp_curva_ganho", {
  set.seed(1); y <- rbinom(300, 1, 0.3); s <- y * 0.5 + runif(300)
  lift <- rnp_curva_lift(y, s, positivo = 1)
  expect_gt(lift$tabela$lift[1], 1)            # primeira faixa concentra positivos
  expect_s3_class(lift$grafico, "ggplot")
  ganho <- rnp_curva_ganho(y, s, positivo = 1)
  expect_equal(tail(ganho$tabela$prop_positivos, 1), 1)  # captura tudo no fim
})

test_that("rnp_calibracao com Hosmer-Lemeshow (modelo bem calibrado)", {
  set.seed(1); p <- runif(500); y <- rbinom(500, 1, p)
  r <- rnp_calibracao(y, p, positivo = 1)
  expect_gt(r$hosmer_lemeshow$p_valor, 0.05)   # bem calibrado -> nao rejeita
  expect_s3_class(r$grafico, "ggplot")
})

test_that("rnp_brier coerente", {
  set.seed(1); p <- runif(300); y <- rbinom(300, 1, p)
  r <- rnp_brier(y, p, positivo = 1)
  expect_equal(r$brier, round(mean((p - y)^2), 4))
  expect_lt(r$brier, r$brier_referencia)       # melhor que prever a prevalencia
})

test_that("rnp_ks_classificador separa classes", {
  set.seed(1); y <- rbinom(300, 1, 0.3); s <- y * 0.6 + runif(300)
  r <- rnp_ks_classificador(y, s, positivo = 1)
  expect_true(r$ks > 0 && r$ks <= 1)
  expect_s3_class(r$grafico, "ggplot")
})

test_that("rnp_curva_precisao_revocacao calcula AUC-PR", {
  set.seed(1); y <- rbinom(300, 1, 0.3); s <- y * 0.6 + runif(300)
  r <- rnp_curva_precisao_revocacao(y, s, positivo = 1)
  expect_true(r$auc_pr > 0.3 && r$auc_pr <= 1)  # acima da base
})

test_that("rnp_comparar_roc (DeLong) detecta modelo melhor", {
  set.seed(1); y <- rbinom(400, 1, 0.4)
  s1 <- y * 0.8 + runif(400); s2 <- y * 0.1 + runif(400)
  r <- rnp_comparar_roc(y, s1, s2, positivo = 1)
  expect_gt(r$auc1, r$auc2)
  expect_lt(r$p_valor, 0.05)
  # confere AUC1 com rnp_curva_roc
  expect_equal(r$auc1, rnp_curva_roc(y, s1, positivo = 1)$auc, tolerance = 0.01)
})

test_that("rnp_acuracia_diagnostica reproduz sensibilidade/especificidade", {
  obs <- c("D","D","D","S","S","S","S","D")
  tst <- c("+","+","-","-","-","+","-","+")
  r <- rnp_acuracia_diagnostica(obs, tst, positivo = "D")
  val <- setNames(r$valor, r$metrica)
  # doentes: 4 (3 testaram +), TP=3, FN=1 -> sens=0.75
  expect_equal(unname(val["sensibilidade"]), 0.75)
})
