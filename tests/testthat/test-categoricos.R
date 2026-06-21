test_that("rnp_teste_fisher bate com fisher.test", {
  tb <- matrix(c(8, 2, 1, 5), 2, 2)
  r <- rnp_teste_fisher(tb)
  expect_equal(r$p_valor, round(fisher.test(tb)$p.value, 4))
  expect_true("odds_ratio" %in% names(r))
})

test_that("rnp_odds_ratio calcula OR e IC corretos", {
  tb <- matrix(c(20, 10, 15, 25), 2, 2, byrow = TRUE)
  r <- rnp_odds_ratio(tb)
  expect_equal(r$odds_ratio, round((20 * 25) / (10 * 15), 4))
  expect_true(r$ic_inf < r$odds_ratio && r$odds_ratio < r$ic_sup)
})

test_that("rnp_risco_relativo calcula RR", {
  tb <- matrix(c(20, 80, 10, 90), 2, 2, byrow = TRUE)
  r <- rnp_risco_relativo(tb)
  expect_equal(r$risco_expostos, 0.2)
  expect_equal(r$risco_relativo, round(0.2 / 0.1, 4))
})

test_that("rnp_kappa concorda com calculo manual", {
  a1 <- c("a", "b", "a", "c", "b", "a", "c", "c")
  a2 <- c("a", "b", "a", "c", "c", "a", "c", "b")
  r <- rnp_kappa(a1, a2)
  expect_true(r$kappa > 0 && r$kappa <= 1)
  # concordancia observada = 6/8
  expect_equal(r$concordancia_observada, 0.75)
})

test_that("rnp_kappa ponderado funciona com ordinal", {
  a1 <- c(1, 2, 3, 1, 2, 3)
  a2 <- c(1, 2, 2, 1, 3, 3)
  expect_s3_class(rnp_kappa(a1, a2, ponderado = TRUE), "tbl_df")
})
