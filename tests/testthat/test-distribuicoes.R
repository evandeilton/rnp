test_that("rnp_tabela_classes retorna tibble correta", {
  x <- rnorm(500)
  res <- rnp_tabela_classes(x)
  expect_s3_class(res, "tbl_df")
  expected_cols <- c("classe", "lim_inf", "lim_sup", "ponto_medio",
                     "fa", "fr", "fa_acumulada", "fr_acumulada")
  expect_true(all(expected_cols %in% names(res)))
  expect_equal(sum(res$fa), 500L)
  expect_true(abs(sum(res$fr) - 1) < 0.01)
})

test_that("rnp_tabela_classes com diferentes regras", {
  x <- rnorm(100)
  res1 <- rnp_tabela_classes(x, regra = "sturges")
  res2 <- rnp_tabela_classes(x, regra = "scott")
  res3 <- rnp_tabela_classes(x, regra = "fd")
  res4 <- rnp_tabela_classes(x, regra = "fixa", k = 5)
  expect_s3_class(res1, "tbl_df")
  expect_s3_class(res2, "tbl_df")
  expect_s3_class(res3, "tbl_df")
  expect_s3_class(res4, "tbl_df")
  expect_equal(nrow(res4), 5L)
})

test_that("rnp_distribuicao funciona para norm", {
  d <- rnp_distribuicao("norm", "d", x = 0, mean = 0, sd = 1)
  expect_true(is.numeric(d))
  expect_true(d > 0.39 && d < 0.40)
  p <- rnp_distribuicao("norm", "p", q = 0, mean = 0, sd = 1)
  expect_equal(p, 0.5)
  q <- rnp_distribuicao("norm", "q", p = 0.975, mean = 0, sd = 1)
  expect_true(q > 1.95 && q < 1.97)
  r <- rnp_distribuicao("norm", "r", n = 100, mean = 0, sd = 1)
  expect_length(r, 100L)
})

test_that("rnp_distribuicao funciona para binom", {
  d <- rnp_distribuicao("binom", "d", x = 3, size = 10, prob = 0.5)
  expect_true(is.numeric(d))
  p <- rnp_distribuicao("binom", "p", q = 5, size = 10, prob = 0.5)
  expect_true(p > 0 && p < 1)
})

test_that("rnp_esperanca_var calcula corretamente", {
  res <- rnp_esperanca_var("binom", size = 10, prob = 0.5)
  expect_s3_class(res, "tbl_df")
  expect_equal(res$esperanca, 5)
  expect_equal(res$variancia, 2.5)
  res2 <- rnp_esperanca_var("pois", lambda = 3)
  expect_equal(res2$esperanca, 3)
  expect_equal(res2$variancia, 3)
})
