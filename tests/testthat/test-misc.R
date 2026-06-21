test_that("rnp_tamanho_efeito funciona para duas amostras", {
  x <- rnorm(30, 5)
  y <- rnorm(30, 5.5)
  res <- rnp_tamanho_efeito(x, y)
  expect_s3_class(res, "tbl_df")
  expect_true("cohens_d" %in% names(res))
  expect_true("hedges_g" %in% names(res))
  expect_true("r" %in% names(res))
  expect_true("eta2" %in% names(res))
})

test_that("rnp_tamanho_efeito funciona para amostras pareadas", {
  x <- rnorm(30, 5)
  y <- rnorm(30, 5.2)
  res <- rnp_tamanho_efeito(x, y, paired = TRUE)
  expect_s3_class(res, "tbl_df")
})

test_that("rnp_tamanho_efeito funciona para aov", {
  fit <- aov(mpg ~ factor(cyl), mtcars)
  res <- rnp_tamanho_efeito(fit)
  expect_s3_class(res, "tbl_df")
  expect_true(!is.na(res$eta2))
})

test_that("rnp_na_summary funciona", {
  res <- rnp_na_summary(airquality)
  expect_type(res, "list")
  expect_true("por_variavel" %in% names(res))
  expect_true("por_observacao" %in% names(res))
  expect_s3_class(res$por_variavel, "tbl_df")
  expect_true("n_faltantes" %in% names(res$por_variavel))
})

test_that("rnp_potencia funciona", {
  skip_if_not_installed("pwr")
  res <- rnp_potencia("t.test", d = 0.5, n = 30, sig.level = 0.05)
  expect_s3_class(res, "power.htest")
})
