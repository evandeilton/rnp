test_that("rnp_descritiva retorna tibble com todas as colunas", {
  x <- rnorm(100, mean = 10, sd = 2)
  res <- rnp_descritiva(x)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 1L)
  expected_cols <- c("n", "n_validos", "n_faltantes", "soma", "media", "mediana",
                     "moda", "desvio", "variancia", "min", "q1", "q3", "max",
                     "amplitude", "iqr", "cv", "se_media", "ic_inf", "ic_sup",
                     "assimetria", "curtose")
  expect_true(all(expected_cols %in% names(res)))
  expect_equal(res$n, 100L)
  expect_equal(res$n_faltantes, 0L)
  expect_true(res$media > 8 && res$media < 12)
})

test_that("rnp_descritiva lida com NA corretamente", {
  x <- c(1, 2, NA, 4, 5)
  res <- rnp_descritiva(x, na.rm = TRUE)
  expect_equal(res$n, 5L)
  expect_equal(res$n_validos, 4L)
  expect_equal(res$n_faltantes, 1L)
})

test_that("rnp_descritiva aborta com entrada invalida", {
  expect_error(rnp_descritiva("abc"))
  expect_error(rnp_descritiva(NULL))
  expect_error(rnp_descritiva(matrix(1:4, 2, 2)))
})

test_that("rnp_descritiva_by funciona com agrupamento", {
  res <- rnp_descritiva_by(mtcars, "mpg", "cyl")
  expect_s3_class(res, "tbl_df")
  expect_true("cyl" %in% names(res))
  expect_true("media" %in% names(res))
  expect_equal(nrow(res), length(unique(mtcars$cyl)))
})

test_that("rnp_quantis retorna quantis corretos", {
  x <- 1:100
  q <- rnp_quantis(x, c(0.25, 0.5, 0.75))
  expect_length(q, 3L)
  expect_true(q[1] < q[2] && q[2] < q[3])
})

test_that("rnp_skewness calcula assimetria", {
  set.seed(42)
  x <- rnorm(1000)
  sk <- rnp_skewness(x)
  expect_true(is.numeric(sk))
  expect_true(abs(sk) < 0.5)
})

test_that("rnp_kurtosis calcula curtose", {
  set.seed(42)
  x <- rnorm(1000)
  ku <- rnp_kurtosis(x)
  expect_true(is.numeric(ku))
  expect_true(abs(ku) < 1)
})

test_that("rnp_outliers detecta outliers", {
  x <- c(rnorm(100), 50, -30)
  out <- rnp_outliers(x, method = "iqr")
  expect_s3_class(out, "tbl_df")
  expect_true(nrow(out) >= 2L)
  expect_true(50 %in% out$valor || -30 %in% out$valor)
})

test_that("rnp_outliers com diferentes metodos", {
  x <- c(rnorm(50), 100)
  out1 <- rnp_outliers(x, method = "iqr")
  out2 <- rnp_outliers(x, method = "zscore")
  out3 <- rnp_outliers(x, method = "modzscore")
  expect_s3_class(out1, "tbl_df")
  expect_s3_class(out2, "tbl_df")
  expect_s3_class(out3, "tbl_df")
})
