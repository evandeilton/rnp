test_that("rnp_ic_media retorna IC correto", {
  set.seed(42)
  x <- rnorm(100, mean = 10, sd = 2)
  res <- rnp_ic_media(x, conf = 0.95)
  expect_s3_class(res, "tbl_df")
  expect_true(res$limite_inferior < res$media)
  expect_true(res$media < res$limite_superior)
  expect_equal(res$n, 100L)
  expect_equal(res$nivel_confianca, 0.95)
})

test_that("rnp_ic_media com sigma conhecido usa Z", {
  x <- rnorm(100, mean = 10, sd = 2)
  res <- rnp_ic_media(x, sigma = 2)
  expect_equal(res$distribuicao, "normal")
})

test_that("rnp_ic_proporcao funciona com diferentes metodos", {
  res1 <- rnp_ic_proporcao(45, 100, method = "wald")
  res2 <- rnp_ic_proporcao(45, 100, method = "wilson")
  res3 <- rnp_ic_proporcao(45, 100, method = "agresti")
  res4 <- rnp_ic_proporcao(45, 100, method = "clopper")
  expect_s3_class(res1, "tbl_df")
  expect_s3_class(res2, "tbl_df")
  expect_s3_class(res3, "tbl_df")
  expect_s3_class(res4, "tbl_df")
  expect_true(all(res1$limite_inferior >= 0))
  expect_true(all(res1$limite_superior <= 1))
})

test_that("rnp_ic_variancia retorna IC", {
  x <- rnorm(50, sd = 2)
  res <- rnp_ic_variancia(x)
  expect_s3_class(res, "tbl_df")
  expect_true(res$limite_inferior < res$variancia)
  expect_true(res$variancia < res$limite_superior)
})

test_that("rnp_ic_diff_medias funciona", {
  x <- rnorm(30, 5, 1)
  y <- rnorm(30, 5.5, 1)
  res <- rnp_ic_diff_medias(x, y)
  expect_s3_class(res, "tbl_df")
  expect_true("diff_medias" %in% names(res))
})

test_that("rnp_teste_t funciona para uma amostra", {
  set.seed(42)
  x <- rnorm(30, mean = 5)
  res <- rnp_teste_t(x, mu = 5)
  expect_s3_class(res, "tbl_df")
  expect_true("p_valor" %in% names(res))
  expect_true(res$p_valor > 0.05)
})

test_that("rnp_teste_t funciona para duas amostras", {
  x <- rnorm(30, 5)
  y <- rnorm(30, 5.5)
  res <- rnp_teste_t(x, y)
  expect_s3_class(res, "tbl_df")
  expect_true("p_valor" %in% names(res))
})

test_that("rnp_teste_z_media funciona", {
  x <- rnorm(100, mean = 10, sd = 2)
  res <- rnp_teste_z_media(x, mu = 10, sigma = 2)
  expect_s3_class(res, "tbl_df")
  expect_true("p_valor" %in% names(res))
})

test_that("rnp_teste_z_proporcao funciona", {
  res <- rnp_teste_z_proporcao(55, 100, p0 = 0.5)
  expect_s3_class(res, "tbl_df")
  expect_true("p_valor" %in% names(res))
})

test_that("rnp_teste_variancias funciona", {
  res <- rnp_teste_variancias(mtcars$mpg, as.factor(mtcars$cyl), method = "bartlett")
  expect_s3_class(res, "tbl_df")
  expect_true("p_valor" %in% names(res))
})

test_that("rnp_teste_shapiro funciona", {
  x <- rnorm(50)
  res <- rnp_teste_shapiro(x)
  expect_s3_class(res, "tbl_df")
  expect_equal(res$metodo, "shapiro-wilk")
})
