test_that("rnp_pca funciona", {
  res <- rnp_pca(mtcars[, c("mpg", "disp", "hp", "drat", "wt")])
  expect_type(res, "list")
  expect_true("scores" %in% names(res))
  expect_true("loadings" %in% names(res))
  expect_true("variancia" %in% names(res))
  expect_s3_class(res$scores, "tbl_df")
})

test_that("rnp_kmeans funciona", {
  res <- rnp_kmeans(mtcars[, c("mpg", "hp", "wt")], k = 3)
  expect_type(res, "list")
  expect_true("clusters" %in% names(res))
  expect_true("centros" %in% names(res))
  expect_true("metricas" %in% names(res))
  expect_equal(nrow(res$clusters), nrow(mtcars))
})

test_that("rnp_distancia funciona", {
  d <- rnp_distancia(mtcars[1:5, c("mpg", "hp")], method = "euclidean")
  expect_s3_class(d, "dist")
  expect_equal(attr(d, "Size"), 5L)
})

test_that("rnp_mds funciona", {
  d <- dist(mtcars[1:10, c("mpg", "hp", "wt")])
  res <- rnp_mds(d, k = 2)
  expect_type(res, "list")
  expect_true("pontos" %in% names(res))
  expect_equal(ncol(res$pontos), 2L)
})
