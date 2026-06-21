test_that("rnp_amostra_simples funciona", {
  res <- rnp_amostra_simples(mtcars, 10)
  expect_equal(nrow(res), 10L)
  res2 <- rnp_amostra_simples(1:100, 20)
  expect_length(res2, 20L)
})

test_that("rnp_amostra_sistematica funciona", {
  res <- rnp_amostra_sistematica(mtcars, 10)
  expect_equal(nrow(res), 10L)
})

test_that("rnp_amostra_estratificada funciona", {
  res <- rnp_amostra_estratificada(mtcars, "cyl", n = c("4" = 3, "6" = 2, "8" = 3))
  expect_s3_class(res, "data.frame")
  expect_true(nrow(res) <= 8L)
})

test_that("rnp_amostra_conglomerada funciona", {
  res <- rnp_amostra_conglomerada(mtcars, "cyl", n_cong = 2)
  expect_s3_class(res, "data.frame")
  expect_true(length(unique(res$cyl)) == 2L)
})

test_that("rnp_tamanho_amostra_media funciona", {
  res <- rnp_tamanho_amostra_media(sigma = 10, E = 2, conf = 0.95)
  expect_s3_class(res, "tbl_df")
  expect_true(res$n_infinita > 0)
  expect_true(res$n_finita > 0)
})

test_that("rnp_tamanho_amostra_proporcao funciona", {
  res <- rnp_tamanho_amostra_proporcao(E = 0.05, conf = 0.95)
  expect_s3_class(res, "tbl_df")
  expect_true(res$n_infinita > 0)
})

test_that("rnp_tamanho_amostra com populacao finita", {
  res <- rnp_tamanho_amostra_media(sigma = 10, E = 2, conf = 0.95, N = 1000)
  expect_true(res$n_finita < res$n_infinita)
})
