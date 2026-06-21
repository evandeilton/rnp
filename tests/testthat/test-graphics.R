test_that("rnp_tema_rnp retorna theme", {
  tema <- rnp_tema_rnp()
  expect_s3_class(tema, "theme")
})

test_that("rnp_paleta_rnp retorna cores", {
  p1 <- rnp_paleta_rnp("rnp_qual")
  expect_true(is.character(p1))
  expect_true(length(p1) >= 5L)
  p2 <- rnp_paleta_rnp("rnp_seq", 5)
  expect_length(p2, 5L)
  p3 <- rnp_paleta_rnp("rnp_div", 9)
  expect_length(p3, 9L)
})

test_that("rnp_grafico_barras retorna ggplot", {
  skip_if_not_installed("ggplot2")
  p <- rnp_grafico_barras(mtcars, "cyl")
  expect_s3_class(p, "gg")
})

test_that("rnp_grafico_boxplot retorna ggplot", {
  skip_if_not_installed("ggplot2")
  p <- rnp_grafico_boxplot(mtcars, "cyl", "mpg")
  expect_s3_class(p, "gg")
})

test_that("rnp_grafico_dispersao retorna ggplot", {
  skip_if_not_installed("ggplot2")
  p <- rnp_grafico_dispersao(mtcars, "wt", "mpg")
  expect_s3_class(p, "gg")
})

test_that("rnp_grafico_histograma retorna ggplot", {
  skip_if_not_installed("ggplot2")
  p <- rnp_grafico_histograma(mtcars, "mpg")
  expect_s3_class(p, "gg")
})

test_that("rnp_grafico_qq retorna ggplot", {
  skip_if_not_installed("ggplot2")
  p <- rnp_grafico_qq(rnorm(100))
  expect_s3_class(p, "gg")
})

test_that("rnp_grafico_violino retorna ggplot", {
  skip_if_not_installed("ggplot2")
  p <- rnp_grafico_violino(mtcars, "cyl", "mpg")
  expect_s3_class(p, "gg")
})
