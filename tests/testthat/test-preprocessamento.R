test_that("rnp_padroniza e rnp_normaliza", {
  z <- rnp_padroniza(mtcars$mpg)
  expect_equal(mean(z), 0, tolerance = 1e-8)
  expect_equal(sd(z), 1, tolerance = 1e-8)
  nm <- rnp_normaliza(mtcars$hp)
  expect_equal(min(nm), 0); expect_equal(max(nm), 1)
})

test_that("rnp_winsoriza limita extremos", {
  x <- c(-100, 1:20, 200)
  w <- rnp_winsoriza(x, p = 0.05)
  expect_true(max(w) < 200 && min(w) > -100)
  expect_equal(length(w), length(x))
})

test_that("rnp_imputa: media/mediana/moda", {
  df <- data.frame(a = c(1, NA, 3, 4), b = c(NA, 2, 3, 4))
  r <- rnp_imputa(df, metodo = "media")
  expect_false(anyNA(r))
  expect_equal(r$a[2], mean(c(1, 3, 4)))
})

test_that("rnp_imputa knn preenche NA (backend C++)", {
  set.seed(1)
  df <- as.data.frame(matrix(rnorm(100), 25, 4))
  df[5, 2] <- NA; df[10, 3] <- NA
  r <- rnp_imputa(df, metodo = "knn", k = 3)
  expect_false(anyNA(r))
})

test_that("knn_imputa_cpp imputa valor proximo dos vizinhos", {
  X <- matrix(c(1, 1, 1.1, 1, 0.9, 1, 10, 10, NaN, 1), ncol = 2, byrow = TRUE)
  imp <- knn_imputa_cpp(X, 2L)
  # linha 5 (NaN, 1) deve ser imputada perto de 1 (vizinhos das linhas 1-3)
  expect_false(is.nan(imp[5, 1]))
  expect_lt(imp[5, 1], 5)
})

test_that("rnp_discretiza gera k classes", {
  d1 <- rnp_discretiza(mtcars$mpg, k = 3, metodo = "frequencia")
  expect_equal(nlevels(d1), 3L)
  d2 <- rnp_discretiza(mtcars$mpg, k = 4, metodo = "largura")
  expect_true(is.factor(d2))
})

test_that("rnp_dummy cria colunas indicadoras", {
  df <- data.frame(cor = c("a", "b", "a", "c"), x = 1:4)
  r <- rnp_dummy(df)
  expect_true(all(c("cor_a", "cor_b", "cor_c") %in% names(r)))
  expect_equal(r$cor_a, c(1L, 0L, 1L, 0L))
  # remover_primeira deixa k-1 colunas
  r2 <- rnp_dummy(df, remover_primeira = TRUE)
  expect_false("cor_a" %in% names(r2))
})
