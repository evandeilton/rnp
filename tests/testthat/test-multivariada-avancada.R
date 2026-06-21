test_that("silhueta_cpp bate com calculo de referencia", {
  set.seed(1)
  X <- rbind(matrix(rnorm(20, 0), 10, 2), matrix(rnorm(20, 5), 10, 2))
  D <- as.matrix(dist(X))
  cl <- c(rep(1L, 10), rep(2L, 10))
  s <- as.numeric(silhueta_cpp(D, cl))
  # referencia manual para a observacao 1
  a1 <- mean(D[1, cl == 1][-1])
  b1 <- mean(D[1, cl == 2])
  expect_equal(s[1], (b1 - a1) / max(a1, b1), tolerance = 1e-8)
  expect_true(mean(s) > 0.5)  # grupos bem separados
})

test_that("rnp_matriz_correlacao bate com cor() e cor.test()", {
  mc <- rnp_matriz_correlacao(mtcars[, c("mpg", "hp", "wt")])
  expect_equal(mc$matriz["mpg", "hp"], round(cor(mtcars$mpg, mtcars$hp), 4),
               tolerance = 1e-3)
  pv <- mc$tidy$p_valor[mc$tidy$var1 == "mpg" & mc$tidy$var2 == "hp"]
  expect_equal(pv, round(cor.test(mtcars$mpg, mtcars$hp)$p.value, 4),
               tolerance = 1e-4)
})

test_that("rnp_grafico_correlograma e dendrograma retornam ggplot", {
  expect_s3_class(rnp_grafico_correlograma(mtcars[, c("mpg", "hp", "wt")]), "ggplot")
  ch <- rnp_cluster_hierarquico(mtcars[, c("mpg", "hp", "wt")], k = 3)
  expect_s3_class(rnp_grafico_dendrograma(ch), "ggplot")
})

test_that("rnp_cluster_hierarquico concorda com hclust+cutree", {
  X <- scale(mtcars[, c("mpg", "hp", "wt")])
  ch <- rnp_cluster_hierarquico(mtcars[, c("mpg", "hp", "wt")], k = 3)
  ref <- cutree(hclust(dist(X)), k = 3)
  expect_equal(length(unique(ch$grupos$grupo)), 3L)
  expect_equal(ch$grupos$grupo, unname(ref))
})

test_that("rnp_silhueta media coerente com cluster real", {
  km <- rnp_kmeans(iris[, 1:4], k = 3)
  sil <- rnp_silhueta(iris[, 1:4], km$clusters$cluster)
  expect_true(sil$media > 0.3 && sil$media <= 1)
  expect_equal(nrow(sil$silhuetas), 150L)
})

test_that("rnp_kmedoids agrupa dados separados", {
  set.seed(1)
  X <- rbind(matrix(rnorm(40, 0), 20, 2), matrix(rnorm(40, 8), 20, 2))
  r <- rnp_kmedoids(as.data.frame(X), k = 2, escalar = FALSE)
  expect_length(r$medoides, 2L)
  # os dois grupos verdadeiros devem ficar separados
  tab <- table(r$clusters$cluster, rep(1:2, each = 20))
  expect_true(max(tab) == 20)
})

test_that("rnp_lda classifica iris com alta acuracia", {
  r <- rnp_lda(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
               iris)
  expect_gt(r$acuracia, 0.95)
  expect_length(r$predito, 150L)
})

test_that("rnp_hotelling detecta diferenca de medias", {
  r <- rnp_hotelling(iris[1:50, 1:4], iris[51:100, 1:4])
  expect_lt(r$p_valor, 0.05)
  expect_true(r$t2 > 0)
})

test_that("rnp_manova reporta Wilks e Pillai", {
  r <- rnp_manova(cbind(Sepal.Length, Petal.Length) ~ Species, iris)
  expect_equal(nrow(r), 2L)
  expect_true(all(r$p_valor < 0.05))
})

test_that("rnp_normalidade_multivariada roda (Mardia)", {
  r <- rnp_normalidade_multivariada(iris[, 1:4])
  expect_equal(nrow(r), 2L)
  expect_true(all(c("assimetria", "curtose") %in% r$medida))
})

test_that("rnp_correlacao_canonica retorna correlacoes em [0,1]", {
  r <- rnp_correlacao_canonica(iris[, 1:2], iris[, 3:4])
  expect_true(all(r$correlacao_canonica >= 0 & r$correlacao_canonica <= 1))
  expect_true(r$correlacao_canonica[1] > 0.9)  # forte associacao em iris
})

test_that("analise fatorial, correspondencia, biplot, dispersao-matriz", {
  af <- rnp_analise_fatorial(mtcars[, c("mpg", "disp", "hp", "drat", "wt", "qsec")],
                             n_fatores = 2)
  expect_true("variavel" %in% names(af$cargas))

  ca <- rnp_correspondencia(table(mtcars$cyl, mtcars$gear))
  expect_true("coord_linhas" %in% names(ca))

  p <- rnp_pca(mtcars[, c("mpg", "disp", "hp", "drat", "wt")])
  expect_s3_class(rnp_biplot(p), "ggplot")
  expect_s3_class(rnp_grafico_dispersao_matriz(iris[, 1:4]), "ggplot")
})
