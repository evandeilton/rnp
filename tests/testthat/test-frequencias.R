test_that("rnp_momentos bate com referencias do R", {
  set.seed(1)
  x <- rnorm(500, 10, 3)
  r <- rnp_momentos(x, ordem = 4L)
  expect_equal(r$resumo$media, round(mean(x), 4))
  expect_equal(r$resumo$variancia, round(var(x), 4))
  # assimetria de normal ~ 0
  expect_lt(abs(r$resumo$assimetria), 0.5)
  expect_lt(abs(r$resumo$curtose_excesso), 0.6)
  expect_equal(nrow(r$momentos), 3L)
})

test_that("rnp_momentos valida entradas", {
  expect_error(rnp_momentos(1))
  expect_error(rnp_momentos(rnorm(10), ordem = 1))
})

test_that("rnp_intervalo_classes calcula classes coerentes", {
  set.seed(2)
  x <- rnorm(1000)
  r <- rnp_intervalo_classes(x, regra = "sturges")
  expect_equal(r$n_classes, grDevices::nclass.Sturges(x))
  expect_length(r$limites, r$n_classes + 1L)
  expect_equal(rnp_intervalo_classes(1:100, regra = "fixa", k = 5)$n_classes, 5L)
})

test_that("rnp_tabela_frequencia soma 1 nas relativas", {
  r <- rnp_tabela_frequencia(mtcars$cyl)
  expect_equal(sum(r$fa), nrow(mtcars))
  expect_equal(tail(r$fr_acumulada, 1), 1)
  expect_true(all(diff(r$fa_acumulada) >= 0))
})

test_that("rnp_tabela_contingencia corrige a frequencia relativa (B-02)", {
  ct <- rnp_tabela_contingencia(mtcars$cyl, mtcars$gear, tipo = "fr")
  # soma de todas as celulas (sem Total) deve ser 1
  corpo <- as.matrix(ct[, setdiff(names(ct), c("categoria", "Total"))])
  expect_equal(sum(corpo), 1, tolerance = 1e-3)  # tolerancia p/ arredondamento das celulas
  # fr_linha: cada linha soma 1
  ctl <- rnp_tabela_contingencia(mtcars$cyl, mtcars$gear, tipo = "fr_linha")
  corpo_l <- as.matrix(ctl[, setdiff(names(ctl), c("categoria", "Total"))])
  expect_equal(unname(rowSums(corpo_l)), rep(1, nrow(corpo_l)), tolerance = 1e-3)
})

test_that("rnp_medias calcula as quatro medias corretamente", {
  x <- c(2, 4, 8)
  r <- rnp_medias(x)
  val <- setNames(r$valor, r$tipo)
  expect_equal(unname(val["aritmetica"]), round(mean(x), 4))
  expect_equal(unname(val["geometrica"]), round(prod(x)^(1 / 3), 4))
  expect_equal(unname(val["harmonica"]), round(3 / sum(1 / x), 4))
  expect_equal(unname(val["quadratica"]), round(sqrt(mean(x^2)), 4))
})

test_that("rnp_medias exige positivos para geometrica/harmonica", {
  expect_error(rnp_medias(c(-1, 2, 3), tipo = "geometrica"))
  # aritmetica aceita negativos
  expect_silent(rnp_medias(c(-1, 2, 3), tipo = "aritmetica"))
})

test_that("rnp_estrutura resume data.frame e vetor", {
  e <- rnp_estrutura(mtcars)
  expect_s3_class(e, "tbl_df")
  expect_equal(nrow(e), ncol(mtcars))
  expect_true(all(c("variavel", "classe", "n", "n_faltantes") %in% names(e)))
  ev <- rnp_estrutura(c(1, NA, 3))
  expect_equal(ev$n_faltantes, 1L)
})
