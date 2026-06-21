test_that("rnp_bayes reproduz o caso diagnostico classico", {
  # prevalencia 1%, sensibilidade 99%, 1-especificidade 5%
  r <- rnp_bayes(priori = c(doente = 0.01, sadio = 0.99),
                 verossimilhanca = c(0.99, 0.05))
  # P(doente | +) = .01*.99 / (.01*.99 + .99*.05)
  esperado <- 0.0099 / (0.0099 + 0.0495)
  expect_equal(r$posteriori[1], round(esperado, 4))
  expect_equal(sum(r$posteriori), 1, tolerance = 1e-3)
})

test_that("rnp_bayes avisa priori que nao soma 1", {
  expect_warning(rnp_bayes(c(0.5, 0.6), c(0.2, 0.3)))
})

test_that("rnp_distribuicao_conjunta calcula marginais e dependencia", {
  p <- matrix(c(0.1, 0.2, 0.2, 0.5), 2, 2,
              dimnames = list(c("0", "1"), c("0", "1")))
  r <- rnp_distribuicao_conjunta(p)
  expect_equal(sum(r$marginal_x$p), 1, tolerance = 1e-8)
  expect_equal(r$resumo$e_x, 0.7)  # P(X=1)=0.7
  # covariancia: E[XY]-E[X]E[Y] = 0.5 - 0.7*0.7
  expect_equal(r$resumo$cov_xy, round(0.5 - 0.49, 4))
})

test_that("rnp_distribuicao_conjunta valida soma 1", {
  expect_error(rnp_distribuicao_conjunta(matrix(c(0.1, 0.2, 0.2, 0.4), 2, 2)))
})

test_that("rnp_esperanca_condicional retorna E[Y|X] coerente", {
  p <- matrix(c(0.1, 0.2, 0.2, 0.5), 2, 2,
              dimnames = list(c("0", "1"), c("0", "1")))
  r <- rnp_esperanca_condicional(p)
  # X=0: P(Y=1|X=0) = 0.2/0.3 -> E[Y|X=0]=0.6667
  expect_equal(r$e_y_dado_x[1], round(0.2 / 0.3, 4))
  expect_equal(nrow(r), 2L)
})

test_that("rnp_lei_grandes_numeros e rnp_tcl_simulacao retornam ggplot", {
  expect_s3_class(
    rnp_lei_grandes_numeros(function(n) rbinom(n, 1, 0.3),
                            n_max = 200, media_teorica = 0.3),
    "ggplot")
  expect_s3_class(
    rnp_tcl_simulacao(function(n) rexp(n), n = 20, n_amostras = 200),
    "ggplot")
})

test_that("rnp_paleta_rnp interpola cores sem erro (ERROR corrigido)", {
  # a interpolacao emite aviso informativo intencional ao exceder a paleta base
  expect_length(suppressWarnings(rnp_paleta_rnp("rnp_seq", 10)), 10L)
  expect_length(rnp_paleta_rnp("rnp_qual", 5), 5L)
})
