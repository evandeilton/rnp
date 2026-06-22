test_that("funcoes-modelo retornam objetos rnp_resultado sem quebrar a estrutura", {
  fit <- rnp_regressao(mpg ~ wt + hp, mtcars)
  expect_s3_class(fit, "rnp_resultado")
  expect_true(is.list(fit))                       # continua sendo lista
  expect_type(fit, "list")
  expect_true(is.data.frame(fit$coeficientes))    # acesso por $ intacto
  expect_named(fit, c("coeficientes", "modelo"))
})

test_that("print.rnp_resultado roda sem erro e e invisivel", {
  fit <- rnp_regressao(mpg ~ wt, mtcars)
  expect_no_error(suppressMessages(print(fit)))
  expect_invisible(suppressMessages(print(fit)))     # print devolve x invisivel
  # o titulo e gerado por cli (capturado via cli_fmt)
  saida <- cli::cli_fmt(print(fit))
  expect_true(any(grepl("Regressao linear", saida)))
})

test_that("tidy() devolve a tabela de coeficientes", {
  fit <- rnp_regressao(mpg ~ wt + hp, mtcars)
  td <- generics::tidy(fit)
  expect_s3_class(td, "tbl_df")
  expect_true("termo" %in% names(td))
  expect_equal(nrow(td), 3L)                      # intercepto + 2 preditores
  expect_identical(td, tibble::as_tibble(fit$coeficientes))
})

test_that("glance() devolve o resumo do modelo (uma linha)", {
  fit <- rnp_regressao(mpg ~ wt + hp, mtcars)
  gl <- generics::glance(fit)
  expect_s3_class(gl, "tbl_df")
  expect_equal(nrow(gl), 1L)
  expect_true("r2" %in% names(gl))
})

test_that("tidy/glance funcionam para outros modelos", {
  # GLM
  g <- rnp_glm(am ~ mpg, mtcars, familia = "binomial")
  expect_true("termo" %in% names(generics::tidy(g)))
  expect_equal(nrow(generics::glance(g)), 1L)
  # EMV (componentes estimativas / ajuste)
  set.seed(1); x <- rnorm(100, 5, 2)
  e <- rnp_emv(function(th) sum(dnorm(x, th[1], th[2], log = TRUE)),
               c(4, 1), c("media", "desvio"))
  expect_true("parametro" %in% names(generics::tidy(e)))
  # ajuste de distribuicao (qualidade vira glance)
  aj <- rnp_ajuste_distribuicao(x, "norm")
  expect_true("ks_estatistica" %in% names(generics::glance(aj)))
})

test_that("print lida com componentes de modelo, grafico e escalar", {
  ks <- rnp_ks_classificador(rbinom(200, 1, 0.3), runif(200), positivo = 1)
  expect_s3_class(ks, "rnp_resultado")
  expect_no_error(suppressMessages(print(ks)))
  expect_s3_class(ks$grafico, "ggplot")            # grafico preservado
  expect_true(is.numeric(ks$ks))                   # escalar preservado
})
