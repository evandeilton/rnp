test_that("markov_npassos_cpp reproduz potencia de matriz", {
  P <- matrix(c(0.9, 0.1, 0.5, 0.5), 2, 2, byrow = TRUE)
  P3 <- P %*% P %*% P
  expect_equal(markov_npassos_cpp(P, 3L), P3, tolerance = 1e-10)
  expect_equal(markov_npassos_cpp(P, 0L), diag(2), tolerance = 1e-10)
})

test_that("markov_estacionaria_cpp resolve pi P = pi", {
  P <- matrix(c(0.9, 0.1, 0.5, 0.5), 2, 2, byrow = TRUE)
  pi_est <- as.numeric(markov_estacionaria_cpp(P))
  # estacionaria analitica: pi = (5/6, 1/6)
  expect_equal(sort(pi_est), sort(c(5 / 6, 1 / 6)), tolerance = 1e-8)
  expect_equal(as.numeric(pi_est %*% P), pi_est, tolerance = 1e-8)
  expect_equal(sum(pi_est), 1, tolerance = 1e-10)
})

test_that("rnp_cadeia_markov entrega distribuicoes coerentes", {
  P <- matrix(c(0.9, 0.1, 0.5, 0.5), 2, 2, byrow = TRUE)
  r <- rnp_cadeia_markov(P, estado_inicial = 1L, n = 50L)
  expect_equal(sum(r$distribuicao_n$probabilidade), 1, tolerance = 1e-3)
  # apos muitos passos converge para a estacionaria
  expect_equal(r$distribuicao_n$probabilidade, r$estacionaria$probabilidade,
               tolerance = 1e-2)
  expect_error(rnp_cadeia_markov(matrix(c(0.5, 0.6, 0.5, 0.5), 2, 2), n = 5))
})

test_that("rnp_passeio_aleatorio e processos retornam objetos certos", {
  expect_s3_class(rnp_passeio_aleatorio(200, 0.5), "ggplot")
  pp <- rnp_processo_poisson(taxa = 3, t_max = 5)
  expect_s3_class(pp, "tbl_df")
  expect_true(all(diff(pp$tempo_chegada) > 0))
})

test_that("rnp_simula_inversao gera amostra com media esperada", {
  set.seed(1)
  x <- rnp_simula_inversao(function(u) -log(1 - u) / 2, n = 5000)  # Exp(2)
  expect_length(x, 5000L)
  expect_equal(mean(x), 0.5, tolerance = 0.05)  # E[X] = 1/taxa = 0.5
})

test_that("rnp_simula_aceitacao_rejeicao amostra a densidade alvo", {
  set.seed(2)
  r <- rnp_simula_aceitacao_rejeicao(
    f = function(x) dbeta(x, 2, 2),
    gerador = function(n) runif(n),
    densidade_g = function(x) dunif(x),
    M = 1.6, n = 2000)
  expect_length(r$amostra, 2000L)
  expect_equal(mean(r$amostra), 0.5, tolerance = 0.05)  # E[Beta(2,2)] = 0.5
  expect_true(r$taxa_aceitacao > 0 && r$taxa_aceitacao <= 1)
  expect_error(rnp_simula_aceitacao_rejeicao(
    f = function(x) dbeta(x, 2, 2), gerador = function(n) runif(n),
    densidade_g = function(x) dunif(x), M = 0.5, n = 100))
})

test_that("rnp_monte_carlo estima integral conhecida", {
  r <- rnp_monte_carlo(function(x) x^2, limites = c(0, 1), n = 1e5)
  expect_equal(r$estimativa, 1 / 3, tolerance = 0.01)  # integral x^2 [0,1]
  expect_true(r$ic_inf < 1 / 3 && r$ic_sup > 1 / 3)
})
