test_that("rnp_combinacao calcula corretamente", {
  expect_equal(rnp_combinacao(10, 3), 120)
  expect_equal(rnp_combinacao(5, 2), 10)
})

test_that("rnp_arranjo calcula corretamente", {
  expect_equal(rnp_arranjo(10, 3), 720)
  expect_equal(rnp_arranjo(5, 2), 20)
})

test_that("rnp_permutacao calcula corretamente", {
  expect_equal(rnp_permutacao(5), 120)
  expect_equal(rnp_permutacao(7, elementos = c(2, 2, 3)), 210)
})

test_that("rnp_probabilidade_condicional funciona", {
  expect_equal(rnp_probabilidade_condicional(0.12, 0.30), 0.4)
})

test_that("rnp_distribuicao_binomial funciona", {
  d <- rnp_distribuicao_binomial("d", x = 3, size = 10, prob = 0.5)
  expect_true(is.numeric(d))
  p <- rnp_distribuicao_binomial("p", q = 5, size = 10, prob = 0.5)
  expect_true(p > 0 && p < 1)
})

test_that("rnp_distribuicao_poisson funciona", {
  d <- rnp_distribuicao_poisson("d", x = 2, lambda = 3)
  expect_true(is.numeric(d))
})

test_that("wrappers de distribuicoes validam parametros", {
  expect_error(rnp_distribuicao_binomial("d", x = 3, size = -1, prob = 0.5))
  expect_error(rnp_distribuicao_binomial("d", x = 3, size = 10, prob = 1.5))
  expect_error(rnp_distribuicao_poisson("d", x = 2, lambda = -1))
})
