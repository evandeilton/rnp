tol <- 1e-8

test_that("bootstrap_stat_cpp e jackknife_stat_cpp computam estatisticas certas", {
  set.seed(1)
  x <- rnorm(50)
  set.seed(10); r1 <- bootstrap_stat_cpp(x, 100L, 0L)  # media
  expect_length(as.numeric(r1), 100L)
  # jackknife da media: leave-one-out
  jk <- as.numeric(jackknife_stat_cpp(x, 0L))
  esperado <- vapply(seq_along(x), function(i) mean(x[-i]), numeric(1))
  expect_equal(jk, esperado, tolerance = tol)
})

test_that("rnp_bootstrap estima media com vies pequeno", {
  set.seed(1)
  b <- rnp_bootstrap(rnorm(200, 10, 2), "media", B = 2000)
  expect_equal(b$resumo$estimativa, b$resumo$media_boot, tolerance = 0.1)
  expect_lt(abs(b$resumo$vies), 0.1)
  expect_gt(b$resumo$erro_padrao, 0)
})

test_that("rnp_bootstrap aceita funcao customizada", {
  set.seed(1)
  b <- rnp_bootstrap(rnorm(100), function(x) diff(range(x)), B = 500)
  expect_length(b$replicas, 500L)
})

test_that("rnp_ic_bootstrap: metodos cobrem o parametro verdadeiro", {
  set.seed(1)
  x <- rnorm(300, 5, 1)
  for (tp in c("percentil", "normal", "basico", "bca")) {
    ic <- rnp_ic_bootstrap(x, "media", B = 1500, conf = 0.95, tipo = tp)
    expect_true(ic$limite_inferior < 5 && ic$limite_superior > 5,
                info = tp)
  }
})

test_that("rnp_jackknife reproduz EP da media", {
  set.seed(1)
  x <- rnorm(100, 0, 3)
  jk <- rnp_jackknife(x, "media")
  # EP jackknife da media ~ sd(x)/sqrt(n)
  expect_equal(jk$resumo$erro_padrao, stats::sd(x) / sqrt(length(x)),
               tolerance = 1e-3)  # funcao arredonda a 4 casas
})

test_that("rnp_bootstrap_parametrico funciona", {
  set.seed(1)
  b <- rnp_bootstrap_parametrico(rexp(150, 0.5), "exp", function(x) mean(x),
                                 B = 800)
  expect_equal(b$resumo$estimativa, 2, tolerance = 0.5)  # E[X]=1/0.5=2
  expect_gt(b$resumo$erro_padrao, 0)
})

test_that("rnp_teste_permutacao detecta diferenca real e nao-diferenca", {
  set.seed(1)
  p_dif <- rnp_teste_permutacao(rnorm(40, 0), rnorm(40, 1.0), B = 2000)$p_valor
  expect_lt(p_dif, 0.05)
  set.seed(2)
  p_igual <- rnp_teste_permutacao(rnorm(40, 0), rnorm(40, 0), B = 2000)$p_valor
  expect_gt(p_igual, 0.05)
})
