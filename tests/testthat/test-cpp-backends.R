# Testes dos backends C++ (RcppArmadillo) contra a referencia do R.
# Tolerancia de 1e-8 conforme contrato do TODO.md (C-15).

tol <- 1e-8

test_that("dist_pairwise_cpp reproduz stats::dist", {
  set.seed(123)
  X <- matrix(rnorm(60), 15, 4)
  z <- matrix(0, 0, 0)
  expect_equal(dist_pairwise_cpp(X, 1L, 2, z),
               as.matrix(stats::dist(X, "euclidean")), tolerance = tol,
               ignore_attr = TRUE)
  expect_equal(dist_pairwise_cpp(X, 2L, 2, z),
               as.matrix(stats::dist(X, "manhattan")), tolerance = tol,
               ignore_attr = TRUE)
  expect_equal(dist_pairwise_cpp(X, 3L, 3, z),
               as.matrix(stats::dist(X, "minkowski", p = 3)), tolerance = tol,
               ignore_attr = TRUE)
  expect_equal(dist_pairwise_cpp(X, 4L, 2, z),
               as.matrix(stats::dist(X, "canberra")), tolerance = tol,
               ignore_attr = TRUE)
})

test_that("dist_pairwise_cpp Mahalanobis bate com mahalanobis()", {
  set.seed(1)
  X <- matrix(rnorm(50), 10, 5)
  S <- stats::cov(X)
  iS <- solve(S)
  d <- dist_pairwise_cpp(X, 5L, 2, iS)
  ref <- sqrt(stats::mahalanobis(X, X[1, ], S))
  expect_equal(d[1, ], ref, tolerance = tol)
})

test_that("cov_cpp e cor_cpp reproduzem stats", {
  set.seed(7)
  X <- matrix(rnorm(80), 20, 4)
  expect_equal(cov_cpp(X), stats::cov(X), tolerance = tol, ignore_attr = TRUE)
  expect_equal(cor_cpp(X, 1L), stats::cor(X), tolerance = tol, ignore_attr = TRUE)
  expect_equal(cor_cpp(X, 2L), stats::cor(X, method = "spearman"),
               tolerance = tol, ignore_attr = TRUE)
})

test_that("ols_fit_cpp reproduz lm (coeficientes e vcov)", {
  set.seed(42)
  X <- matrix(rnorm(100), 25, 4)
  y <- as.numeric(X %*% c(1, -2, 0.5, 3) + rnorm(25))
  Xd <- cbind(1, X)
  fit <- ols_fit_cpp(Xd, y)
  lmf <- stats::lm(y ~ X)
  expect_equal(as.numeric(fit$coeficientes), unname(coef(lmf)), tolerance = tol)
  expect_equal(fit$vcov, unname(vcov(lmf)), tolerance = tol, ignore_attr = TRUE)
  expect_equal(fit$sigma2, summary(lmf)$sigma^2, tolerance = tol)
})

test_that("momentos_cpp reproduz media/variancia e momentos", {
  set.seed(99)
  x <- rnorm(200, 5, 2)
  m <- momentos_cpp(x, 4L)
  expect_equal(m$media, mean(x), tolerance = tol)
  expect_equal(m$variancia, var(x), tolerance = tol)
  expect_equal(m$momentos_centrais[3], mean((x - mean(x))^2), tolerance = tol)
})

test_that("ecdf_cpp, ranks_cpp e binning_cpp batem com stats", {
  set.seed(3)
  x <- rnorm(50)
  pts <- seq(-2, 2, by = 0.25)
  expect_equal(as.numeric(ecdf_cpp(x, pts)), stats::ecdf(x)(pts), tolerance = tol)

  v <- c(3, 1, 1, 2, 5, 5, 5)
  expect_equal(as.numeric(ranks_cpp(v)), rank(v), tolerance = tol)

  br <- seq(min(x) - 1e-9, max(x) + 1e-9, length.out = 6)
  expect_equal(as.integer(binning_cpp(x, br)),
               as.integer(hist(x, breaks = br, plot = FALSE)$counts))
})
