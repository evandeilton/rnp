test_that("rnp_anova funciona", {
  res <- rnp_anova(mtcars$mpg, as.factor(mtcars$cyl))
  expect_type(res, "list")
  expect_true("anova" %in% names(res))
  expect_true("levene" %in% names(res))
  expect_s3_class(res$anova, "tbl_df")
})

test_that("rnp_anova com formula funciona", {
  res <- rnp_anova(mpg ~ factor(cyl), data = mtcars)
  expect_type(res, "list")
})

test_that("rnp_anova com post-hoc funciona", {
  res <- rnp_anova(mtcars$mpg, as.factor(mtcars$cyl), post_hoc = "tukey")
  expect_true(!is.null(res$post_hoc))
  expect_s3_class(res$post_hoc, "tbl_df")
})

test_that("rnp_tukey_hsd funciona", {
  fit <- aov(mpg ~ factor(cyl), mtcars)
  res <- rnp_tukey_hsd(fit)
  expect_s3_class(res, "tbl_df")
  expect_true("comparacao" %in% names(res))
})

test_that("rnp_anova_dois_fatores funciona", {
  res <- rnp_anova_dois_fatores(breaks ~ wool * tension, warpbreaks)
  expect_s3_class(res, "tbl_df")
  expect_true("fonte" %in% names(res))
})

test_that("rnp_kruskal funciona", {
  res <- rnp_kruskal(mtcars$mpg, as.factor(mtcars$cyl))
  expect_s3_class(res, "tbl_df")
  expect_equal(res$metodo, "kruskal-wallis")
})

test_that("rnp_mann_whitney funciona", {
  x <- rnorm(20, 5)
  y <- rnorm(20, 6)
  res <- rnp_mann_whitney(x, y)
  expect_s3_class(res, "tbl_df")
  expect_true("p_valor" %in% names(res))
})

test_that("rnp_wilcoxon funciona", {
  x <- rnorm(20, 5)
  y <- rnorm(20, 5.2)
  res <- rnp_wilcoxon(x, y)
  expect_s3_class(res, "tbl_df")
})

test_that("rnp_teste_friedman funciona", {
  set.seed(42)
  dados <- data.frame(
    valor = c(rnorm(10, 5), rnorm(10, 6), rnorm(10, 7)),
    grupo = rep(letters[1:3], each = 10),
    bloco = rep(1:10, 3)
  )
  res <- rnp_teste_friedman(valor ~ grupo | bloco, dados)
  expect_s3_class(res, "tbl_df")
  expect_equal(res$metodo, "friedman")
})

test_that("rnp_teste_kendall_w funciona", {
  mat <- matrix(sample(1:5, 30, TRUE), nrow = 6, ncol = 5)
  res <- rnp_teste_kendall_w(mat)
  expect_s3_class(res, "tbl_df")
  expect_equal(res$metodo, "kendall-w")
})

test_that("rnp_teste_mcnemar funciona", {
  x <- sample(0:1, 100, TRUE)
  y <- sample(0:1, 100, TRUE)
  res <- rnp_teste_mcnemar(x, y)
  expect_s3_class(res, "tbl_df")
  expect_equal(res$metodo, "mcnemar")
})
