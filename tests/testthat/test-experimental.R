test_that("rnp_ancova reproduz anova(lm)", {
  r <- rnp_ancova(mpg ~ factor(cyl) + wt, mtcars)
  ref <- anova(lm(mpg ~ factor(cyl) + wt, mtcars))
  expect_equal(r$estatistica_f[1], round(ref$`F value`[1], 4))
  expect_true("p_valor" %in% names(r))
})

test_that("rnp_dbc reproduz aov de dois fatores", {
  set.seed(1)
  resp <- rnorm(12, rep(c(5, 7, 9), 4))
  trat <- factor(rep(1:3, 4)); bloco <- factor(rep(1:4, each = 3))
  r <- rnp_dbc(resp, trat, bloco)
  ref <- summary(aov(resp ~ trat + bloco))[[1]]
  expect_equal(r$estatistica_f[1], round(ref$`F value`[1], 4))
  expect_equal(nrow(r), 3L)  # trat, bloco, residuo
})

test_that("rnp_anova_medidas_repetidas roda", {
  df <- data.frame(
    resp = c(10, 12, 14, 9, 11, 13, 8, 10, 12, 7, 9, 11),
    cond = factor(rep(c("A", "B", "C"), 4)),
    suj  = factor(rep(1:4, each = 3)))
  r <- rnp_anova_medidas_repetidas(resp ~ cond, df, sujeito = "suj")
  expect_s3_class(r, "tbl_df")
  expect_true("estatistica_f" %in% names(r))
})

test_that("rnp_contrastes testa contrastes ortogonais", {
  set.seed(1)
  y <- rnorm(30, rep(c(5, 6, 9), each = 10))
  g <- factor(rep(1:3, each = 10))
  C <- cbind(c(2, -1, -1), c(0, 1, -1))
  r <- rnp_contrastes(y, g, C)
  expect_equal(nrow(r), 2L)
  # primeiro contraste (g1 vs media de g2,g3) deve ser significativo
  expect_lt(r$p_valor[1], 0.05)
})
