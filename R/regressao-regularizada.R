# Regressao regularizada (FATIA 3): ridge, lasso e elastic net, com backends
# C++ (C-04). Predictores sao padronizados internamente; os coeficientes sao
# devolvidos na escala original.

# Extrai y e a matriz de preditores (sem intercepto) de formula + data.
.prep_regularizada <- function(formula, data) {
  mf <- stats::model.frame(formula, data, na.action = stats::na.omit)
  y <- as.numeric(stats::model.response(mf))
  X <- stats::model.matrix(formula, mf)
  inter <- which(colnames(X) == "(Intercept)")
  if (length(inter)) X <- X[, -inter, drop = FALSE]
  list(y = y, X = X, nomes = colnames(X))
}

# Padroniza colunas com media 0 e (1/n) sum z^2 = 1 (sd populacional).
.padroniza_pop <- function(X) {
  medias <- colMeans(X)
  sds <- sqrt(colMeans(sweep(X, 2L, medias)^2))
  sds[sds == 0] <- 1
  Z <- sweep(sweep(X, 2L, medias), 2L, sds, "/")
  list(Z = Z, medias = medias, sds = sds)
}

#' Regressao ridge (L2)
#'
#' Ajusta regressao linear com penalizacao L2 (ridge) via backend C++.
#' Os preditores sao padronizados e os coeficientes devolvidos na escala
#' original. O intercepto nao e penalizado.
#'
#' @param formula Formula `y ~ x1 + x2 + ...`.
#' @param data data.frame.
#' @param lambda Parametro de penalizacao (>= 0).
#' @param digits Inteiro.
#'
#' @return tibble com `termo` e `estimativa`.
#'
#' @examples
#' rnp_regressao_ridge(mpg ~ wt + hp + disp, mtcars, lambda = 1)
#' @family regressao
#' @export
rnp_regressao_ridge <- function(formula, data, lambda = 1, digits = 4L) {
  if (!inherits(formula, "formula")) rlang::abort("{.arg formula} deve ser formula.")
  if (lambda < 0) rlang::abort("{.arg lambda} deve ser >= 0.")
  pr <- .prep_regularizada(formula, data)
  pad <- .padroniza_pop(pr$X)
  n <- length(pr$y)
  Xz <- cbind(1, pad$Z)
  # ridge na escala padronizada (lambda escalado por n para consistencia)
  beta_z <- as.numeric(ridge_cpp(Xz, pr$y, lambda))
  # converte para a escala original
  slopes <- beta_z[-1L] / pad$sds
  intercepto <- beta_z[1L] - sum(slopes * pad$medias)
  tibble::tibble(
    termo      = c("(Intercept)", pr$nomes),
    estimativa = arredonda(unname(c(intercepto, slopes)), digits)
  )
}

# Nucleo comum de lasso/elastic net.
.ajusta_enet <- function(formula, data, lambda, alpha, digits,
                         max_iter = 10000L, tol = 1e-7) {
  pr <- .prep_regularizada(formula, data)
  pad <- .padroniza_pop(pr$X)
  ybar <- mean(pr$y)
  beta_z <- as.numeric(enet_cd_cpp(pad$Z, pr$y - ybar, lambda, alpha,
                                   as.integer(max_iter), tol))
  slopes <- beta_z / pad$sds
  intercepto <- ybar - sum(slopes * pad$medias)
  tibble::tibble(
    termo      = c("(Intercept)", pr$nomes),
    estimativa = arredonda(unname(c(intercepto, slopes)), digits)
  )
}

#' Regressao lasso (L1)
#'
#' Ajusta regressao com penalizacao L1 (lasso) por coordinate descent (C++),
#' produzindo selecao de variaveis (coeficientes exatamente zero).
#'
#' @inheritParams rnp_regressao_ridge
#'
#' @return tibble com `termo` e `estimativa`.
#'
#' @examples
#' rnp_regressao_lasso(mpg ~ wt + hp + disp + drat, mtcars, lambda = 0.5)
#' @family regressao
#' @export
rnp_regressao_lasso <- function(formula, data, lambda = 0.1, digits = 4L) {
  if (!inherits(formula, "formula")) rlang::abort("{.arg formula} deve ser formula.")
  if (lambda < 0) rlang::abort("{.arg lambda} deve ser >= 0.")
  .ajusta_enet(formula, data, lambda, alpha = 1, digits = digits)
}

#' Elastic net (L1 + L2)
#'
#' Combina penalizacoes lasso (L1) e ridge (L2) controladas por `alpha`
#' (`alpha = 1` lasso, `alpha = 0` ridge), por coordinate descent (C++).
#'
#' @inheritParams rnp_regressao_ridge
#' @param alpha Mistura L1/L2 em \[0, 1\].
#'
#' @return tibble com `termo` e `estimativa`.
#'
#' @examples
#' rnp_elastic_net(mpg ~ wt + hp + disp, mtcars, lambda = 0.3, alpha = 0.5)
#' @family regressao
#' @export
rnp_elastic_net <- function(formula, data, lambda = 0.1, alpha = 0.5,
                            digits = 4L) {
  if (!inherits(formula, "formula")) rlang::abort("{.arg formula} deve ser formula.")
  if (lambda < 0) rlang::abort("{.arg lambda} deve ser >= 0.")
  abort_proporcao(alpha, "alpha")
  .ajusta_enet(formula, data, lambda, alpha = alpha, digits = digits)
}
