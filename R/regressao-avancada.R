# Regressao avancada (FATIA 3): polinomial, ponderada, stepwise, Poisson, VIF,
# comparacao de modelos, predicao, diagnostico grafico, robusta, nao-linear,
# Box-Cox e multinomial.

#' Regressao polinomial
#'
#' Ajusta um polinomio de grau `grau` de um unico preditor, via backend C++
#' (OLS por QR).
#'
#' @param formula Formula `y ~ x` (um unico preditor).
#' @param data data.frame.
#' @param grau Inteiro. Grau do polinomio.
#' @param digits Inteiro.
#'
#' @return Uma lista com `coeficientes` (tibble) e `modelo` (tibble com `r2`,
#'   `r2_ajustado`, `sigma`, `nobs`).
#'
#' @examples
#' rnp_regressao_polinomial(dist ~ speed, cars, grau = 2)
#' @family regressao
#' @export
rnp_regressao_polinomial <- function(formula, data, grau = 2L, digits = 4L) {
  if (!inherits(formula, "formula")) rlang::abort("{.arg formula} deve ser formula.")
  abort_inteiro_pos(grau, "grau")
  mf <- stats::model.frame(formula, data, na.action = stats::na.omit)
  y <- as.numeric(stats::model.response(mf))
  preditores <- attr(stats::terms(formula), "term.labels")
  if (length(preditores) != 1L) rlang::abort("Use exatamente um preditor.")
  x <- as.numeric(mf[[preditores]])
  X <- outer(x, 0:grau, `^`)  # [1, x, x^2, ...]
  fit <- ols_fit_cpp(X, y)
  res <- as.numeric(fit$residuos)
  sstot <- sum((y - mean(y))^2)
  ssres <- sum(res^2)
  r2 <- 1 - ssres / sstot
  n <- length(y); p <- ncol(X)
  list(
    coeficientes = tibble::tibble(
      termo      = c("(Intercept)", paste0(preditores, "^", 1:grau)),
      estimativa = arredonda(as.numeric(fit$coeficientes), digits),
      erro_padrao = arredonda(sqrt(diag(as.matrix(fit$vcov))), digits)
    ),
    modelo = tibble::tibble(
      r2          = arredonda(r2, digits),
      r2_ajustado = arredonda(1 - (1 - r2) * (n - 1) / (n - p), digits),
      sigma       = arredonda(sqrt(fit$sigma2), digits),
      nobs        = n
    )
  )
}

#' Regressao linear ponderada (WLS)
#'
#' Minimos quadrados ponderados via backend C++.
#'
#' @param formula Formula.
#' @param data data.frame.
#' @param pesos Vetor de pesos (positivos), comprimento = nrow(data).
#' @param digits Inteiro.
#'
#' @return tibble com `termo`, `estimativa`, `erro_padrao`, `estatistica_t`,
#'   `p_valor`.
#'
#' @examples
#' rnp_regressao_ponderada(mpg ~ wt, mtcars, pesos = 1 / mtcars$wt)
#' @family regressao
#' @export
rnp_regressao_ponderada <- function(formula, data, pesos, digits = 4L) {
  if (!inherits(formula, "formula")) rlang::abort("{.arg formula} deve ser formula.")
  mf <- stats::model.frame(formula, data, na.action = stats::na.omit)
  y <- as.numeric(stats::model.response(mf))
  X <- stats::model.matrix(formula, mf)
  if (length(pesos) != length(y)) rlang::abort("{.arg pesos} deve ter comprimento = n.")
  if (any(pesos <= 0)) rlang::abort("{.arg pesos} devem ser positivos.")
  sw <- sqrt(pesos)
  fit <- ols_fit_cpp(X * sw, y * sw)
  est <- as.numeric(fit$coeficientes)
  se <- sqrt(diag(as.matrix(fit$vcov)))
  t <- est / se
  gl <- fit$gl
  tibble::tibble(
    termo         = colnames(X),
    estimativa    = arredonda(est, digits),
    erro_padrao   = arredonda(se, digits),
    estatistica_t = arredonda(t, digits),
    p_valor       = arredonda(2 * stats::pt(-abs(t), gl), digits)
  )
}

#' Selecao de variaveis (stepwise)
#'
#' Selecao automatica por AIC ou BIC usando [stats::step()].
#'
#' @param formula Formula do modelo completo.
#' @param data data.frame.
#' @param direcao String: `"both"`, `"backward"`, `"forward"`.
#' @param criterio String: `"AIC"` ou `"BIC"`.
#' @param digits Inteiro.
#'
#' @return Uma lista com `formula_final`, `coeficientes` (tibble) e `criterio`.
#'
#' @examples
#' rnp_regressao_stepwise(mpg ~ wt + hp + disp + drat + qsec, mtcars)
#' @family regressao
#' @export
rnp_regressao_stepwise <- function(formula, data,
                                   direcao = c("both", "backward", "forward"),
                                   criterio = c("AIC", "BIC"), digits = 4L) {
  direcao <- rlang::arg_match(direcao)
  criterio <- rlang::arg_match(criterio)
  # liga a formula ao ambiente atual para que step() encontre 'data' ao refitar
  environment(formula) <- environment()
  full <- stats::lm(formula, data = data)
  k <- if (criterio == "BIC") log(stats::nobs(full)) else 2
  fit <- stats::step(full, direction = direcao, k = k, trace = 0)
  co <- summary(fit)$coefficients
  list(
    formula_final = stats::formula(fit),
    coeficientes = tibble::tibble(
      termo       = rownames(co),
      estimativa  = arredonda(unname(co[, 1L]), digits),
      erro_padrao = arredonda(unname(co[, 2L]), digits),
      p_valor     = arredonda(unname(co[, 4L]), digits)
    ),
    criterio = criterio
  )
}

#' Regressao de Poisson (contagens)
#'
#' GLM Poisson com saida tidy e razao de taxas (IRR = exp(beta)).
#'
#' @param formula Formula.
#' @param data data.frame.
#' @param digits Inteiro.
#'
#' @return Uma lista com `coeficientes` (tibble com `irr`) e `modelo` (tibble).
#'
#' @examples
#' rnp_regressao_poisson(carb ~ hp + wt, mtcars)
#' @family regressao
#' @export
rnp_regressao_poisson <- function(formula, data, digits = 4L) {
  fit <- stats::glm(formula, data = data, family = stats::poisson(),
                    na.action = stats::na.omit)
  co <- summary(fit)$coefficients
  list(
    coeficientes = tibble::tibble(
      termo       = rownames(co),
      estimativa  = arredonda(unname(co[, 1L]), digits),
      erro_padrao = arredonda(unname(co[, 2L]), digits),
      p_valor     = arredonda(unname(co[, 4L]), digits),
      irr         = arredonda(unname(exp(co[, 1L])), digits)
    ),
    modelo = tibble::tibble(
      aic               = arredonda(fit$aic, digits),
      deviance          = arredonda(fit$deviance, digits),
      deviance_nula     = arredonda(fit$null.deviance, digits),
      nobs              = stats::nobs(fit)
    )
  )
}

#' Fator de inflacao de variancia (VIF)
#'
#' Mede a multicolinearidade: VIF_j = 1 / (1 - R_j^2), onde R_j^2 e o R-quadrado
#' da regressao do preditor j sobre os demais.
#'
#' @param modelo Objeto `lm`.
#' @param digits Inteiro.
#'
#' @return tibble com `termo`, `vif` e `interpretacao`.
#'
#' @examples
#' rnp_vif(lm(mpg ~ wt + hp + disp, mtcars))
#' @family regressao
#' @export
rnp_vif <- function(modelo, digits = 4L) {
  if (!inherits(modelo, "lm")) rlang::abort("{.arg modelo} deve ser lm.")
  X <- stats::model.matrix(modelo)
  inter <- which(colnames(X) == "(Intercept)")
  if (length(inter)) X <- X[, -inter, drop = FALSE]
  if (ncol(X) < 2L) rlang::abort("VIF requer >= 2 preditores.")
  vifs <- vapply(seq_len(ncol(X)), function(j) {
    r2 <- summary(stats::lm(X[, j] ~ X[, -j]))$r.squared
    1 / (1 - r2)
  }, numeric(1))
  tibble::tibble(
    termo = colnames(X),
    vif   = arredonda(vifs, digits),
    interpretacao = dplyr::case_when(
      vifs < 5  ~ "baixa",
      vifs < 10 ~ "moderada",
      TRUE      ~ "alta"
    )
  )
}

#' Comparacao de modelos aninhados (ANOVA)
#'
#' Compara dois ou mais modelos `lm`/`glm` aninhados via [stats::anova()].
#'
#' @param ... Modelos `lm`/`glm` na ordem crescente de complexidade.
#' @param digits Inteiro.
#'
#' @return tibble com a tabela de comparacao.
#'
#' @examples
#' m1 <- lm(mpg ~ wt, mtcars); m2 <- lm(mpg ~ wt + hp, mtcars)
#' rnp_anova_modelos(m1, m2)
#' @family regressao
#' @export
rnp_anova_modelos <- function(..., digits = 4L) {
  modelos <- list(...)
  if (length(modelos) < 2L) rlang::abort("Informe pelo menos 2 modelos.")
  tab <- as.data.frame(stats::anova(...))
  out <- tibble::as_tibble(tab, .name_repair = "minimal")
  out <- stats::setNames(out, make.names(names(out)))
  dplyr::mutate(out, dplyr::across(where(is.numeric), ~ arredonda(.x, digits)))
}

#' Predicao com intervalo
#'
#' Predicao de um `lm` com intervalo de confianca (media) ou de predicao
#' (nova observacao).
#'
#' @param modelo Objeto `lm`.
#' @param novos_dados data.frame com os preditores.
#' @param tipo String: `"confianca"` ou `"predicao"`.
#' @param conf Nivel de confianca.
#' @param digits Inteiro.
#'
#' @return tibble com `ajuste`, `limite_inferior`, `limite_superior`.
#'
#' @examples
#' fit <- lm(mpg ~ wt, mtcars)
#' rnp_predicao(fit, data.frame(wt = c(2, 3, 4)))
#' @family regressao
#' @export
rnp_predicao <- function(modelo, novos_dados, tipo = c("confianca", "predicao"),
                         conf = 0.95, digits = 4L) {
  if (!inherits(modelo, "lm")) rlang::abort("{.arg modelo} deve ser lm.")
  tipo <- rlang::arg_match(tipo)
  interval <- if (tipo == "confianca") "confidence" else "prediction"
  pred <- stats::predict(modelo, newdata = novos_dados, interval = interval,
                         level = conf)
  tibble::tibble(
    ajuste          = arredonda(pred[, "fit"], digits),
    limite_inferior = arredonda(pred[, "lwr"], digits),
    limite_superior = arredonda(pred[, "upr"], digits)
  )
}

#' Painel de diagnostico de residuos
#'
#' Gera os graficos classicos de diagnostico de um `lm`.
#'
#' @param modelo Objeto `lm`.
#'
#' @return Uma lista nomeada de objetos `ggplot`: `residuo_ajustado`, `qq`,
#'   `escala_locacao`, `residuo_leverage`.
#'
#' @examples
#' g <- rnp_grafico_residuos(lm(mpg ~ wt + hp, mtcars))
#' g$residuo_ajustado
#' @family regressao
#' @export
rnp_grafico_residuos <- function(modelo) {
  if (!inherits(modelo, "lm")) rlang::abort("{.arg modelo} deve ser lm.")
  d <- tibble::tibble(
    ajustado     = stats::fitted(modelo),
    residuo      = stats::residuals(modelo),
    padronizado  = stats::rstandard(modelo),
    leverage     = stats::hatvalues(modelo)
  )
  cor1 <- rnp_paleta_rnp("rnp_qual", 1)
  g1 <- ggplot2::ggplot(d, ggplot2::aes(.data$ajustado, .data$residuo)) +
    ggplot2::geom_point(color = cor1) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    ggplot2::geom_smooth(se = FALSE, color = "grey40", linewidth = 0.6) +
    rnp_tema_rnp() + ggplot2::labs(title = "Residuos vs Ajustados",
                                   x = "Ajustado", y = "Residuo")
  g2 <- ggplot2::ggplot(d, ggplot2::aes(sample = .data$padronizado)) +
    ggplot2::stat_qq(color = cor1) + ggplot2::stat_qq_line(color = "red") +
    rnp_tema_rnp() + ggplot2::labs(title = "Q-Q Normal dos residuos",
                                   x = "Teorico", y = "Padronizado")
  g3 <- ggplot2::ggplot(d, ggplot2::aes(.data$ajustado, sqrt(abs(.data$padronizado)))) +
    ggplot2::geom_point(color = cor1) +
    ggplot2::geom_smooth(se = FALSE, color = "grey40", linewidth = 0.6) +
    rnp_tema_rnp() + ggplot2::labs(title = "Escala-Locacao",
                                   x = "Ajustado", y = expression(sqrt(abs(padronizado))))
  g4 <- ggplot2::ggplot(d, ggplot2::aes(.data$leverage, .data$padronizado)) +
    ggplot2::geom_point(color = cor1) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    rnp_tema_rnp() + ggplot2::labs(title = "Residuos vs Leverage",
                                   x = "Leverage", y = "Padronizado")
  list(residuo_ajustado = g1, qq = g2, escala_locacao = g3,
       residuo_leverage = g4)
}

#' Regressao robusta (M-estimadores via IRLS)
#'
#' Ajuste robusto por minimos quadrados reponderados iterativamente, com
#' funcoes psi de Huber ou bisquare, via backend C++.
#'
#' @param formula Formula.
#' @param data data.frame.
#' @param metodo String: `"huber"` ou `"bisquare"`.
#' @param digits Inteiro.
#'
#' @return Uma lista com `coeficientes` (tibble) e `iteracoes`.
#'
#' @examples
#' rnp_regressao_robusta(mpg ~ wt + hp, mtcars)
#' @family regressao
#' @export
rnp_regressao_robusta <- function(formula, data,
                                  metodo = c("huber", "bisquare"), digits = 4L) {
  metodo <- rlang::arg_match(metodo)
  mf <- stats::model.frame(formula, data, na.action = stats::na.omit)
  y <- as.numeric(stats::model.response(mf))
  X <- stats::model.matrix(formula, mf)
  c_tuning <- if (metodo == "huber") 1.345 else 4.685
  fit <- irls_cpp(X, y, if (metodo == "huber") 0L else 1L, c_tuning, 100L, 1e-8)
  list(
    coeficientes = tibble::tibble(
      termo      = colnames(X),
      estimativa = arredonda(as.numeric(fit$coeficientes), digits)
    ),
    iteracoes = fit$iteracoes
  )
}

#' Regressao nao-linear
#'
#' Wrapper tidy de [stats::nls()].
#'
#' @param formula Formula nao-linear (ex.: `y ~ a * exp(b * x)`).
#' @param data data.frame.
#' @param inicio Lista de valores iniciais dos parametros.
#' @param digits Inteiro.
#'
#' @return tibble com `parametro`, `estimativa`, `erro_padrao`, `p_valor`.
#'
#' @examples
#' df <- data.frame(x = 1:10, y = 2 * exp(0.3 * (1:10)) + rnorm(10))
#' rnp_regressao_nao_linear(y ~ a * exp(b * x), df, inicio = list(a = 1, b = 0.1))
#' @family regressao
#' @export
rnp_regressao_nao_linear <- function(formula, data, inicio, digits = 4L) {
  fit <- stats::nls(formula, data = data, start = inicio)
  co <- summary(fit)$coefficients
  tibble::tibble(
    parametro   = rownames(co),
    estimativa  = arredonda(unname(co[, 1L]), digits),
    erro_padrao = arredonda(unname(co[, 2L]), digits),
    p_valor     = arredonda(unname(co[, 4L]), digits)
  )
}

#' Transformacao de Box-Cox
#'
#' Encontra o parametro lambda otimo da transformacao de Box-Cox por
#' verossimilhanca perfilada e devolve o grafico do perfil.
#'
#' @param formula Formula do modelo linear.
#' @param data data.frame (resposta deve ser positiva).
#' @param lambda_seq Sequencia de valores de lambda a avaliar.
#' @param digits Inteiro.
#'
#' @return Uma lista com `lambda` (otimo), `tabela` (tibble) e `grafico`
#'   (`ggplot`).
#'
#' @examples
#' rnp_box_cox(mpg ~ wt + hp, mtcars)$lambda
#' @family regressao
#' @export
rnp_box_cox <- function(formula, data, lambda_seq = seq(-2, 2, by = 0.1),
                        digits = 4L) {
  mf <- stats::model.frame(formula, data, na.action = stats::na.omit)
  y <- as.numeric(stats::model.response(mf))
  if (any(y <= 0)) rlang::abort("Box-Cox requer resposta estritamente positiva.")
  X <- stats::model.matrix(formula, mf)
  n <- length(y)
  geo <- exp(mean(log(y)))
  loglik <- vapply(lambda_seq, function(lam) {
    z <- if (abs(lam) < 1e-8) log(y) * geo else (y^lam - 1) / (lam * geo^(lam - 1))
    res <- stats::lm.fit(X, z)$residuals
    -n / 2 * log(sum(res^2) / n)
  }, numeric(1))
  lam_opt <- lambda_seq[which.max(loglik)]
  dados <- tibble::tibble(lambda = lambda_seq, log_veross = loglik)
  g <- ggplot2::ggplot(dados, ggplot2::aes(.data$lambda, .data$log_veross)) +
    ggplot2::geom_line(color = rnp_paleta_rnp("rnp_qual", 1), linewidth = 1) +
    ggplot2::geom_vline(xintercept = lam_opt, linetype = "dashed", color = "red") +
    rnp_tema_rnp() +
    ggplot2::labs(title = "Perfil de verossimilhanca Box-Cox",
                  x = expression(lambda), y = "log-verossimilhanca")
  list(
    lambda  = arredonda(lam_opt, digits),
    tabela  = dplyr::mutate(dados, dplyr::across(where(is.numeric), ~ arredonda(.x, digits))),
    grafico = g
  )
}

#' Regressao logistica multinomial
#'
#' Ajusta um modelo logistico multinomial (softmax) por maxima verossimilhanca
#' via [stats::optim()], com a primeira categoria como referencia. Implementacao
#' propria (sem dependencias externas).
#'
#' @param formula Formula `y ~ x1 + ...` (y fator com >= 3 niveis).
#' @param data data.frame.
#' @param digits Inteiro.
#'
#' @return Uma lista com `coeficientes` (tibble: `classe`, `termo`,
#'   `estimativa`) e `referencia`.
#'
#' @examples
#' rnp_regressao_multinomial(Species ~ Sepal.Length + Petal.Length, iris)
#' @family regressao
#' @export
rnp_regressao_multinomial <- function(formula, data, digits = 4L) {
  mf <- stats::model.frame(formula, data, na.action = stats::na.omit)
  y <- stats::model.response(mf)
  if (!is.factor(y)) y <- as.factor(y)
  niveis <- levels(y)
  K <- length(niveis)
  if (K < 3L) rlang::abort("Multinomial requer resposta com >= 3 categorias.")
  X <- stats::model.matrix(formula, mf)
  n <- nrow(X); p <- ncol(X)
  Y <- stats::model.matrix(~ y - 1)  # indicadores n x K
  nll <- function(theta) {
    B <- matrix(theta, nrow = K - 1L, ncol = p, byrow = TRUE)
    eta <- cbind(0, X %*% t(B))                 # n x K (1a classe = referencia)
    m <- apply(eta, 1L, max)
    logsum <- m + log(rowSums(exp(eta - m)))
    logp <- eta - logsum
    -sum(Y * logp)
  }
  op <- stats::optim(rep(0, (K - 1L) * p), nll, method = "BFGS")
  B <- matrix(op$par, nrow = K - 1L, ncol = p, byrow = TRUE)
  coefs <- tibble::tibble(
    classe     = rep(niveis[-1L], each = p),
    termo      = rep(colnames(X), times = K - 1L),
    estimativa = arredonda(as.numeric(t(B)), digits)
  )
  list(coeficientes = coefs, referencia = niveis[1L])
}
