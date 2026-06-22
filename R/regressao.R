#' Correlacao com teste de hipotese
#'
#' Calcula correlacao (Pearson/Spearman/Kendall) com p-valor e IC 95%
#' (para Pearson, via transformacao de Fisher).
#'
#' @param x,y Vetores numericos.
#' @param method String: \code{"pearson"}, \code{"spearman"}, \code{"kendall"}.
#' @param conf Nivel de confianca para o IC (apenas Pearson).
#' @param digits Inteiro.
#'
#' @return tibble com \code{correlacao}, \code{p_valor}, \code{ic_inf},
#'   \code{ic_sup}, \code{metodo}, \code{n}.
#'
#' @examples
#' rnp_correlacao_teste(mtcars$mpg, mtcars$wt)
#' rnp_correlacao_teste(mtcars$mpg, mtcars$hp, method = "spearman")
#' @export
rnp_correlacao_teste <- function(x, y,
                                 method = c("pearson", "spearman", "kendall"),
                                 conf = 0.95, digits = 4L) {
  abort_numerico(x, "x"); abort_numerico(y, "y")
  if (length(x) != length(y)) rlang::abort("x e y devem ter o mesmo comprimento.")
  abort_confianca(conf)
  method <- rlang::arg_match(method)
  # exact = NULL deixa o cor.test decidir; suprimimos apenas o aviso benigno
  # de "ties" em Spearman/Kendall (a estatistica assintotica segue valida).
  ct <- withCallingHandlers(
    stats::cor.test(x, y, method = method, conf.level = conf),
    warning = function(w) {
      if (grepl("ties|exact", conditionMessage(w))) invokeRestart("muffleWarning")
    }
  )
  ic_inf <- ic_sup <- NA_real_
  if (method == "pearson" && !is.null(ct$conf.int)) {
    ic_inf <- ct$conf.int[1L]
    ic_sup <- ct$conf.int[2L]
  } else if (method == "pearson") {
    z_atan <- atanh(ct$estimate)
    n <- length(x)
    zcrit <- stats::qnorm((1 + conf) / 2)
    se <- 1 / sqrt(n - 3)
    ic_inf <- tanh(z_atan - zcrit * se)
    ic_sup <- tanh(z_atan + zcrit * se)
  }
  tibble::tibble(
    correlacao = unname(ct$estimate),
    p_valor    = ct$p.value,
    ic_inf     = ic_inf,
    ic_sup     = ic_sup,
    metodo     = method,
    n          = length(x)
  ) |> dplyr::mutate(dplyr::across(where(is.numeric) & !n,
                                   ~ arredonda(.x, digits)))
}

#' Correlacao parcial
#'
#' Calcula correlacao parcial entre x e y, controlando por z (vetor ou matriz).
#'
#' @param x,y Vetores numericos.
#' @param z Vetor numerico ou matriz de variaveis de controle.
#' @param method String. Apenas \code{"pearson"} implementado por enquanto.
#' @param digits Inteiro.
#'
#' @return tibble com \code{correlacao_parcial}, \code{gl}, \code{p_valor}, \code{n}.
#'
#' @examples
#' rnp_correlacao_parcial(mtcars$mpg, mtcars$hp, mtcars$wt)
#' @export
rnp_correlacao_parcial <- function(x, y, z,
                                   method = "pearson", digits = 4L) {
  abort_numerico(x, "x"); abort_numerico(y, "y")
  if (is.null(z)) rlang::abort("{.arg z} (controle) e necessario.")
  Z <- as.matrix(z)
  if (!is.numeric(Z)) rlang::abort("{.arg z} deve ser numerico.")
  if (nrow(Z) != length(x)) rlang::abort("z deve ter mesmo comprimento de x.")
  d <- cbind(x = x, y = y, Z)
  d <- d[stats::complete.cases(d), , drop = FALSE]
  n <- nrow(d)
  if (n < 4L) rlang::abort("Correlacao parcial requer n >= 4 apos remover NA.")
  xz <- d[, 1L, drop = FALSE]
  yz <- d[, 2L, drop = FALSE]
  Zc <- d[, -(1:2), drop = FALSE]
  if (ncol(Zc) == 0L) {
    rx <- xz; ry <- yz
  } else {
    beta_x <- stats::lm.fit(Zc, xz)$coefficients
    beta_y <- stats::lm.fit(Zc, yz)$coefficients
    rx <- xz - Zc %*% beta_x
    ry <- yz - Zc %*% beta_y
  }
  r <- stats::cor(rx, ry)
  k <- ncol(Zc)
  gl <- n - 2 - k
  t_stat <- r * sqrt(gl / (1 - r^2))
  p <- 2 * stats::pt(-abs(t_stat), df = gl)
  tibble::tibble(
    correlacao_parcial = r,
    gl                 = gl,
    p_valor            = p,
    n                  = n
  ) |> dplyr::mutate(dplyr::across(where(is.numeric) & !gl & !n,
                                   ~ arredonda(.x, digits)))
}

#' Regressao linear (simples ou multipla) com saida tidy
#'
#' Ajusta modelo linear e retorna coeficientes, IC, R2, R2-ajustado,
#' estatistica-F global e residuos.
#'
#' @param formula Formula \code{y ~ x1 + x2 + ...}.
#' @param data data.frame.
#' @param conf Nivel de confianca para IC dos coeficientes.
#' @param digits Inteiro.
#'
#' @return lista com:
#'   * \code{coeficientes}: tibble com termo, estimativa, erro_padrao,
#'     estatistica_t, p_valor, ic_inf, ic_sup.
#'   * \code{modelo}: tibble com r2, r2_ajustado, f_statistic, f_pvalor,
#'     sigma, gl_residuos, nobs.
#'
#' @examples
#' rnp_regressao(mpg ~ wt + hp, mtcars)
#' @export
rnp_regressao <- function(formula, data, conf = 0.95, digits = 4L) {
  if (!inherits(formula, "formula")) rlang::abort("{.arg formula} deve ser formula.")
  if (!is.data.frame(data)) rlang::abort("{.arg data} deve ser data.frame.")
  abort_confianca(conf)
  fit <- stats::lm(formula, data = data, na.action = na.omit)
  sm <- summary(fit)
  coefs <- sm$coefficients
  ci <- stats::confint(fit, level = conf)
  coef_tbl <- tibble::tibble(
    termo        = rownames(coefs),
    estimativa   = coefs[, "Estimate"],
    erro_padrao  = coefs[, "Std. Error"],
    estatistica_t = coefs[, "t value"],
    p_valor      = coefs[, "Pr(>|t|)"],
    ic_inf       = ci[, 1L],
    ic_sup       = ci[, 2L]
  )
  fstat <- sm$fstatistic
  mod_tbl <- tibble::tibble(
    r2            = sm$r.squared,
    r2_ajustado   = sm$adj.r.squared,
    f_statistic   = unname(fstat[1L]),
    f_pvalor      = stats::pf(fstat[1L], fstat[2L], fstat[3L], lower.tail = FALSE),
    sigma         = sm$sigma,
    gl_residuos   = fit$df.residual,
    nobs          = stats::nobs(fit)
  )
  .rnp_lista(list(
    coeficientes = coef_tbl |>
      dplyr::mutate(dplyr::across(where(is.numeric),
                                  ~ arredonda(.x, digits))),
    modelo = mod_tbl |>
      dplyr::mutate(dplyr::across(where(is.numeric) & !gl_residuos & !nobs,
                                  ~ arredonda(.x, digits)))
  ), "Regressao linear")
}

#' Diagnosticos de regressao linear
#'
#' Retorna diagnosticos clássicos: hat, residuos studentizados, Cook's D,
#' DFBETAS, DFFITS, leverage, e testes de normalidade e homocedasticidade.
#'
#' @param modelo Objeto \code{lm} ou formula.
#' @param data data.frame (se modelo for formula).
#' @param digits Inteiro.
#'
#' @return lista:
#'   * \code{pontos}: tibble com indice, residuo, studentizado, hat, cooks_d,
#'     dffits.
#'   * \code{testes}: tibble com Shapiro-Wilk residuos, Breusch-Pagan
#'     (homocedasticidade), Durbin-Watson (autocorrelacao).
#'
#' @examples
#' fit <- lm(mpg ~ wt + hp, mtcars)
#' rnp_regressao_diagnosticos(fit)
#' @export
rnp_regressao_diagnosticos <- function(modelo, data = NULL, digits = 4L) {
  if (inherits(modelo, "formula")) modelo <- stats::lm(modelo, data = data)
  if (!inherits(modelo, "lm")) rlang::abort("{.arg modelo} deve ser lm ou formula.")
  infl <- stats::influence.measures(modelo)
  sm <- summary(modelo)
  n <- stats::nobs(modelo)
  p <- length(stats::coef(modelo))
  hat <- stats::hatvalues(modelo)
  resid_std <- stats::rstudent(modelo)
  cooks <- stats::cooks.distance(modelo)
  dffits_val <- stats::dffits(modelo)
  dfbetas_mat <- stats::dfbetas(modelo)
  pontos <- tibble::tibble(
    indice      = seq_len(n),
    residuo     = stats::residuals(modelo),
    studentizado = resid_std,
    hat         = hat,
    cooks_d     = cooks,
    dffits      = dffits_val,
    dplyr::bind_cols(as.data.frame(dfbetas_mat) |>
                       stats::setNames(paste0("dfbeta_", seq_len(ncol(dfbetas_mat)))))
  )
  pontos_flags <- pontos |>
    dplyr::mutate(
      leverage_alta = hat > 2 * p / n,
      cooks_alto    = cooks > 4 / n,
      outlier_t     = abs(studentizado) > 2
    )
  residuos <- stats::residuals(modelo)
  sw <- if (n >= 3L && n <= 5000L) stats::shapiro.test(residuos) else NULL
  bp <- tryCatch(
    {
      X <- model.matrix(modelo)
      aux <- stats::lm(residuos^2 ~ X - 1)
      R2 <- summary(aux)$r.squared
      stat <- n * R2
      p_val <- stats::pchisq(stat, df = ncol(X) - 1, lower.tail = FALSE)
      list(stat = stat, p = p_val)
    },
    error = function(e) list(stat = NA_real_, p = NA_real_)
  )
  dw <- tryCatch({
    e <- residuos
    denom <- sum(e^2)
    if (denom == 0) NA_real_
    else sum(diff(e)^2) / denom
  }, error = function(e) NA_real_)
  testes <- tibble::tibble(
    teste = c("shapiro-wilk (residuos)", "breusch-pagan", "durbin-watson"),
    estatistica = c(if (!is.null(sw)) unname(sw$statistic) else NA_real_,
                    bp$stat, dw),
    p_valor = c(if (!is.null(sw)) sw$p.value else NA_real_,
                bp$p, NA_real_),
    interpretacao = c(
      if (!is.null(sw) && sw$p.value < 0.05) "Rejeita normalidade" else "Normalidade nao rejeitada",
      if (bp$p < 0.05) "Heterocedasticidade" else "Homocedasticidade nao rejeitada",
      if (is.na(dw)) NA_character_ else
        if (dw < 1.5) "Possivel autocorrelacao positiva" else
          if (dw > 2.5) "Possivel autocorrelacao negativa" else "Sem autocorrelacao preocupante"
    )
  )
  .rnp_lista(list(
    pontos = pontos_flags |>
      dplyr::mutate(dplyr::across(where(is.numeric) & !indice,
                                  ~ arredonda(.x, digits))),
    testes = testes |>
      dplyr::mutate(dplyr::across(where(is.numeric),
                                  ~ arredonda(.x, digits)))
  ), "Diagnosticos de regressao")
}

#' Regressao logistica binaria
#'
#' Ajusta GLM binomial com saida tidy.
#'
#' @param formula Formula.
#' @param data data.frame.
#' @param conf Nivel de confianca.
#' @param digits Inteiro.
#'
#' @return lista:
#'   * \code{coeficientes}: tibble.
#'   * \code{modelo}: tibble com aic, deviance_residual, nobs, null_deviance,
#'     residual_deviance.
#'
#' @examples
#' rnp_logistic(am ~ mpg + wt, mtcars)
#' @export
rnp_logistic <- function(formula, data, conf = 0.95, digits = 4L) {
  if (!inherits(formula, "formula")) rlang::abort("{.arg formula} deve ser formula.")
  if (!is.data.frame(data)) rlang::abort("{.arg data} deve ser data.frame.")
  abort_confianca(conf)
  fit <- stats::glm(formula, data = data, family = stats::binomial(),
                    na.action = na.omit)
  sm <- summary(fit)
  coefs <- sm$coefficients
  ci <- stats::confint.default(fit, level = conf)
  coef_tbl <- tibble::tibble(
    termo        = rownames(coefs),
    estimativa   = coefs[, "Estimate"],
    erro_padrao  = coefs[, "Std. Error"],
    estatistica_z = coefs[, "z value"],
    p_valor      = coefs[, "Pr(>|z|)"],
    odds_ratio   = exp(coefs[, "Estimate"]),
    ic_inf       = exp(ci[, 1L]),
    ic_sup       = exp(ci[, 2L])
  )
  mod_tbl <- tibble::tibble(
    aic                 = fit$aic,
    null_deviance       = fit$null.deviance,
    residual_deviance   = fit$deviance,
    nobs                = stats::nobs(fit),
    df_residuos         = fit$df.residual
  )
  .rnp_lista(list(
    coeficientes = coef_tbl |>
      dplyr::mutate(dplyr::across(where(is.numeric),
                                  ~ arredonda(.x, digits))),
    modelo = mod_tbl |>
      dplyr::mutate(dplyr::across(where(is.numeric) & !df_residuos & !nobs,
                                  ~ arredonda(.x, digits)))
  ), "Regressao logistica")
}

#' Matriz de confusao
#'
#' @param observado Vetor de classes observadas (factor/character).
#' @param predito Vetor de classes preditas (mesmos niveis).
#' @param positivo String. Nivel da classe positiva (default primeiro nivel).
#' @param digits Inteiro.
#'
#' @return lista:
#'   * \code{matriz}: tabela.
#'   * \code{metricas}: tibble com sensibilidade, especificidade,
#'     precisao, f1, acuracia, prevalence, npv.
#'
#' @examples
#' obs <- sample(c("Sim", "Nao"), 100, TRUE)
#' pred <- sample(c("Sim", "Nao"), 100, TRUE)
#' rnp_matriz_confusao(obs, pred, positivo = "Sim")
#' @export
rnp_matriz_confusao <- function(observado, predito,
                                positivo = NULL, digits = 4L) {
  if (length(observado) != length(predito)) {
    rlang::abort("observado e predito devem ter o mesmo comprimento.")
  }
  obs <- as.factor(observado)
  pred <- factor(predito, levels = levels(obs))
  mat <- table(obs, pred)
  if (is.null(positivo)) positivo <- levels(obs)[1L]
  if (!positivo %in% levels(obs)) {
    rlang::abort("Classe positiva '{positivo}' nao encontrada.")
  }
  neg <- setdiff(levels(obs), positivo)
  TP <- mat[positivo, positivo]
  FP <- sum(mat[neg, positivo])
  FN <- sum(mat[positivo, neg])
  TN <- sum(mat[neg, neg])
  N <- sum(mat)
  safe_div <- function(a, b) if (b == 0) NA_real_ else a / b
  metricas <- tibble::tibble(
    sensibilidade = safe_div(TP, TP + FN),
    especificidade = safe_div(TN, TN + FP),
    precisao      = safe_div(TP, TP + FP),
    npv           = safe_div(TN, TN + FN),
    f1            = {
      p <- safe_div(TP, TP + FP)
      r <- safe_div(TP, TP + FN)
      if (is.na(p) || is.na(r) || (p + r) == 0) NA_real_
      else 2 * p * r / (p + r)
    },
    acuracia      = safe_div(TP + TN, N),
    prevalencia   = safe_div(TP + FN, N),
    n             = N
  )
  .rnp_lista(list(
    matriz = mat,
    metricas = metricas |>
      dplyr::mutate(dplyr::across(where(is.numeric) & !n,
                                  ~ arredonda(.x, digits)))
  ), "Matriz de confusao")
}

#' Curva ROC e AUC
#'
#' @param observado Vetor binario (0/1 ou factor 2 niveis).
#' @param escores Vetor numerico de probabilidades preditas.
#' @param positivo Nivel da classe positiva (default primeiro nivel).
#' @param digits Inteiro.
#'
#' @return lista:
#'   * \code{curva}: tibble com fpr, tpr, limiar.
#'   * \code{auc}: escalar.
#'
#' @examples
#' obs <- sample(0:1, 100, TRUE)
#' esc <- runif(100)
#' rnp_curva_roc(obs, esc, positivo = 1)
#' @export
rnp_curva_roc <- function(observado, escores, positivo = NULL, digits = 4L) {
  if (length(observado) != length(escores)) {
    rlang::abort("observado e escores devem ter o mesmo comprimento.")
  }
  if (!is.numeric(escores)) rlang::abort("{.arg escores} deve ser numerico.")
  if (!is.null(positivo)) {
    pos <- as.character(positivo)
    obs_pos <- as.character(observado) == pos
  } else {
    obs_pos <- as.logical(observado) | (as.character(observado) == levels(factor(observado))[1L])
  }
  ord <- order(escores, decreasing = TRUE)
  escores <- escores[ord]; obs_pos <- obs_pos[ord]
  P <- sum(obs_pos); N <- sum(!obs_pos)
  if (P == 0 || N == 0) rlang::abort("Conjunto deve ter ambas as classes.")
  TP <- cumsum(obs_pos)
  FP <- cumsum(!obs_pos)
  tpr <- TP / P
  fpr <- FP / N
  thr <- escores
  auc <- sum(diff(c(0, fpr, 1)) * (c(0, tpr, 1)[-1L] + c(0, tpr, 1)[-(length(tpr) + 1L)]) / 2)
  auc <- max(0, min(1, auc))
  .rnp_lista(list(
    curva = tibble::tibble(fpr = fpr, tpr = tpr, limiar = thr) |>
      dplyr::mutate(dplyr::across(where(is.numeric),
                                  ~ arredonda(.x, digits))),
    auc = arredonda(auc, digits)
  ), "Curva ROC")
}

#' AUC (somente)
#'
#' Atalho para \code{rnp_curva_roc(...)$auc}.
#'
#' @inheritParams rnp_curva_roc
#' @return Escalar numerico.
#' @export
rnp_roc_auc <- function(observado, escores, positivo = NULL, digits = 4L) {
  rnp_curva_roc(observado, escores, positivo = positivo, digits = digits)$auc
}
