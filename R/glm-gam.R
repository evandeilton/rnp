# Modelos lineares generalizados e extensoes (FATIA 10):
# GLM unificado, binomial negativa, ordinal, modelo misto e aditivo (GAM).

#' Modelo linear generalizado (GLM)
#'
#' Ajuste unificado de GLM para as familias mais comuns, com saida tidy.
#'
#' @param formula Formula.
#' @param data data.frame.
#' @param familia String: `"gaussian"`, `"binomial"`, `"poisson"`, `"gamma"`,
#'   `"inverse_gaussian"`.
#' @param ligacao String opcional (funcao de ligacao). `NULL` usa o padrao
#'   canonico da familia.
#' @param conf Nivel de confianca para os IC (Wald).
#' @param digits Inteiro.
#'
#' @return Uma lista com `coeficientes` (tibble), `modelo` (AIC, deviance,
#'   dispersao, nobs) e `objeto` (`glm`).
#'
#' @examples
#' rnp_glm(am ~ mpg + wt, mtcars, familia = "binomial")$coeficientes
#' rnp_glm(carb ~ hp + wt, mtcars, familia = "poisson")$coeficientes
#' @family glm
#' @export
rnp_glm <- function(formula, data,
                    familia = c("gaussian", "binomial", "poisson", "gamma",
                                "inverse_gaussian"),
                    ligacao = NULL, conf = 0.95, digits = 4L) {
  familia <- rlang::arg_match(familia)
  abort_confianca(conf)
  fam <- switch(familia,
    gaussian         = stats::gaussian(ligacao %||% "identity"),
    binomial         = stats::binomial(ligacao %||% "logit"),
    poisson          = stats::poisson(ligacao %||% "log"),
    gamma            = stats::Gamma(ligacao %||% "log"),
    inverse_gaussian = stats::inverse.gaussian(ligacao %||% "1/mu^2"))
  fit <- stats::glm(formula, data = data, family = fam, na.action = stats::na.omit)
  co <- summary(fit)$coefficients
  z <- stats::qnorm((1 + conf) / 2)
  est <- co[, 1L]; se <- co[, 2L]
  coefs <- tibble::tibble(
    termo       = rownames(co),
    estimativa  = arredonda(unname(est), digits),
    erro_padrao = arredonda(unname(se), digits),
    estatistica = arredonda(unname(co[, 3L]), digits),
    p_valor     = arredonda(unname(co[, 4L]), digits),
    ic_inf      = arredonda(unname(est - z * se), digits),
    ic_sup      = arredonda(unname(est + z * se), digits))
  modelo <- tibble::tibble(
    familia           = familia,
    aic               = arredonda(fit$aic, digits),
    deviance          = arredonda(fit$deviance, digits),
    deviance_nula     = arredonda(fit$null.deviance, digits),
    dispersao         = arredonda(summary(fit)$dispersion, digits),
    nobs              = stats::nobs(fit))
  .rnp_lista(list(coeficientes = coefs, modelo = modelo, objeto = fit),
             "Modelo linear generalizado (GLM)")
}

#' Diagnostico de GLM
#'
#' Avalia ajuste e superdispersao de um GLM: residuos de deviance/Pearson,
#' dispersao estimada e teste de superdispersao.
#'
#' @param modelo Objeto `glm` ou saida de [rnp_glm()].
#' @param grafico Logico. Retorna grafico de residuos.
#' @param digits Inteiro.
#'
#' @return Uma lista com `testes` (tibble) e, se solicitado, `grafico` (`ggplot`).
#'
#' @examples
#' rnp_glm_diagnosticos(glm(carb ~ hp + wt, mtcars, family = poisson()))$testes
#' @family glm
#' @export
rnp_glm_diagnosticos <- function(modelo, grafico = FALSE, digits = 4L) {
  fit <- if (inherits(modelo, "glm")) modelo else modelo$objeto
  if (!inherits(fit, "glm")) rlang::abort("{.arg modelo} deve ser glm ou saida de rnp_glm().")
  rp <- stats::residuals(fit, type = "pearson")
  gl <- fit$df.residual
  dispersao <- sum(rp^2) / gl
  estat <- sum(rp^2)
  p_disp <- stats::pchisq(estat, gl, lower.tail = FALSE)
  testes <- tibble::tibble(
    medida = c("dispersao", "deviance/gl", "qui-quadrado de Pearson"),
    valor  = arredonda(c(dispersao, fit$deviance / gl, estat), digits),
    p_valor = c(NA_real_, NA_real_, arredonda(p_disp, digits)),
    interpretacao = c(
      if (dispersao > 1.5) "superdispersao" else "dispersao ok",
      NA_character_,
      if (p_disp < 0.05) "falta de ajuste/superdispersao" else "ajuste adequado"))
  out <- .rnp_lista(list(testes = testes), "Diagnostico de GLM")
  if (grafico) {
    d <- tibble::tibble(ajustado = stats::fitted(fit),
                        residuo = stats::residuals(fit, type = "deviance"))
    out$grafico <- ggplot2::ggplot(d, ggplot2::aes(.data$ajustado, .data$residuo)) +
      ggplot2::geom_point(color = rnp_paleta_rnp("rnp_qual", 1), alpha = 0.7) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      ggplot2::geom_smooth(se = FALSE, color = "grey40", linewidth = 0.6) +
      rnp_tema_rnp() +
      ggplot2::labs(title = "Residuos de deviance", x = "Ajustado", y = "Residuo")
  }
  out
}

#' Regressao binomial negativa
#'
#' Para contagens com superdispersao (variancia maior que a media), via
#' [MASS::glm.nb()].
#'
#' @param formula Formula.
#' @param data data.frame.
#' @param digits Inteiro.
#'
#' @return Uma lista com `coeficientes` (com razao de taxas `irr`), `theta`
#'   (parametro de dispersao) e `objeto`.
#'
#' @examples
#' rnp_binomial_negativa(Days ~ Sex + Age, MASS::quine)$theta
#' @family glm
#' @export
rnp_binomial_negativa <- function(formula, data, digits = 4L) {
  fit <- MASS::glm.nb(formula, data = data)
  co <- summary(fit)$coefficients
  coefs <- tibble::tibble(
    termo       = rownames(co),
    estimativa  = arredonda(unname(co[, 1L]), digits),
    erro_padrao = arredonda(unname(co[, 2L]), digits),
    p_valor     = arredonda(unname(co[, 4L]), digits),
    irr         = arredonda(unname(exp(co[, 1L])), digits))
  .rnp_lista(list(coeficientes = coefs,
                  theta = arredonda(fit$theta, digits),
                  aic = arredonda(stats::AIC(fit), digits),
                  objeto = fit),
             "Regressao binomial negativa")
}

#' Regressao ordinal (odds proporcionais)
#'
#' Modelo de odds proporcionais para respostas ordinais, via [MASS::polr()].
#'
#' @param formula Formula com resposta `factor` ordenado.
#' @param data data.frame.
#' @param pesos Vetor opcional de pesos (frequencias).
#' @param digits Inteiro.
#'
#' @return Uma lista com `coeficientes` (com `odds_ratio`), `limiares`
#'   (interceptos) e `objeto`.
#'
#' @examples
#' rnp_regressao_ordinal(Sat ~ Infl + Type, MASS::housing,
#'                       pesos = MASS::housing$Freq)$coeficientes
#' @family glm
#' @export
rnp_regressao_ordinal <- function(formula, data, pesos = NULL, digits = 4L) {
  args <- list(formula = formula, data = data, Hess = TRUE)
  if (!is.null(pesos)) args$weights <- pesos
  fit <- do.call(MASS::polr, args)
  ct <- summary(fit)$coefficients
  p <- length(stats::coef(fit))
  slopes <- ct[seq_len(p), , drop = FALSE]
  z <- slopes[, "Value"] / slopes[, "Std. Error"]
  coefs <- tibble::tibble(
    termo       = rownames(slopes),
    estimativa  = arredonda(unname(slopes[, "Value"]), digits),
    erro_padrao = arredonda(unname(slopes[, "Std. Error"]), digits),
    p_valor     = arredonda(unname(2 * stats::pnorm(-abs(z))), digits),
    odds_ratio  = arredonda(unname(exp(slopes[, "Value"])), digits))
  limiares <- tibble::tibble(
    limiar = names(fit$zeta),
    valor  = arredonda(unname(fit$zeta), digits))
  .rnp_lista(list(coeficientes = coefs, limiares = limiares, objeto = fit),
             "Regressao ordinal (odds proporcionais)")
}

#' Modelo linear de efeitos mistos
#'
#' Ajusta um modelo misto (efeitos fixos + aleatorios) via [nlme::lme()].
#'
#' @param fixos Formula dos efeitos fixos (ex.: `distance ~ age`).
#' @param data data.frame.
#' @param aleatorio Formula dos efeitos aleatorios (ex.: `~ 1 | Subject`).
#' @param digits Inteiro.
#'
#' @return Uma lista com `efeitos_fixos` (tibble), `variancia` (componentes de
#'   variancia + ICC) e `objeto`.
#'
#' @examples
#' rnp_modelo_misto(distance ~ age, nlme::Orthodont,
#'                  aleatorio = ~ 1 | Subject)$efeitos_fixos
#' @family glm
#' @export
rnp_modelo_misto <- function(fixos, data, aleatorio, digits = 4L) {
  fit <- nlme::lme(fixed = fixos, data = data, random = aleatorio, method = "REML")
  tt <- summary(fit)$tTable
  fixos_tb <- tibble::tibble(
    termo       = rownames(tt),
    estimativa  = arredonda(unname(tt[, "Value"]), digits),
    erro_padrao = arredonda(unname(tt[, "Std.Error"]), digits),
    gl          = unname(tt[, "DF"]),
    p_valor     = arredonda(unname(tt[, "p-value"]), digits))
  vc <- as.numeric(nlme::VarCorr(fit)[, "Variance"])
  var_aleat <- vc[1L]; var_resid <- vc[length(vc)]
  icc <- var_aleat / (var_aleat + var_resid)
  variancia <- tibble::tibble(
    componente = c("aleatorio", "residuo", "ICC"),
    variancia  = arredonda(c(var_aleat, var_resid, icc), digits))
  .rnp_lista(list(efeitos_fixos = fixos_tb, variancia = variancia, objeto = fit),
             "Modelo linear de efeitos mistos")
}

#' Modelo aditivo generalizado (GAM)
#'
#' Ajusta um GAM via [mgcv::gam()], permitindo termos suaves `s()`.
#'
#' @param formula Formula com termos `s()` (ex.: `y ~ s(x1) + x2`).
#' @param data data.frame.
#' @param familia String aceita por [rnp_glm()] (mapeada para a familia mgcv).
#' @param digits Inteiro.
#'
#' @return Uma lista com `parametricos` (tibble), `suaves` (tibble com `edf`,
#'   estatistica e p), `modelo` (r2 ajustado, deviance explicada, AIC) e `objeto`.
#'
#' @examples
#' set.seed(1)
#' d <- data.frame(x = runif(200)); d$y <- sin(2 * pi * d$x) + rnorm(200, 0, 0.3)
#' rnp_gam(y ~ s(x), d)$suaves
#' @family glm
#' @export
rnp_gam <- function(formula, data, familia = "gaussian", digits = 4L) {
  fam <- switch(familia,
    gaussian = stats::gaussian(), binomial = stats::binomial(),
    poisson = stats::poisson(), gamma = stats::Gamma(link = "log"),
    stats::gaussian())
  fit <- mgcv::gam(formula, data = data, family = fam)
  sm <- summary(fit)
  pt <- sm$p.table
  parametricos <- tibble::tibble(
    termo       = rownames(pt),
    estimativa  = arredonda(unname(pt[, 1L]), digits),
    erro_padrao = arredonda(unname(pt[, 2L]), digits),
    p_valor     = arredonda(unname(pt[, 4L]), digits))
  suaves <- if (nrow(sm$s.table) > 0) {
    st <- sm$s.table
    tibble::tibble(
      termo   = rownames(st),
      edf     = arredonda(unname(st[, "edf"]), digits),
      estatistica = arredonda(unname(st[, ncol(st) - 1L]), digits),
      p_valor = arredonda(unname(st[, ncol(st)]), digits))
  } else NULL
  modelo <- tibble::tibble(
    r2_ajustado       = arredonda(sm$r.sq, digits),
    deviance_explicada = arredonda(sm$dev.expl, digits),
    aic               = arredonda(stats::AIC(fit), digits))
  .rnp_lista(list(parametricos = parametricos, suaves = suaves,
                  modelo = modelo, objeto = fit),
             "Modelo aditivo generalizado (GAM)")
}

#' Grafico de efeitos parciais
#'
#' Exibe a contribuicao parcial de cada termo (efeitos parciais via
#' `predict(type = "terms")`), util para GLM e GAM.
#'
#' @param modelo Objeto `glm`/`gam`/`lm` ou saida das funcoes `rnp_glm`/`rnp_gam`.
#'
#' @return Objeto `ggplot` facetado por termo.
#'
#' @examples
#' set.seed(1)
#' d <- data.frame(x = runif(200)); d$y <- sin(2 * pi * d$x) + rnorm(200, 0, 0.3)
#' rnp_grafico_efeitos(rnp_gam(y ~ s(x), d))
#' @family glm
#' @export
rnp_grafico_efeitos <- function(modelo) {
  fit <- if (inherits(modelo, c("glm", "gam", "lm"))) modelo else modelo$objeto
  termos <- stats::predict(fit, type = "terms")
  mf <- stats::model.frame(fit)
  nomes <- colnames(termos)
  d <- purrr::map_dfr(nomes, function(tm) {
    var <- sub("^s\\((.*)\\)$", "\\1", tm)
    if (!var %in% names(mf)) return(NULL)
    if (!is.numeric(mf[[var]])) return(NULL)
    tibble::tibble(termo = tm, x = mf[[var]], efeito = termos[, tm])
  })
  ggplot2::ggplot(d, ggplot2::aes(.data$x, .data$efeito)) +
    ggplot2::geom_line(color = rnp_paleta_rnp("rnp_qual", 1), linewidth = 0.8) +
    ggplot2::facet_wrap(~ termo, scales = "free") +
    rnp_tema_rnp() +
    ggplot2::labs(title = "Efeitos parciais", x = NULL, y = "Contribuicao")
}
