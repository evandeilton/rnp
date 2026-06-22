# Analise de sobrevivencia (FATIA 8). Apoia-se no pacote survival (recomendado).
# Convencao: 'evento' = 1 quando o evento de interesse ocorreu, 0 quando censurado.

# Extrai a mediana de sobrevida (e IC) de um objeto survfit, com ou sem estratos.
.km_mediana <- function(fit, digits) {
  tb <- summary(fit)$table
  if (is.null(dim(tb))) tb <- t(as.matrix(tb))
  cn <- colnames(tb)
  lcl <- cn[grepl("LCL", cn)][1L]
  ucl <- cn[grepl("UCL", cn)][1L]
  grupos <- rownames(tb)
  if (is.null(grupos)) grupos <- "todos"
  tibble::tibble(
    grupo    = sub("^.*=", "", grupos),
    n        = as.integer(unname(tb[, "records"])),
    eventos  = as.integer(unname(tb[, "events"])),
    mediana  = arredonda(unname(tb[, "median"]), digits),
    ic_inf   = arredonda(unname(tb[, lcl]), digits),
    ic_sup   = arredonda(unname(tb[, ucl]), digits)
  )
}

#' Estimador de Kaplan-Meier
#'
#' Estima a funcao de sobrevivencia S(t) = P(T > t) pelo metodo de
#' Kaplan-Meier, com intervalo de confianca, para uma ou varias curvas.
#'
#' @param tempo Vetor de tempos de seguimento.
#' @param evento Vetor 0/1 (1 = evento ocorreu; 0 = censura).
#' @param grupo Fator opcional para curvas estratificadas.
#' @param conf Nivel de confianca.
#' @param digits Inteiro.
#'
#' @return Uma lista com `tabela` (passos da curva), `mediana` (sobrevida mediana
#'   por grupo) e `modelo` (objeto `survfit`, para [rnp_grafico_sobrevivencia()]).
#'
#' @examples
#' km <- rnp_kaplan_meier(survival::lung$time, survival::lung$status == 2,
#'                        grupo = survival::lung$sex)
#' km$mediana
#' @family sobrevivencia
#' @export
rnp_kaplan_meier <- function(tempo, evento, grupo = NULL, conf = 0.95,
                             digits = 4L) {
  abort_numerico(tempo, "tempo")
  abort_confianca(conf)
  ev <- as.integer(evento)
  if (is.null(grupo)) {
    d <- data.frame(tempo = tempo, evento = ev)
    fit <- survival::survfit(survival::Surv(tempo, evento) ~ 1, data = d,
                             conf.int = conf)
    rotulos <- rep("todos", length(fit$time))
  } else {
    d <- data.frame(tempo = tempo, evento = ev, grupo = as.factor(grupo))
    fit <- survival::survfit(survival::Surv(tempo, evento) ~ grupo, data = d,
                             conf.int = conf)
    rotulos <- rep(sub("^grupo=", "", names(fit$strata)), fit$strata)
  }
  tabela <- tibble::tibble(
    grupo         = rotulos,
    tempo         = fit$time,
    n_risco       = fit$n.risk,
    n_evento      = fit$n.event,
    sobrevivencia = arredonda(fit$surv, digits),
    ic_inf        = arredonda(fit$lower, digits),
    ic_sup        = arredonda(fit$upper, digits)
  )
  .rnp_lista(list(tabela = tabela, mediana = .km_mediana(fit, digits),
                  modelo = fit), "Estimador de Kaplan-Meier")
}

#' Curva de Kaplan-Meier (grafico)
#'
#' @param km Saida de [rnp_kaplan_meier()] (ou objeto `survfit`).
#' @param intervalo Logico. Exibe a faixa de confianca.
#' @param titulo Titulo opcional.
#'
#' @return Objeto `ggplot`.
#'
#' @examples
#' km <- rnp_kaplan_meier(survival::lung$time, survival::lung$status == 2,
#'                        grupo = survival::lung$sex)
#' rnp_grafico_sobrevivencia(km)
#' @family sobrevivencia
#' @export
rnp_grafico_sobrevivencia <- function(km, intervalo = TRUE, titulo = NULL) {
  tab <- if (is.list(km) && !is.null(km$tabela)) km$tabela else
    rnp_kaplan_meier(km$time, km$n.event > 0)$tabela
  # acrescenta o ponto inicial (t = 0, S = 1) por grupo
  ini <- dplyr::distinct(tab, .data$grupo)
  ini$tempo <- 0; ini$sobrevivencia <- 1; ini$ic_inf <- 1; ini$ic_sup <- 1
  ini$n_risco <- NA_integer_; ini$n_evento <- NA_integer_
  d <- dplyr::arrange(dplyr::bind_rows(ini[names(tab)], tab),
                      .data$grupo, .data$tempo)
  um_grupo <- length(unique(d$grupo)) == 1L
  g <- ggplot2::ggplot(d, ggplot2::aes(x = .data$tempo, y = .data$sobrevivencia,
                                       color = .data$grupo, fill = .data$grupo))
  if (intervalo) {
    g <- g + ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$ic_inf,
                                               ymax = .data$ic_sup),
                                  alpha = 0.15, color = NA)
  }
  g <- g + ggplot2::geom_step(linewidth = 0.8) +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    rnp_tema_rnp() +
    ggplot2::labs(title = titulo %||% "Curva de Kaplan-Meier",
                  x = "Tempo", y = "Sobrevivencia S(t)",
                  color = "Grupo", fill = "Grupo")
  if (um_grupo) g <- g + ggplot2::guides(color = "none", fill = "none")
  g
}

#' Teste log-rank
#'
#' Compara curvas de sobrevivencia entre grupos. Com `rho = 0` obtem-se o teste
#' log-rank; com `rho = 1`, o teste de Gehan-Wilcoxon (peso de Peto).
#'
#' @param tempo Vetor de tempos.
#' @param evento Vetor 0/1.
#' @param grupo Fator de grupos.
#' @param rho Peso (0 = log-rank; 1 = Wilcoxon).
#' @param digits Inteiro.
#'
#' @return tibble com `estatistica`, `gl`, `p_valor`, `metodo`.
#'
#' @examples
#' rnp_log_rank(survival::lung$time, survival::lung$status == 2,
#'              survival::lung$sex)
#' @family sobrevivencia
#' @export
rnp_log_rank <- function(tempo, evento, grupo, rho = 0, digits = 4L) {
  abort_numerico(tempo, "tempo")
  d <- data.frame(tempo = tempo, evento = as.integer(evento),
                  grupo = as.factor(grupo))
  sd <- survival::survdiff(survival::Surv(tempo, evento) ~ grupo, data = d,
                           rho = rho)
  gl <- length(sd$n) - 1L
  tibble::tibble(
    estatistica = arredonda(sd$chisq, digits),
    gl          = gl,
    p_valor     = arredonda(stats::pchisq(sd$chisq, gl, lower.tail = FALSE), digits),
    metodo      = if (rho == 0) "log-rank" else "gehan-wilcoxon"
  )
}

#' Risco acumulado de Nelson-Aalen
#'
#' Estima a funcao de risco acumulado H(t) pelo estimador de Nelson-Aalen.
#'
#' @param tempo Vetor de tempos.
#' @param evento Vetor 0/1.
#' @param digits Inteiro.
#'
#' @return tibble com `tempo`, `n_risco`, `n_evento`, `risco_acumulado`, `ep`.
#'
#' @examples
#' head(rnp_nelson_aalen(survival::lung$time, survival::lung$status == 2))
#' @family sobrevivencia
#' @export
rnp_nelson_aalen <- function(tempo, evento, digits = 4L) {
  abort_numerico(tempo, "tempo")
  d <- data.frame(tempo = tempo, evento = as.integer(evento))
  fit <- survival::survfit(survival::Surv(tempo, evento) ~ 1, data = d)
  tibble::tibble(
    tempo           = fit$time,
    n_risco         = fit$n.risk,
    n_evento        = fit$n.event,
    risco_acumulado = arredonda(fit$cumhaz, digits),
    ep              = arredonda(fit$std.chaz, digits)
  )
}

#' Modelo de Cox de riscos proporcionais
#'
#' Ajusta o modelo semiparametrico de Cox. A formula deve ter
#' `survival::Surv(tempo, evento)` no lado esquerdo.
#'
#' @param formula Formula, ex.: `Surv(time, status) ~ age + sex`.
#' @param data data.frame.
#' @param conf Nivel de confianca.
#' @param digits Inteiro.
#'
#' @return Uma lista com `coeficientes` (tibble com `hazard_ratio`, IC, p),
#'   `modelo` (concordancia, AIC, testes globais) e `objeto` (`coxph`).
#'
#' @examples
#' rnp_cox(survival::Surv(time, status) ~ age + sex + ph.ecog,
#'         data = survival::lung)$coeficientes
#' @family sobrevivencia
#' @export
rnp_cox <- function(formula, data, conf = 0.95, digits = 4L) {
  if (!inherits(formula, "formula")) rlang::abort("{.arg formula} deve ser formula.")
  abort_confianca(conf)
  fit <- survival::coxph(formula, data = data, model = TRUE)
  sm <- summary(fit, conf.int = conf)
  co <- sm$coefficients
  ci <- sm$conf.int
  coefs <- tibble::tibble(
    termo        = rownames(co),
    coef         = arredonda(unname(co[, "coef"]), digits),
    hazard_ratio = arredonda(unname(co[, "exp(coef)"]), digits),
    erro_padrao  = arredonda(unname(co[, "se(coef)"]), digits),
    z            = arredonda(unname(co[, "z"]), digits),
    p_valor      = arredonda(unname(co[, ncol(co)]), digits),
    ic_inf       = arredonda(unname(ci[, 3L]), digits),
    ic_sup       = arredonda(unname(ci[, 4L]), digits)
  )
  modelo <- tibble::tibble(
    concordancia    = arredonda(unname(sm$concordance[1L]), digits),
    aic             = arredonda(stats::AIC(fit), digits),
    n               = sm$n,
    eventos         = sm$nevent,
    p_razao_veross  = arredonda(sm$logtest["pvalue"], digits)
  )
  .rnp_lista(list(coeficientes = coefs, modelo = modelo, objeto = fit),
             "Modelo de Cox de riscos proporcionais")
}

#' Diagnostico da hipotese de riscos proporcionais
#'
#' Testa a hipotese de proporcionalidade dos riscos (Schoenfeld, `cox.zph`) e
#' opcionalmente exibe os residuos de Schoenfeld.
#'
#' @param modelo Objeto `coxph` ou saida de [rnp_cox()].
#' @param grafico Logico. Retorna tambem o grafico dos residuos.
#' @param digits Inteiro.
#'
#' @return Uma lista com `teste` (tibble por covariavel + global) e, se
#'   solicitado, `grafico` (`ggplot`).
#'
#' @examples
#' fit <- rnp_cox(survival::Surv(time, status) ~ age + sex, survival::lung)
#' rnp_cox_diagnosticos(fit)$teste
#' @family sobrevivencia
#' @export
rnp_cox_diagnosticos <- function(modelo, grafico = FALSE, digits = 4L) {
  fit <- if (inherits(modelo, "coxph")) modelo else modelo$objeto
  if (!inherits(fit, "coxph")) rlang::abort("{.arg modelo} deve ser coxph ou saida de rnp_cox().")
  zph <- survival::cox.zph(fit)
  tab <- as.data.frame(zph$table)
  teste <- tibble::tibble(
    termo       = rownames(tab),
    chisq       = arredonda(tab$chisq, digits),
    gl          = tab$df,
    p_valor     = arredonda(tab$p, digits),
    interpretacao = ifelse(tab$p < 0.05, "viola PH", "PH nao rejeitada")
  )
  out <- .rnp_lista(list(teste = teste),
                    "Diagnostico de riscos proporcionais (Schoenfeld)")
  if (grafico) {
    res <- as.data.frame(zph$y)
    res$tempo <- zph$x
    longo <- tidyr::pivot_longer(res, cols = -"tempo",
                                 names_to = "covariavel", values_to = "residuo")
    out$grafico <- ggplot2::ggplot(longo, ggplot2::aes(.data$tempo, .data$residuo)) +
      ggplot2::geom_point(alpha = 0.5, color = rnp_paleta_rnp("rnp_qual", 1)) +
      ggplot2::geom_smooth(se = TRUE, color = "red", linewidth = 0.7) +
      ggplot2::facet_wrap(~ covariavel, scales = "free_y") +
      rnp_tema_rnp() +
      ggplot2::labs(title = "Residuos de Schoenfeld", x = "Tempo", y = "Residuo")
  }
  out
}

#' Modelo parametrico de sobrevivencia (AFT)
#'
#' Ajusta um modelo de tempo de falha acelerado (`survreg`) com distribuicao
#' configuravel.
#'
#' @param formula Formula com `Surv()` no lado esquerdo.
#' @param data data.frame.
#' @param dist String: `"weibull"`, `"exponential"`, `"lognormal"`,
#'   `"loglogistic"`.
#' @param digits Inteiro.
#'
#' @return Uma lista com `coeficientes` (tibble), `escala`, `aic` e `objeto`.
#'
#' @examples
#' rnp_sobrevivencia_parametrica(survival::Surv(time, status) ~ age + sex,
#'                               survival::lung, dist = "weibull")$coeficientes
#' @family sobrevivencia
#' @export
rnp_sobrevivencia_parametrica <- function(formula, data,
                                          dist = c("weibull", "exponential",
                                                   "lognormal", "loglogistic"),
                                          digits = 4L) {
  dist <- rlang::arg_match(dist)
  fit <- survival::survreg(formula, data = data, dist = dist)
  sm <- summary(fit)
  co <- sm$table
  coefs <- tibble::tibble(
    termo       = rownames(co),
    estimativa  = arredonda(unname(co[, "Value"]), digits),
    erro_padrao = arredonda(unname(co[, "Std. Error"]), digits),
    z           = arredonda(unname(co[, "z"]), digits),
    p_valor     = arredonda(unname(co[, "p"]), digits)
  )
  .rnp_lista(list(
    coeficientes = coefs,
    escala       = arredonda(fit$scale, digits),
    aic          = arredonda(stats::AIC(fit), digits),
    objeto       = fit
  ), "Modelo parametrico de sobrevivencia (AFT)")
}

#' Tabela de vida atuarial
#'
#' Constroi a tabua de vida (metodo atuarial de Cutler-Ederer) agrupando os
#' tempos em intervalos.
#'
#' @param tempo Vetor de tempos.
#' @param evento Vetor 0/1.
#' @param intervalos Vetor de cortes (limites dos intervalos).
#' @param digits Inteiro.
#'
#' @return tibble com `intervalo`, `n_inicio`, `n_evento`, `n_censura`,
#'   `n_risco`, `prob_evento`, `prob_sobrevivencia`, `sobrevivencia_acumulada`.
#'
#' @examples
#' rnp_tabela_vida(survival::lung$time, survival::lung$status == 2,
#'                 intervalos = seq(0, 1000, by = 200))
#' @family sobrevivencia
#' @export
rnp_tabela_vida <- function(tempo, evento, intervalos, digits = 4L) {
  abort_numerico(tempo, "tempo")
  ev <- as.integer(evento)
  classe <- cut(tempo, breaks = intervalos, right = FALSE, include.lowest = TRUE)
  niveis <- levels(classe)
  k <- length(niveis)
  n_evento <- as.integer(tapply(ev, classe, sum)[niveis]); n_evento[is.na(n_evento)] <- 0L
  n_total  <- as.integer(table(classe)[niveis])
  n_censura <- n_total - n_evento
  n_inicio <- rev(cumsum(rev(n_total)))            # entram no intervalo
  n_risco  <- n_inicio - n_censura / 2             # ajuste atuarial
  q <- n_evento / n_risco                          # prob. condicional de evento
  p <- 1 - q
  S <- cumprod(c(1, p))[seq_len(k)]                # S no inicio de cada intervalo
  tibble::tibble(
    intervalo               = niveis,
    n_inicio                = n_inicio,
    n_evento                = n_evento,
    n_censura               = n_censura,
    n_risco                 = arredonda(n_risco, digits),
    prob_evento             = arredonda(q, digits),
    prob_sobrevivencia      = arredonda(p, digits),
    sobrevivencia_acumulada = arredonda(S, digits)
  )
}

#' Risco relativo predito pelo modelo de Cox
#'
#' Calcula o risco relativo (em relacao a media das covariaveis) para novos
#' perfis de covariaveis.
#'
#' @param modelo Objeto `coxph` ou saida de [rnp_cox()].
#' @param novos_dados data.frame com as covariaveis dos perfis.
#' @param digits Inteiro.
#'
#' @return tibble com o risco relativo predito por perfil.
#'
#' @examples
#' fit <- rnp_cox(survival::Surv(time, status) ~ age + sex, survival::lung)
#' rnp_cox_risco_relativo(fit, data.frame(age = c(50, 70), sex = c(1, 1)))
#' @family sobrevivencia
#' @export
rnp_cox_risco_relativo <- function(modelo, novos_dados, digits = 4L) {
  fit <- if (inherits(modelo, "coxph")) modelo else modelo$objeto
  if (!inherits(fit, "coxph")) rlang::abort("{.arg modelo} deve ser coxph ou saida de rnp_cox().")
  rr <- stats::predict(fit, newdata = novos_dados, type = "risk")
  dplyr::bind_cols(tibble::as_tibble(novos_dados),
                   tibble::tibble(risco_relativo = arredonda(unname(rr), digits)))
}
