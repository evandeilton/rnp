#' Decomposicao de serie temporal
#'
#' Wrapper de [stats::decompose()] e [stats::stl()].
#'
#' @param x Vetor numerico ou objeto \code{ts}.
#' @param type String: \code{"classica"} ou \code{"stl"}.
#' @param frequency Inteiro. Periodicidade (default NULL = auto).
#' @param digits Inteiro.
#'
#' @return lista:
#'   * \code{serie}: objeto \code{ts} ou \code{stl}.
#'   * \code{componentes}: tibble com observacao, tempo, observada, tendencia,
#'     sazonal, aleatorio.
#'
#' @examples
#' x <- ts(rnorm(100) + rep(1:4, 25), frequency = 4)
#' rnp_ts_decomposicao(x)
#' @export
rnp_ts_decomposicao <- function(x, type = c("classica", "stl"),
                                frequency = NULL, digits = 4L) {
  type <- rlang::arg_match(type)
  if (!is.ts(x)) {
    if (!is.numeric(x)) rlang::abort("{.arg x} deve ser numerico ou ts.")
    if (is.null(frequency)) frequency <- 1L
    x <- stats::ts(x, frequency = as.integer(frequency))
  }
  if (type == "classica") {
    fit <- stats::decompose(x)
    comp <- tibble::tibble(
      observacao = seq_along(x),
      tempo      = stats::time(x),
      observada  = as.numeric(fit$x),
      tendencia  = as.numeric(fit$trend),
      sazonal    = as.numeric(fit$seasonal),
      aleatorio  = as.numeric(fit$random)
    )
  } else {
    fit <- stats::stl(x, s.window = "periodic")
    comp <- tibble::tibble(
      observacao = seq_along(x),
      tempo      = stats::time(x),
      observada  = as.numeric(x),
      tendencia  = as.numeric(fit$time.series[, "trend"]),
      sazonal    = as.numeric(fit$time.series[, "seasonal"]),
      aleatorio  = as.numeric(fit$time.series[, "remainder"])
    )
  }
  list(
    serie       = fit,
    componentes = comp |> dplyr::mutate(dplyr::across(where(is.numeric) & !observacao,
                                                      ~ arredonda(.x, digits)))
  )
}

#' ACF e PACF
#'
#' Calcula e plota ACF e PACF com IC 95%.
#'
#' @param x Vetor numerico ou objeto \code{ts}.
#' @param lag.max Inteiro. Numero maximo de lags.
#' @param plot Logico. Gerar grafico.
#' @param digits Inteiro.
#'
#' @return lista:
#'   * \code{acf}: tibble com lag, acf.
#'   * \code{pacf}: tibble com lag, pacf.
#'   * \code{plot}: objeto ggplot (se plot = TRUE).
#'
#' @examples
#' rnp_ts_acf_pacf(rnorm(100), lag.max = 20)
#' @export
rnp_ts_acf_pacf <- function(x, lag.max = NULL, plot = TRUE, digits = 4L) {
  if (!is.numeric(x) && !is.ts(x)) rlang::abort("{.arg x} deve ser numerico ou ts.")
  if (is.null(lag.max)) lag.max <- min(max(10, 10 * log10(length(x))), length(x) - 1L)
  acf_fit <- stats::acf(x, lag.max = lag.max, plot = FALSE)
  pacf_fit <- stats::pacf(x, lag.max = lag.max, plot = FALSE)
  acf_tbl <- tibble::tibble(
    lag = acf_fit$lag[, 1L, 1L],
    acf = as.numeric(acf_fit$acf[, 1L, 1L])
  )
  pacf_tbl <- tibble::tibble(
    lag = pacf_fit$lag[, 1L, 1L],
    pacf = as.numeric(pacf_fit$acf[, 1L, 1L])
  )
  out <- list(
    acf  = acf_tbl |> dplyr::mutate(dplyr::across(where(is.numeric),
                                                  ~ arredonda(.x, digits))),
    pacf = pacf_tbl |> dplyr::mutate(dplyr::across(where(is.numeric),
                                                   ~ arredonda(.x, digits)))
  )
  if (plot) {
    n <- length(x)
    ci <- 1.96 / sqrt(n)
    p1 <- ggplot2::ggplot(acf_tbl, ggplot2::aes(x = .data$lag, y = .data$acf)) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
      ggplot2::geom_hline(yintercept = c(-ci, ci), linetype = "dotted", color = "blue") +
      ggplot2::geom_segment(ggplot2::aes(xend = .data$lag, yend = 0)) +
      ggplot2::labs(title = "ACF", x = "Lag", y = "ACF") +
      ggplot2::theme_minimal()
    p2 <- ggplot2::ggplot(pacf_tbl, ggplot2::aes(x = .data$lag, y = .data$pacf)) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
      ggplot2::geom_hline(yintercept = c(-ci, ci), linetype = "dotted", color = "blue") +
      ggplot2::geom_segment(ggplot2::aes(xend = .data$lag, yend = 0)) +
      ggplot2::labs(title = "PACF", x = "Lag", y = "PACF") +
      ggplot2::theme_minimal()
    out$plot <- p1 + p2
  }
  out
}

#' Teste de estacionariedade (ADF, KPSS)
#'
#' Wrapper para testes de raiz unitaria.
#'
#' @param x Vetor numerico ou objeto \code{ts}.
#' @param test String: \code{"adf"} ou \code{"kpss"}.
#' @param k Inteiro. Numero de lags (default NULL = auto).
#' @param digits Inteiro.
#'
#' @return tibble com \code{estatistica}, \code{p_valor}, \code{test},
#'   \code{lags}, \code{estacionaria}.
#'
#' @examples
#' rnp_ts_teste_estacionariedade(rnorm(100), test = "kpss")
#' @export
rnp_ts_teste_estacionariedade <- function(x, test = c("adf", "kpss"),
                                          k = NULL, digits = 4L) {
  test <- rlang::arg_match(test)
  precisa_pacote("forecast", "rnp_ts_teste_estacionariedade")
  precisa_pacote("tseries", "rnp_ts_teste_estacionariedade")
  if (!is.numeric(x) && !is.ts(x)) rlang::abort("{.arg x} deve ser numerico ou ts.")
  if (!is.ts(x)) x <- stats::ts(x)
  if (test == "adf") {
    fit <- forecast::ndiffs(x, test = "kpss")
    ur <- tseries::adf.test(x, k = k %||% trunc((length(x) - 1)^(1/3)))
    tibble::tibble(
      estatistica   = unname(ur$statistic),
      p_valor       = ur$p.value,
      test          = "ADF",
      lags          = k %||% trunc((length(x) - 1)^(1/3)),
      estacionaria  = ur$p.value < 0.05
    )
  } else {
    ur <- tryCatch(
      tseries::kpss.test(x, null = "Level", lshort = TRUE),
      error = function(e) {
        rlang::abort("Teste KPSS falhou. Verifique se o pacote tseries esta instalado.")
      }
    )
    tibble::tibble(
      estatistica   = unname(ur$statistic),
      p_valor       = ur$p.value,
      test          = "KPSS",
      lags          = NA_integer_,
      estacionaria  = ur$p.value > 0.05
    )
  } |> dplyr::mutate(dplyr::across(where(is.numeric) & !lags,
                                   ~ arredonda(.x, digits)))
}

#' Ajuste de modelo ARIMA
#'
#' Wrapper de [forecast::auto.arima()].
#'
#' @param x Vetor numerico ou objeto \code{ts}.
#' @param seasonal Logico. Incluir componente sazonal.
#' @param ic String: \code{"aic"}, \code{"bic"}, \code{"aicc"}.
#' @param digits Inteiro.
#'
#' @return lista:
#'   * \code{modelo}: objeto \code{Arima}.
#'   * \code{resumo}: tibble com coeficientes, erros-padrao, AIC, BIC, AICc.
#'
#' @examples
#' x <- ts(rnorm(100), frequency = 12)
#' rnp_ts_arima(x)
#' @export
rnp_ts_arima <- function(x, seasonal = TRUE, ic = c("aic", "bic", "aicc"),
                         digits = 4L) {
  precisa_pacote("forecast", "rnp_ts_arima")
  ic <- rlang::arg_match(ic)
  if (!is.numeric(x) && !is.ts(x)) rlang::abort("{.arg x} deve ser numerico ou ts.")
  if (!is.ts(x)) x <- stats::ts(x)
  fit <- forecast::auto.arima(x, seasonal = seasonal, ic = ic, trace = FALSE)
  sm <- summary(fit)
  coefs <- sm$coef
  se <- sm$se
  coef_tbl <- tibble::tibble(
    termo       = names(coefs),
    estimativa  = coefs,
    erro_padrao = se
  )
  mod_tbl <- tibble::tibble(
    aic  = sm$aic,
    bic  = sm$bic,
    aicc = sm$aicc,
    nobs = stats::nobs(fit)
  )
  list(
    modelo  = fit,
    resumo  = list(
      coeficientes = coef_tbl |> dplyr::mutate(dplyr::across(where(is.numeric),
                                                             ~ arredonda(.x, digits))),
      modelo       = mod_tbl |> dplyr::mutate(dplyr::across(where(is.numeric) & !nobs,
                                                            ~ arredonda(.x, digits)))
    )
  )
}
