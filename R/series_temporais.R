# Series temporais (FATIA 6) — implementacao com base R + backend C++ (C-07),
# sem dependencias de forecast/tseries/patchwork.

#' Decomposicao de serie temporal
#'
#' Wrapper de [stats::decompose()] e [stats::stl()].
#'
#' @param x Vetor numerico ou objeto `ts`.
#' @param type String: `"classica"` ou `"stl"`.
#' @param frequency Inteiro. Periodicidade (default `NULL` = auto).
#' @param digits Inteiro.
#'
#' @return Uma lista com `serie` (objeto ajustado) e `componentes` (tibble).
#'
#' @examples
#' x <- ts(rnorm(100) + rep(1:4, 25), frequency = 4)
#' rnp_ts_decomposicao(x)
#' @family series
#' @export
rnp_ts_decomposicao <- function(x, type = c("classica", "stl"),
                                frequency = NULL, digits = 4L) {
  type <- rlang::arg_match(type)
  if (!stats::is.ts(x)) {
    if (!is.numeric(x)) rlang::abort("{.arg x} deve ser numerico ou ts.")
    if (is.null(frequency)) frequency <- 1L
    x <- stats::ts(x, frequency = as.integer(frequency))
  }
  if (type == "classica") {
    fit <- stats::decompose(x)
    comp <- tibble::tibble(
      observacao = seq_along(x), tempo = as.numeric(stats::time(x)),
      observada = as.numeric(fit$x), tendencia = as.numeric(fit$trend),
      sazonal = as.numeric(fit$seasonal), aleatorio = as.numeric(fit$random))
  } else {
    fit <- stats::stl(x, s.window = "periodic")
    comp <- tibble::tibble(
      observacao = seq_along(x), tempo = as.numeric(stats::time(x)),
      observada = as.numeric(x),
      tendencia = as.numeric(fit$time.series[, "trend"]),
      sazonal = as.numeric(fit$time.series[, "seasonal"]),
      aleatorio = as.numeric(fit$time.series[, "remainder"]))
  }
  list(serie = fit,
       componentes = comp |> dplyr::mutate(
         dplyr::across(where(is.numeric) & !observacao, ~ arredonda(.x, digits))))
}

#' Media movel
#'
#' Media movel simples (centrada ou retroativa), via backend C++.
#'
#' @param x Vetor numerico.
#' @param k Inteiro. Tamanho da janela.
#' @param centrada Logico. Janela centrada (default) ou retroativa.
#' @param digits Inteiro.
#'
#' @return tibble com `tempo`, `original` e `media_movel`.
#'
#' @examples
#' rnp_media_movel(as.numeric(AirPassengers), k = 12)
#' @family series
#' @export
rnp_media_movel <- function(x, k = 3L, centrada = TRUE, digits = 4L) {
  abort_numerico(x, "x")
  abort_inteiro_pos(k, "k")
  mm <- as.numeric(media_movel_cpp(as.numeric(x), as.integer(k), centrada))
  tibble::tibble(
    tempo = seq_along(x),
    original = as.numeric(x),
    media_movel = arredonda(mm, digits))
}

#' Suavizacao exponencial
#'
#' Suavizacao exponencial simples (EWMA), via backend C++.
#'
#' @param x Vetor numerico.
#' @param alpha Fator de suavizacao em (0, 1).
#' @param digits Inteiro.
#'
#' @return tibble com `tempo`, `original` e `suavizada`.
#'
#' @examples
#' rnp_suavizacao_exponencial(as.numeric(AirPassengers), alpha = 0.3)
#' @family series
#' @export
rnp_suavizacao_exponencial <- function(x, alpha = 0.3, digits = 4L) {
  abort_numerico(x, "x")
  if (alpha <= 0 || alpha >= 1) rlang::abort("{.arg alpha} deve estar em (0, 1).")
  sv <- as.numeric(ewma_cpp(as.numeric(x), alpha))
  tibble::tibble(
    tempo = seq_along(x),
    original = as.numeric(x),
    suavizada = arredonda(sv, digits))
}

#' Funcao de autocorrelacao (ACF)
#'
#' Calcula a ACF ate `lag_max` (backend C++), com bandas de confianca.
#'
#' @param x Vetor numerico ou `ts`.
#' @param lag_max Inteiro. Numero maximo de defasagens.
#' @param digits Inteiro.
#'
#' @return tibble com `lag`, `acf`, `lim_inf`, `lim_sup`.
#'
#' @examples
#' rnp_ts_acf(as.numeric(AirPassengers), lag_max = 20)
#' @family series
#' @export
rnp_ts_acf <- function(x, lag_max = NULL, digits = 4L) {
  x <- as.numeric(x)
  n <- length(x)
  if (is.null(lag_max)) lag_max <- floor(10 * log10(n))
  lag_max <- min(lag_max, n - 1L)
  a <- as.numeric(acf_cpp(x, as.integer(lag_max)))
  ci <- 1.96 / sqrt(n)
  tibble::tibble(
    lag = 0:lag_max, acf = arredonda(a, digits),
    lim_inf = arredonda(-ci, digits), lim_sup = arredonda(ci, digits))
}

#' Funcao de autocorrelacao parcial (PACF)
#'
#' Calcula a PACF ate `lag_max` por Durbin-Levinson (backend C++).
#'
#' @param x Vetor numerico ou `ts`.
#' @param lag_max Inteiro. Numero maximo de defasagens.
#' @param digits Inteiro.
#'
#' @return tibble com `lag`, `pacf`, `lim_inf`, `lim_sup`.
#'
#' @examples
#' rnp_ts_pacf(as.numeric(AirPassengers), lag_max = 20)
#' @family series
#' @export
rnp_ts_pacf <- function(x, lag_max = NULL, digits = 4L) {
  x <- as.numeric(x)
  n <- length(x)
  if (is.null(lag_max)) lag_max <- floor(10 * log10(n))
  lag_max <- min(lag_max, n - 1L)
  p <- as.numeric(pacf_cpp(x, as.integer(lag_max)))
  ci <- 1.96 / sqrt(n)
  tibble::tibble(
    lag = seq_len(lag_max), pacf = arredonda(p, digits),
    lim_inf = arredonda(-ci, digits), lim_sup = arredonda(ci, digits))
}

#' Diferenciacao de serie
#'
#' Aplica diferenciacao regular (ordem `d`) e/ou sazonal (ordem `D`, periodo
#' `s`).
#'
#' @param x Vetor numerico.
#' @param d Inteiro. Ordem da diferenciacao regular.
#' @param D Inteiro. Ordem da diferenciacao sazonal.
#' @param s Inteiro. Periodo sazonal.
#'
#' @return Vetor numerico diferenciado.
#'
#' @examples
#' rnp_ts_diferenciacao(as.numeric(AirPassengers), d = 1, D = 1, s = 12)
#' @family series
#' @export
rnp_ts_diferenciacao <- function(x, d = 1L, D = 0L, s = 12L) {
  x <- as.numeric(x)
  if (D > 0) x <- diff(x, lag = s, differences = D)
  if (d > 0) x <- diff(x, differences = d)
  x
}

#' Teste de Ljung-Box (autocorrelacao)
#'
#' Testa a presenca de autocorrelacao ate `lag` defasagens (H0: ruido branco).
#'
#' @param x Vetor numerico (ex.: residuos de um modelo).
#' @param lag Inteiro. Numero de defasagens.
#' @param fitdf Inteiro. Graus de liberdade gastos no ajuste (subtraidos).
#' @param digits Inteiro.
#'
#' @return tibble com `estatistica`, `gl`, `p_valor`.
#'
#' @examples
#' rnp_ts_ljung_box(rnorm(200), lag = 10)
#' @family series
#' @export
rnp_ts_ljung_box <- function(x, lag = 10L, fitdf = 0L, digits = 4L) {
  res <- stats::Box.test(as.numeric(x), lag = lag, type = "Ljung-Box",
                         fitdf = fitdf)
  tibble::tibble(
    estatistica = arredonda(unname(res$statistic), digits),
    gl          = unname(res$parameter),
    p_valor     = arredonda(res$p.value, digits))
}

#' Suavizacao de Holt-Winters
#'
#' Wrapper de [stats::HoltWinters()] (nivel, tendencia e sazonalidade).
#'
#' @param x Objeto `ts` (ou numerico, convertido com `frequency`).
#' @param frequency Inteiro. Periodicidade (se `x` nao for `ts`).
#' @param sazonal String: `"additive"` ou `"multiplicative"`.
#' @param digits Inteiro.
#'
#' @return Uma lista com `modelo` (HoltWinters), `parametros` (tibble: alpha,
#'   beta, gamma) e `sse`.
#'
#' @examples
#' rnp_ts_holt_winters(AirPassengers)$parametros
#' @family series
#' @export
rnp_ts_holt_winters <- function(x, frequency = 12L,
                                sazonal = c("additive", "multiplicative"),
                                digits = 4L) {
  sazonal <- rlang::arg_match(sazonal)
  if (!stats::is.ts(x)) x <- stats::ts(as.numeric(x), frequency = as.integer(frequency))
  fit <- stats::HoltWinters(x, seasonal = sazonal)
  list(
    modelo = fit,
    parametros = tibble::tibble(
      alpha = arredonda(fit$alpha, digits),
      beta  = arredonda(as.numeric(fit$beta), digits),
      gamma = arredonda(as.numeric(fit$gamma), digits)),
    sse = arredonda(fit$SSE, digits))
}

#' Periodograma (analise espectral)
#'
#' Estima o periodograma (densidade espectral bruta) de uma serie.
#'
#' @param x Vetor numerico.
#' @param digits Inteiro.
#'
#' @return tibble com `frequencia`, `periodo` e `densidade`.
#'
#' @examples
#' rnp_ts_periodograma(as.numeric(AirPassengers))
#' @family series
#' @export
rnp_ts_periodograma <- function(x, digits = 4L) {
  x <- as.numeric(x)
  sp <- stats::spec.pgram(x, plot = FALSE, detrend = TRUE, taper = 0)
  tibble::tibble(
    frequencia = arredonda(sp$freq, digits),
    periodo    = arredonda(1 / sp$freq, digits),
    densidade  = arredonda(sp$spec, digits))
}

#' Grafico de serie temporal
#'
#' @param x Vetor numerico ou `ts`.
#' @param titulo Titulo opcional.
#'
#' @return Objeto `ggplot`.
#'
#' @examples
#' rnp_grafico_serie(as.numeric(AirPassengers))
#' @family series
#' @export
rnp_grafico_serie <- function(x, titulo = NULL) {
  d <- tibble::tibble(tempo = seq_along(x), valor = as.numeric(x))
  ggplot2::ggplot(d, ggplot2::aes(.data$tempo, .data$valor)) +
    ggplot2::geom_line(color = rnp_paleta_rnp("rnp_qual", 1)) +
    rnp_tema_rnp() +
    ggplot2::labs(title = titulo %||% "Serie temporal", x = "Tempo", y = "Valor")
}

#' Grafico de ACF ou PACF
#'
#' @param x Vetor numerico ou `ts`.
#' @param tipo String: `"acf"` ou `"pacf"`.
#' @param lag_max Inteiro. Numero maximo de defasagens.
#'
#' @return Objeto `ggplot`.
#'
#' @examples
#' rnp_grafico_acf(as.numeric(AirPassengers), tipo = "acf")
#' @family series
#' @export
rnp_grafico_acf <- function(x, tipo = c("acf", "pacf"), lag_max = NULL) {
  tipo <- rlang::arg_match(tipo)
  d <- if (tipo == "acf") rnp_ts_acf(x, lag_max) else rnp_ts_pacf(x, lag_max)
  col <- if (tipo == "acf") "acf" else "pacf"
  ggplot2::ggplot(d, ggplot2::aes(x = .data$lag, y = .data[[col]])) +
    ggplot2::geom_hline(yintercept = 0, color = "grey50") +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lim_inf, ymax = .data$lim_sup),
                         fill = "blue", alpha = 0.1) +
    ggplot2::geom_segment(ggplot2::aes(xend = .data$lag, yend = 0),
                          color = rnp_paleta_rnp("rnp_qual", 1)) +
    rnp_tema_rnp() +
    ggplot2::labs(title = toupper(tipo), x = "Lag", y = toupper(tipo))
}
