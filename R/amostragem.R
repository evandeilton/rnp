#' Amostragem aleatoria simples
#'
#' @param base data.frame ou vetor.
#' @param n Inteiro. Tamanho da amostra.
#' @param seed Inteiro. Semente aleatoria.
#'
#' @return Amostra (mesmo tipo de base).
#'
#' @examples
#' rnp_amostra_simples(mtcars, 10)
#' @export
rnp_amostra_simples <- function(base, n, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  if (is.data.frame(base) || is.matrix(base)) {
    if (n > nrow(base)) rlang::abort("n > nrow(base).")
    idx <- sample.int(nrow(base), n)
    base[idx, , drop = FALSE]
  } else if (is.vector(base)) {
    if (n > length(base)) rlang::abort("n > length(base).")
    sample(base, n)
  } else {
    rlang::abort("{.arg base} deve ser data.frame, matriz ou vetor.")
  }
}

#' Amostragem sistematica
#'
#' @param base data.frame ou vetor.
#' @param n Inteiro. Tamanho da amostra.
#' @param seed Inteiro. Semente aleatoria (para o inicio).
#'
#' @return Amostra.
#'
#' @examples
#' rnp_amostra_sistematica(mtcars, 10)
#' @export
rnp_amostra_sistematica <- function(base, n, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  N <- if (is.data.frame(base) || is.matrix(base)) nrow(base) else length(base)
  if (n > N) rlang::abort("n > N.")
  k <- floor(N / n)
  inicio <- sample.int(k, 1L)
  idx <- seq(inicio, N, by = k)[seq_len(n)]
  if (is.data.frame(base) || is.matrix(base)) {
    base[idx, , drop = FALSE]
  } else {
    base[idx]
  }
}

#' Amostragem estratificada
#'
#' @param base data.frame.
#' @param estrato Nome da coluna (string) com os estratos.
#' @param n Vetor nomeado com tamanho por estrato, ou escalar para proporcional.
#' @param seed Inteiro.
#'
#' @return data.frame com amostra estratificada.
#'
#' @examples
#' rnp_amostra_estratificada(mtcars, "cyl", n = c("4" = 3, "6" = 2, "8" = 3))
#' @export
rnp_amostra_estratificada <- function(base, estrato, n, seed = NULL) {
  if (!is.data.frame(base)) rlang::abort("{.arg base} deve ser data.frame.")
  if (!is.character(estrato) || length(estrato) != 1L) {
    rlang::abort("{.arg estrato} deve ser string.")
  }
  if (!estrato %in% names(base)) rlang::abort("Coluna '{estrato}' nao encontrada.")
  if (!is.null(seed)) set.seed(seed)
  grupos <- split(base, base[[estrato]])
  if (length(n) == 1L && is.numeric(n)) {
    prop <- n / nrow(base)
    n_vec <- ceiling(sapply(grupos, function(g) nrow(g) * prop))
  } else if (is.numeric(n) && !is.null(names(n))) {
    n_vec <- n[names(grupos)]
    if (anyNA(n_vec)) rlang::abort("Nomes de n nao correspondem aos estratos.")
  } else {
    rlang::abort("{.arg n} deve ser escalar (proporcional) ou vetor nomeado.")
  }
  amostras <- mapply(function(g, ni) {
    if (ni > nrow(g)) rlang::warn("n > tamanho do estrato. Ajustando.")
    ni <- min(ni, nrow(g))
    g[sample.int(nrow(g), ni), , drop = FALSE]
  }, grupos, n_vec, SIMPLIFY = FALSE)
  do.call(rbind, amostras)
}

#' Amostragem por conglomerados
#'
#' @param base data.frame.
#' @param conglomerado Nome da coluna (string) com os conglomerados.
#' @param n_cong Inteiro. Numero de conglomerados a amostrar.
#' @param seed Inteiro.
#'
#' @return data.frame com todos os elementos dos conglomerados selecionados.
#'
#' @examples
#' rnp_amostra_conglomerada(mtcars, "cyl", n_cong = 2)
#' @export
rnp_amostra_conglomerada <- function(base, conglomerado, n_cong, seed = NULL) {
  if (!is.data.frame(base)) rlang::abort("{.arg base} deve ser data.frame.")
  if (!is.character(conglomerado) || length(conglomerado) != 1L) {
    rlang::abort("{.arg conglomerado} deve ser string.")
  }
  if (!conglomerado %in% names(base)) {
    rlang::abort("Coluna '{conglomerado}' nao encontrada.")
  }
  abort_inteiro_pos(n_cong, "n_cong")
  if (!is.null(seed)) set.seed(seed)
  cong_unicos <- unique(base[[conglomerado]])
  if (n_cong > length(cong_unicos)) {
    rlang::abort("n_cong > numero de conglomerados.")
  }
  selecionados <- sample(cong_unicos, n_cong)
  base[base[[conglomerado]] %in% selecionados, , drop = FALSE]
}

#' Tamanho de amostra para media
#'
#' Calcula n necessario para estimar media populacional com margem de erro E.
#'
#' @param sigma Desvio-padrao populacional (estimativa).
#' @param E Margem de erro desejada.
#' @param conf Nivel de confianca.
#' @param N Tamanho da populacao (default Inf = populacao infinita).
#'
#' @return tibble com \code{n_infinita}, \code{n_finita}, \code{E}, \code{conf}.
#'
#' @examples
#' rnp_tamanho_amostra_media(sigma = 10, E = 2, conf = 0.95)
#' @export
rnp_tamanho_amostra_media <- function(sigma, E, conf = 0.95, N = Inf) {
  if (!is.numeric(sigma) || sigma <= 0) rlang::abort("{.arg sigma} deve ser positivo.")
  if (!is.numeric(E) || E <= 0) rlang::abort("{.arg E} deve ser positivo.")
  abort_confianca(conf)
  z <- stats::qnorm((1 + conf) / 2)
  n_inf <- ceiling((z * sigma / E)^2)
  n_fin <- if (is.finite(N)) {
    ceiling(n_inf / (1 + (n_inf - 1) / N))
  } else {
    n_inf
  }
  tibble::tibble(
    n_infinita = n_inf,
    n_finita   = n_fin,
    E          = E,
    conf       = conf
  )
}

#' Tamanho de amostra para proporcao
#'
#' @param p Proporcao esperada (default 0.5 = maximo).
#' @param E Margem de erro.
#' @param conf Nivel de confianca.
#' @param N Tamanho da populacao (default Inf).
#'
#' @return tibble.
#'
#' @examples
#' rnp_tamanho_amostra_proporcao(E = 0.05, conf = 0.95)
#' @export
rnp_tamanho_amostra_proporcao <- function(p = 0.5, E, conf = 0.95, N = Inf) {
  abort_proporcao(p, "p")
  if (!is.numeric(E) || E <= 0) rlang::abort("{.arg E} deve ser positivo.")
  abort_confianca(conf)
  z <- stats::qnorm((1 + conf) / 2)
  n_inf <- ceiling(z^2 * p * (1 - p) / E^2)
  n_fin <- if (is.finite(N)) {
    ceiling(n_inf / (1 + (n_inf - 1) / N))
  } else {
    n_inf
  }
  tibble::tibble(
    n_infinita = n_inf,
    n_finita   = n_fin,
    E          = E,
    conf       = conf
  )
}
