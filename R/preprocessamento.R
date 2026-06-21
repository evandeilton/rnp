# Pre-processamento (FATIA 7): padronizacao, normalizacao, winsorizacao,
# imputacao, discretizacao e codificacao dummy.

#' Padroniza (z-score)
#'
#' Centra na media e escala pelo desvio-padrao: z = (x - media) / dp.
#'
#' @param x Vetor numerico.
#' @param na.rm Logico.
#'
#' @return Vetor numerico padronizado.
#'
#' @examples
#' rnp_padroniza(mtcars$mpg)
#' @family preprocessamento
#' @export
rnp_padroniza <- function(x, na.rm = TRUE) {
  abort_numerico(x, "x")
  m <- mean(x, na.rm = na.rm); s <- stats::sd(x, na.rm = na.rm)
  if (is.na(s) || s == 0) rlang::abort("Desvio-padrao nulo ou indefinido.")
  (x - m) / s
}

#' Normaliza (min-max para \[0, 1\])
#'
#' Reescala para o intervalo \[0, 1\]: (x - min) / (max - min).
#'
#' @param x Vetor numerico.
#' @param na.rm Logico.
#'
#' @return Vetor numerico em \[0, 1\].
#'
#' @examples
#' rnp_normaliza(mtcars$hp)
#' @family preprocessamento
#' @export
rnp_normaliza <- function(x, na.rm = TRUE) {
  abort_numerico(x, "x")
  mn <- min(x, na.rm = na.rm); mx <- max(x, na.rm = na.rm)
  if (mn == mx) rlang::abort("Amplitude nula (min == max).")
  (x - mn) / (mx - mn)
}

#' Winsorizacao de outliers
#'
#' Limita os valores extremos aos quantis `p` e `1 - p` (winsorizacao
#' simetrica), reduzindo o impacto de outliers.
#'
#' @param x Vetor numerico.
#' @param p Proporcao a winsorizar em cada cauda (ex.: 0.05).
#' @param na.rm Logico.
#'
#' @return Vetor numerico winsorizado.
#'
#' @examples
#' rnp_winsoriza(c(-100, 1:20, 200), p = 0.05)
#' @family preprocessamento
#' @export
rnp_winsoriza <- function(x, p = 0.05, na.rm = TRUE) {
  abort_numerico(x, "x")
  if (p <= 0 || p >= 0.5) rlang::abort("{.arg p} deve estar em (0, 0.5).")
  lim <- stats::quantile(x, c(p, 1 - p), na.rm = na.rm, names = FALSE)
  pmin(pmax(x, lim[1L]), lim[2L])
}

#' Imputacao de valores faltantes
#'
#' Imputa NA por media/mediana/moda (por coluna) ou por k vizinhos mais
#' proximos (backend C++) para variaveis numericas.
#'
#' @param base data.frame ou matriz.
#' @param metodo String: `"media"`, `"mediana"`, `"moda"` ou `"knn"`.
#' @param k Inteiro. Numero de vizinhos (apenas `"knn"`).
#'
#' @return data.frame com os NA imputados.
#'
#' @examples
#' df <- data.frame(a = c(1, NA, 3, 4), b = c(NA, 2, 3, 4))
#' rnp_imputa(df, metodo = "media")
#' rnp_imputa(df, metodo = "knn", k = 2)
#' @family preprocessamento
#' @export
rnp_imputa <- function(base, metodo = c("media", "mediana", "moda", "knn"),
                       k = 5L) {
  metodo <- rlang::arg_match(metodo)
  base <- as.data.frame(base)
  if (metodo == "knn") {
    num <- vapply(base, is.numeric, logical(1))
    if (!any(num)) rlang::abort("KNN requer colunas numericas.")
    X <- as.matrix(base[, num, drop = FALSE])
    X[is.na(X)] <- NaN
    imp <- knn_imputa_cpp(X, as.integer(k))
    base[, num] <- as.data.frame(imp)
    return(tibble::as_tibble(base))
  }
  moda_vec <- function(v) {
    uv <- unique(v[!is.na(v)])
    uv[which.max(tabulate(match(v, uv)))]
  }
  base[] <- lapply(base, function(col) {
    if (!anyNA(col)) return(col)
    if (is.numeric(col)) {
      val <- switch(metodo,
        media   = mean(col, na.rm = TRUE),
        mediana = stats::median(col, na.rm = TRUE),
        moda    = moda_vec(col))
      col[is.na(col)] <- val
    } else {
      col[is.na(col)] <- moda_vec(col)
    }
    col
  })
  tibble::as_tibble(base)
}

#' Discretizacao (binning)
#'
#' Converte uma variavel continua em categorias por largura igual, frequencia
#' igual (quantis) ou k-means.
#'
#' @param x Vetor numerico.
#' @param k Inteiro. Numero de classes.
#' @param metodo String: `"largura"`, `"frequencia"` ou `"kmeans"`.
#' @param rotulos Vetor opcional de rotulos das classes.
#'
#' @return Fator com `k` niveis.
#'
#' @examples
#' rnp_discretiza(mtcars$mpg, k = 3, metodo = "frequencia")
#' @family preprocessamento
#' @export
rnp_discretiza <- function(x, k = 4L, metodo = c("largura", "frequencia", "kmeans"),
                           rotulos = NULL) {
  abort_numerico(x, "x")
  abort_inteiro_pos(k, "k")
  metodo <- rlang::arg_match(metodo)
  quebras <- switch(metodo,
    largura = seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = k + 1L),
    frequencia = stats::quantile(x, probs = seq(0, 1, length.out = k + 1L),
                                 na.rm = TRUE, names = FALSE),
    kmeans = {
      set.seed(42L)
      km <- stats::kmeans(stats::na.omit(x), centers = k)
      centros <- sort(km$centers[, 1L])
      pontos_medios <- (centros[-1L] + centros[-length(centros)]) / 2
      c(min(x, na.rm = TRUE), pontos_medios, max(x, na.rm = TRUE))
    }
  )
  quebras <- unique(quebras)
  cut(x, breaks = quebras, labels = rotulos, include.lowest = TRUE)
}

#' Codificacao dummy (one-hot)
#'
#' Converte variaveis categoricas em colunas indicadoras (0/1).
#'
#' @param base data.frame.
#' @param colunas Vetor opcional de nomes de colunas a codificar. Default:
#'   todas as colunas categoricas/fator.
#' @param remover_primeira Logico. Remove a primeira categoria (referencia)
#'   de cada variavel para evitar colinearidade.
#'
#' @return tibble com as colunas dummy adicionadas e as originais removidas.
#'
#' @examples
#' rnp_dummy(data.frame(cor = c("a", "b", "a", "c"), x = 1:4))
#' @family preprocessamento
#' @export
rnp_dummy <- function(base, colunas = NULL, remover_primeira = FALSE) {
  base <- as.data.frame(base)
  if (is.null(colunas)) {
    colunas <- names(base)[vapply(base, function(v)
      is.character(v) || is.factor(v), logical(1))]
  }
  if (length(colunas) == 0L) return(tibble::as_tibble(base))
  novas <- list()
  for (cl in colunas) {
    f <- as.factor(base[[cl]])
    niveis <- levels(f)
    if (remover_primeira) niveis <- niveis[-1L]
    for (nv in niveis) {
      novas[[paste0(cl, "_", nv)]] <- as.integer(f == nv)
    }
  }
  resto <- base[, setdiff(names(base), colunas), drop = FALSE]
  tibble::as_tibble(dplyr::bind_cols(resto, tibble::as_tibble(novas)))
}
