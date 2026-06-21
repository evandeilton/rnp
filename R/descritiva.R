#' Estatisticas descritivas robustas
#'
#' Calcula um conjunto completo de estatisticas descritivas com tratamento
#' de valores faltantes e saida em tibble.
#'
#' @param x Vetor numerico.
#' @param digits Inteiro. Numero de casas decimais na saida.
#' @param na.rm Logico. Remove NA antes do calculo.
#'
#' @return tibble com 1 linha e colunas: \code{n}, \code{n_validos},
#'   \code{n_faltantes}, \code{soma}, \code{media}, \code{mediana},
#'   \code{moda}, \code{desvio}, \code{variancia}, \code{min}, \code{q1},
#'   \code{q3}, \code{max}, \code{amplitude}, \code{iqr}, \code{cv},
#'   \code{se_media}, \code{ic_inf}, \code{ic_sup}, \code{assimetria},
#'   \code{curtose}.
#'
#' @examples
#' rnp_descritiva(mtcars$mpg)
#' rnp_descritiva(airquality$Wind, na.rm = TRUE)
#' @export
#' @importFrom stats sd var median quantile
rnp_descritiva <- function(x, digits = 4L, na.rm = TRUE) {
  abort_numerico(x, "x")
  n_total <- length(x)
  n_na <- sum(is.na(x))
  if (na.rm) x <- sem_na(x)
  n <- length(x)
  n_validos <- n   # variavel distinta: evita mascaramento pela coluna 'n' no tibble
  if (n < 1L) {
    rlang::abort("Vetor sem observacoes validas apos remocao de NA.")
  }
  sx <- sum(x)
  mx <- mean(x)
  sdx <- stats::sd(x)
  varx <- stats::var(x)
  qx <- stats::quantile(x, probs = c(0, .25, .5, .75, 1),
                        names = FALSE, type = 7L, na.rm = FALSE)
  iqr_val <- unname(qx[4L] - qx[2L])
  cv_val <- if (mx != 0) sdx / mx else NA_real_
  se_val <- sdx / sqrt(n)
  tcrit <- if (n > 1L) stats::qt(.975, df = n - 1L) else NA_real_
  ic_inf <- mx - tcrit * se_val
  ic_sup <- mx + tcrit * se_val
  skew <- .assimetria_interna(x, mx, sdx, n)
  curt <- .curtose_interna(x, mx, sdx, n)

  out <- tibble::tibble_row(
    n             = n_total,
    n_validos     = n_validos,
    n_faltantes   = n_na,
    soma          = sx,
    media         = mx,
    mediana       = stats::median(x),
    moda          = .moda_interna(x),
    desvio        = sdx,
    variancia     = varx,
    min           = qx[1L],
    q1            = qx[2L],
    q3            = qx[4L],
    max           = qx[5L],
    amplitude     = qx[5L] - qx[1L],
    iqr           = iqr_val,
    cv            = cv_val,
    se_media      = se_val,
    ic_inf        = ic_inf,
    ic_sup        = ic_sup,
    assimetria    = skew,
    curtose       = curt
  )
  if (is.finite(digits) && digits >= 0L) {
    num_cols <- vapply(out, is.numeric, logical(1))
    out[num_cols] <- lapply(out[num_cols], arredonda, digits = digits)
  }
  out
}

# Internal: sample skewness (Fisher-Pearson, bias-corrected)
.assimetria_interna <- function(x, mu, sigma, n) {
  if (n < 3L || sigma == 0) return(NA_real_)
  m3 <- sum((x - mu)^3) / n
  m2 <- sum((x - mu)^2) / n
  g1 <- m3 / (m2^(3 / 2))
  g1 * sqrt(n * (n - 1)) / (n - 2)
}

# Internal: sample excess kurtosis (G2, bias-corrected; Joanes & Gill 1998)
.curtose_interna <- function(x, mu, sigma, n) {
  if (n < 4L || sigma == 0) return(NA_real_)
  m4 <- sum((x - mu)^4) / n
  m2 <- sum((x - mu)^2) / n
  b2 <- m4 / (m2^2)
  ((n - 1) / ((n - 2) * (n - 3))) * ((n + 1) * (b2 - 3) + 6)
}

# Internal: sample mode (most frequent value, smallest on ties)
.moda_interna <- function(x) {
  ux <- unique(x)
  if (length(ux) <= 1L) return(ux[1L])
  counts <- vctrs::vec_match(x, ux)
  tab <- tabulate(counts)
  cand <- which(tab == max(tab))
  ux[min(cand)]
}

#' Estatisticas descritivas por grupo
#'
#' Versao grouped de [rnp_descritiva()], construida sobre \code{dplyr}
#' (sem dependencia do \code{plyr} descontinuado).
#'
#' @param base data.frame ou tibble.
#' @param variavel Nome (string) da coluna numerica a analisar.
#' @param grupos Vetor de strings com nomes das colunas de agrupamento.
#' @param digits Inteiro. Casas decimais.
#'
#' @return tibble com colunas de grupo + estatisticas (ver [rnp_descritiva()]).
#'
#' @examples
#' rnp_descritiva_by(mtcars, "mpg", "cyl")
#' rnp_descritiva_by(mtcars, "wt", c("gear", "cyl"))
#' @export
rnp_descritiva_by <- function(base, variavel, grupos, digits = 4L) {
  if (!is.data.frame(base)) {
    rlang::abort("{.arg base} deve ser um data.frame.")
  }
  if (!is.character(variavel) || length(variavel) != 1L) {
    rlang::abort("{.arg variavel} deve ser string com nome de uma coluna numerica.")
  }
  if (!variavel %in% names(base)) {
    rlang::abort("Coluna '{variavel}' nao encontrada em {.arg base}.")
  }
  if (!is.numeric(base[[variavel]])) {
    rlang::abort("Coluna '{variavel}' deve ser numerica.")
  }
  grupos <- as.character(grupos)
  faltantes <- setdiff(grupos, names(base))
  if (length(faltantes)) {
    rlang::abort("Colunas de grupo nao encontradas: {paste(faltantes, collapse=', ')}.")
  }

  base |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grupos))) |>
    dplyr::group_modify(~ rnp_descritiva(.x[[variavel]], digits = digits)) |>
    dplyr::ungroup()
}

#' Quantis com metodo configuravel
#'
#' Wrapper de [stats::quantile()] expondo o argumento \code{type}
#' (Hyndman-Fan, 1 a 9).
#'
#' @param x Vetor numerico.
#' @param probs Vetor de probabilidades em \[0, 1\].
#' @param type Inteiro 1-9. Metodo do quantil.
#' @param na.rm Logico.
#' @param digits Inteiro. Casas decimais.
#'
#' @return Vetor nomeado.
#'
#' @examples
#' rnp_quantis(1:100, c(.1, .25, .5, .75, .9), type = 7)
#' @export
rnp_quantis <- function(x, probs = c(.25, .5, .75),
                        type = 7L, na.rm = TRUE, digits = 4L) {
  abort_numerico(x, "x")
  type <- check_quantile_type(type)
  if (!is.numeric(probs) || anyNA(probs) || any(probs < 0) || any(probs > 1)) {
    rlang::abort("{.arg probs} deve conter valores em [0, 1].")
  }
  q <- stats::quantile(x, probs = probs, type = type, na.rm = na.rm)
  arredonda(unname(q), digits = digits) |>
    stats::setNames(names(q))
}

#' Coeficiente de assimetria (skewness)
#'
#' Calcula o coeficiente de assimetria. Metodos: \code{pearson},
#' \code{fisher} (bias-corrected, default), \code{bowley} (quartis).
#'
#' @param x Vetor numerico.
#' @param method String: \code{"fisher"}, \code{"pearson"} ou \code{"bowley"}.
#' @param na.rm Logico.
#'
#' @return Escalar numerico.
#'
#' @examples
#' rnp_skewness(rnorm(1000))
#' rnp_skewness(1:100, method = "bowley")
#' @export
rnp_skewness <- function(x, method = c("fisher", "pearson", "bowley"),
                         na.rm = TRUE) {
  abort_numerico(x, "x")
  method <- rlang::arg_match(method)
  if (na.rm) x <- sem_na(x)
  n <- length(x)
  if (n < 3L) {
    rlang::warn("Assimetria requer n >= 3. Retornando NA.")
    return(NA_real_)
  }
  mu <- mean(x)
  s <- stats::sd(x)
  if (s == 0) return(NA_real_)
  switch(method,
    fisher  = .assimetria_interna(x, mu, s, n),
    pearson = 3 * (mu - stats::median(x)) / s,
    bowley  = {
      q <- stats::quantile(x, c(.25, .5, .75), names = FALSE, type = 7L)
      (q[3L] - 2 * q[2L] + q[1L]) / (q[3L] - q[1L])
    }
  )
}

#' Coeficiente de curtose
#'
#' Calcula curtose excessiva. Metodos: \code{fisher} (default, bias-corrected),
#' \code{pearson} (sem correcao), \code{winkler} (correcao de Winker).
#'
#' @param x Vetor numerico.
#' @param method String: \code{"fisher"}, \code{"pearson"} ou \code{"winkler"}.
#' @param na.rm Logico.
#'
#' @return Escalar numerico.
#'
#' @examples
#' rnp_kurtosis(rnorm(1000))
#' @export
rnp_kurtosis <- function(x, method = c("fisher", "pearson", "winkler"),
                         na.rm = TRUE) {
  abort_numerico(x, "x")
  method <- rlang::arg_match(method)
  if (na.rm) x <- sem_na(x)
  n <- length(x)
  if (n < 4L) {
    rlang::warn("Curtose requer n >= 4. Retornando NA.")
    return(NA_real_)
  }
  mu <- mean(x)
  s <- stats::sd(x)
  if (s == 0) return(NA_real_)
  m4 <- sum((x - mu)^4) / n
  m2 <- sum((x - mu)^2) / n
  g2 <- m4 / (m2^2)
  switch(method,
    fisher  = .curtose_interna(x, mu, s, n),
    pearson = g2 - 3,
    winkler = {
      k <- ((n + 1) * (n - 1)) / ((n - 2) * (n - 3)) * (g2 - 3) +
            6 / (n + 1)
      k - 3
    }
  )
}

#' Deteccao de outliers
#'
#' Identifica outliers por metodo configuravel. Retorna posicoes e valores.
#'
#' @param x Vetor numerico.
#' @param method String: \code{"iqr"} (Tukey), \code{"zscore"},
#'   \code{"modzscore"} (Iglewicz-Cohn), \code{"chebyshev"}.
#' @param k Escalar positivo. Limiar: \code{k} IQR para metodo IQR; \code{k}
#'   desvios para zscore/modified zscore; \code{k} desvios garantidos
#'   (prob. min. 1 - 1/k^2) para Chebyshev.
#' @param na.rm Logico.
#'
#' @return tibble com colunas \code{indice} e \code{valor}.
#'
#' @examples
#' x <- c(rnorm(100), 50, -30)
#' rnp_outliers(x, method = "iqr")
#' rnp_outliers(x, method = "modzscore", k = 3.5)
#' @export
rnp_outliers <- function(x,
                         method = c("iqr", "zscore", "modzscore", "chebyshev"),
                         k = NULL,
                         na.rm = TRUE) {
  abort_numerico(x, "x")
  method <- rlang::arg_match(method)
  if (na.rm) {
    keep <- !is.na(x)
    pos_orig <- which(keep)
    x <- x[keep]
  } else {
    pos_orig <- seq_along(x)
  }
  n <- length(x)
  if (n < 4L) {
    rlang::warn("Deteccao de outliers requer n >= 4. Retornando vazio.")
    return(tibble::tibble(indice = integer(0), valor = double(0)))
  }
  k_default <- switch(method,
    iqr = 1.5, zscore = 3, modzscore = 3.5, chebyshev = 3)
  if (is.null(k)) k <- k_default
  if (!is.numeric(k) || length(k) != 1L || k <= 0) {
    rlang::abort("{.arg k} deve ser escalar positivo.")
  }

  is_out <- switch(method,
    iqr = {
      q <- stats::quantile(x, c(.25, .75), names = FALSE, type = 7L)
      fence <- k * (q[2L] - q[1L])
      (x < q[1L] - fence) | (x > q[2L] + fence)
    },
    zscore = {
      z <- (x - mean(x)) / stats::sd(x)
      abs(z) > k
    },
    modzscore = {
      med <- stats::median(x)
      mad <- stats::mad(x, constant = 1.4826)
      if (mad == 0) {
        rlang::warn("MAD = 0; modified z-score indefinido.")
        return(tibble::tibble(indice = integer(0), valor = double(0)))
      }
      z <- 0.6745 * (x - med) / mad
      abs(z) > k
    },
    chebyshev = {
      s <- stats::sd(x)
      if (s == 0) {
        rlang::warn("Desvio-padrao = 0. Sem dispersao para detectar outliers.")
        return(tibble::tibble(indice = integer(0), valor = double(0)))
      }
      abs(x - mean(x)) > k * s
    }
  )

  idx <- which(is_out)
  tibble::tibble(indice = pos_orig[idx], valor = x[idx])
}
