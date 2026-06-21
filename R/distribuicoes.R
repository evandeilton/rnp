#' Tabela de classes (frequencias agrupadas)
#'
#' Constroi tabela de frequencias para variaveis quantitativas continuas,
#' com regra de amplitude configuravel: Sturges, Scott, Freedman-Diaconis,
#' Rice, Yule, sqrt(n) ou numero fixo de classes.
#'
#' @param x Vetor numerico.
#' @param regra String: \code{"sturges"}, \code{"scott"}, \code{"fd"},
#'   \code{"rice"}, \code{"yule"}, \code{"sqrt"}, \code{"fixa"}.
#' @param k Inteiro. Numero de classes (quando \code{regra = "fixa"}).
#' @param amplitudes Vetor opcional de quebras customizadas. Sobrescreve regra.
#' @param right Logico. Intervalo fechado a direita (ver [cut()]).
#' @param digits Inteiro. Casas decimais.
#'
#' @return tibble com colunas: \code{classe}, \code{lim_inf}, \code{lim_sup},
#'   \code{ponto_medio}, \code{fa}, \code{fr}, \code{fa_acumulada},
#'   \code{fr_acumulada}.
#'
#' @examples
#' rnp_tabela_classes(rnorm(500))
#' rnp_tabela_classes(faithful$eruptions, regra = "scott")
#' rnp_tabela_classes(1:100, regra = "fixa", k = 5)
#' @export
rnp_tabela_classes <- function(x,
                               regra = c("sturges", "scott", "fd", "rice",
                                         "yule", "sqrt", "fixa"),
                               k = NULL,
                               amplitudes = NULL,
                               right = TRUE,
                               digits = 4L) {
  abort_numerico(x, "x", allow_na = TRUE)
  regra <- rlang::arg_match(regra)
  x <- sem_na(x)
  n <- length(x)
  if (n < 2L) rlang::abort("Tabela de classes requer n >= 2.")

  if (!is.null(amplitudes)) {
    breaks <- sort(unique(amplitudes))
    if (length(breaks) < 3L) {
      rlang::abort("{.arg amplitudes} deve ter pelo menos 3 limites distintos.")
    }
    if (min(x) < breaks[1L] || max(x) > breaks[length(breaks)]) {
      rlang::warn("Alguns valores de x estao fora dos limites fornecidos.")
    }
  } else {
    nclass <- switch(regra,
      sturges = grDevices::nclass.Sturges(x),
      scott   = grDevices::nclass.scott(x),
      fd      = grDevices::nclass.FD(x),
      rice    = ceiling(2 * n^(1 / 3)),
      yule    = ceiling(2.5 * log10(length(x)) + 1),
      sqrt    = ceiling(sqrt(length(x))),
      fixa    = {
        if (is.null(k)) rlang::abort("{.arg k} requerido quando regra = 'fixa'.")
        abort_inteiro_pos(k, "k")
        as.integer(k)
      }
    )
    nclass <- max(1L, as.integer(nclass))
    rng <- range(x)
    breaks <- seq(rng[1L], rng[2L], length.out = nclass + 1L)
    breaks[length(breaks)] <- breaks[length(breaks)] + .Machine$double.eps^0.5
  }

  cuts <- cut(x, breaks = breaks, right = right, include.lowest = TRUE)
  fa <- as.integer(table(cuts))
  n_total <- sum(fa)
  fr <- fa / n_total
  fa_acum <- cumsum(fa)
  fr_acum <- cumsum(fr)
  pm <- (breaks[-length(breaks)] + breaks[-1L]) / 2

  tibble::tibble(
    classe        = as.character(levels(cuts)),
    lim_inf       = arredonda(breaks[-length(breaks)], digits),
    lim_sup       = arredonda(breaks[-1L], digits),
    ponto_medio   = arredonda(pm, digits),
    fa            = fa,
    fr            = arredonda(fr, digits),
    fa_acumulada  = fa_acum,
    fr_acumulada  = arredonda(fr_acum, digits)
  )
}

#' Distribuicoes de probabilidade
#'
#' Wrapper unificado para funcoes de densidade (d), cumulativa (p),
#' quantilica (q) e geradora (r) das principais distribuicoes.
#'
#' @param dist String com nome da distribuicao: \code{"norm"}, \code{"t"},
#'   \code{"chisq"}, \code{"f"}, \code{"binom"}, \code{"pois"},
#'   \code{"geom"}, \code{"nbinom"}, \code{"hyper"}, \code{"unif"},
#'   \code{"exp"}, \code{"gamma"}, \code{"weibull"}, \code{"beta"}.
#' @param fun String: \code{"d"} (densidade/massa), \code{"p"} (cumulativa),
#'   \code{"q"} (quantil), \code{"r"} (amostra aleatoria).
#' @param x,q Vetor de quantis.
#' @param p Vetor de probabilidades em \[0, 1\].
#' @param n Inteiro. Tamanho da amostra (apenas para fun = "r").
#' @param ... Argumentos da distribuicao (ex.: \code{mean}, \code{sd},
#'   \code{size}, \code{prob}, \code{lambda}, \code{df}, etc.).
#'
#' @return Vetor numerico.
#'
#' @examples
#' rnp_distribuicao("norm", "d", x = 0, mean = 0, sd = 1)
#' rnp_distribuicao("binom", "p", q = 3, size = 10, prob = 0.4)
#' rnp_distribuicao("pois", "r", n = 5, lambda = 2)
#' @export
rnp_distribuicao <- function(dist = c("norm", "t", "chisq", "f", "binom",
                                      "pois", "geom", "nbinom", "hyper",
                                      "unif", "exp", "gamma", "weibull",
                                      "beta", "lnorm"),
                             fun = c("d", "p", "q", "r"),
                             x = NULL, q = NULL, p = NULL, n = NULL, ...) {
  dist <- rlang::arg_match(dist)
  fun  <- rlang::arg_match(fun)

  fname <- paste0(fun, dist)
  fn <- tryCatch(utils::getFromNamespace(fname, "stats"),
                 error = function(e) NULL)
  if (is.null(fn) || !is.function(fn)) {
    rlang::abort("Funcao de distribuicao '{fname}' nao encontrada em 'stats'.")
  }

  arg <- if (fun == "d") list(x = x) else if (fun == "p") list(q = q) else if (fun == "q") list(p = p) else list(n = n)
  if (fun == "r") abort_inteiro_pos(n %||% 1L, "n")
  if (fun %in% c("d", "p", "q") && (is.null(arg[[1L]]) || length(arg[[1L]]) < 1L)) {
    rlang::abort("Argumento de quantil/probabilidade ausente para fun = '{fun}'.")
  }
  do.call(fn, c(arg, list(...)))
}

#' Esperanca e variancia teoricas
#'
#' Calcula E\[X\] e Var\[X\] para distribuicoes nomeadas comuns a partir
#' dos parametros.
#'
#' @param dist String: \code{"norm"}, \code{"unif"}, \code{"exp"},
#'   \code{"gamma"}, \code{"weibull"}, \code{"binom"}, \code{"pois"},
#'   \code{"geom"}, \code{"nbinom"}, \code{"hyper"}, \code{"beta"}.
#' @param ... Parametros da distribuicao.
#'
#' @return tibble com colunas \code{distribuicao}, \code{esperanca},
#'   \code{variancia}, \code{desvio}.
#'
#' @examples
#' rnp_esperanca_var("binom", size = 10, prob = .5)
#' rnp_esperanca_var("pois", lambda = 3)
#' @export
rnp_esperanca_var <- function(dist = c("norm", "unif", "exp", "gamma",
                                       "weibull", "binom", "pois", "geom",
                                       "nbinom", "hyper", "beta"), ...) {
  dist <- rlang::arg_match(dist)
  args <- list(...)
  ev <- switch(dist,
    norm    = c(mean = args$mean %||% 0,
                var = (args$sd %||% 1)^2),
    unif    = {
      a <- args$min %||% 0; b <- args$max %||% 1
      c((a + b) / 2, (b - a)^2 / 12)
    },
    exp     = {
      r <- args$rate %||% 1
      c(1 / r, 1 / r^2)
    },
    gamma   = {
      sh <- args$shape; ra <- args$rate %||% (1 / (args$scale %||% 1))
      c(sh / ra, sh / ra^2)
    },
    weibull = {
      sh <- args$shape; sc <- args$scale %||% 1
      c(sc * gamma(1 + 1 / sh),
        sc^2 * (gamma(1 + 2 / sh) - gamma(1 + 1 / sh)^2))
    },
    binom   = c(args$size * args$prob, args$size * args$prob * (1 - args$prob)),
    pois    = { la <- args$lambda; c(la, la) },
    geom    = {
      p <- args$prob
      c((1 - p) / p, (1 - p) / p^2)
    },
    nbinom  = {
      n <- args$size; p <- args$prob
      c(n * (1 - p) / p, n * (1 - p) / p^2)
    },
    hyper   = {
      m <- args$m; n <- args$n; k <- args$k
      N <- m + n
      c(k * m / N,
        k * m * n * (N - k) / (N^2 * (N - 1)))
    },
    beta    = {
      a <- args$shape1; b <- args$shape2
      c(a / (a + b),
        a * b / ((a + b)^2 * (a + b + 1)))
    }
  )
  if (anyNA(ev)) {
    rlang::abort("Parametros ausentes para a distribuicao '{dist}'. Verifique os nomes.")
  }
  tibble::tibble(
    distribuicao = dist,
    esperanca    = ev[1L],
    variancia    = ev[2L],
    desvio       = sqrt(ev[2L])
  )
}

`%||%` <- function(a, b) if (is.null(a)) b else a
