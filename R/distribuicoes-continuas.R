# Wrappers didaticos das distribuicoes de probabilidade (FATIA 1, F-09..F-17,
# F-62..F-65). Todos delegam para rnp_distribuicao() (que chama stats::), exceto
# casos multivariados. Mantemos os nomes de parametro das funcoes em PT-BR.

#' Distribuicao Normal
#'
#' @param fun String: `"d"`, `"p"`, `"q"`, `"r"`.
#' @param media,dp Media e desvio-padrao.
#' @param x,q,p,n Argumentos conforme `fun`.
#' @return Vetor numerico.
#' @examples
#' rnp_distribuicao_normal("d", x = 0)
#' rnp_distribuicao_normal("p", q = 1.96)
#' @family distribuicoes
#' @export
rnp_distribuicao_normal <- function(fun = c("d", "p", "q", "r"), media = 0, dp = 1,
                                    x = NULL, q = NULL, p = NULL, n = NULL) {
  if (dp <= 0) rlang::abort("{.arg dp} deve ser positivo.")
  rnp_distribuicao("norm", fun, x = x, q = q, p = p, n = n, mean = media, sd = dp)
}

#' Distribuicao Exponencial
#'
#' @inheritParams rnp_distribuicao_normal
#' @param taxa Parametro de taxa (rate), positivo.
#' @return Vetor numerico.
#' @examples
#' rnp_distribuicao_exponencial("p", q = 1, taxa = 0.5)
#' @family distribuicoes
#' @export
rnp_distribuicao_exponencial <- function(fun = c("d", "p", "q", "r"), taxa = 1,
                                         x = NULL, q = NULL, p = NULL, n = NULL) {
  if (taxa <= 0) rlang::abort("{.arg taxa} deve ser positivo.")
  rnp_distribuicao("exp", fun, x = x, q = q, p = p, n = n, rate = taxa)
}

#' Distribuicao Gama
#'
#' @inheritParams rnp_distribuicao_normal
#' @param forma,taxa Parametros de forma (shape) e taxa (rate), positivos.
#' @return Vetor numerico.
#' @examples
#' rnp_distribuicao_gama("d", x = 2, forma = 2, taxa = 1)
#' @family distribuicoes
#' @export
rnp_distribuicao_gama <- function(fun = c("d", "p", "q", "r"), forma, taxa = 1,
                                  x = NULL, q = NULL, p = NULL, n = NULL) {
  if (missing(forma) || forma <= 0 || taxa <= 0) {
    rlang::abort("{.arg forma} e {.arg taxa} devem ser positivos.")
  }
  rnp_distribuicao("gamma", fun, x = x, q = q, p = p, n = n,
                   shape = forma, rate = taxa)
}

#' Distribuicao Beta
#'
#' @inheritParams rnp_distribuicao_normal
#' @param a,b Parametros de forma (shape1, shape2), positivos.
#' @return Vetor numerico.
#' @examples
#' rnp_distribuicao_beta("d", x = 0.5, a = 2, b = 2)
#' @family distribuicoes
#' @export
rnp_distribuicao_beta <- function(fun = c("d", "p", "q", "r"), a, b,
                                  x = NULL, q = NULL, p = NULL, n = NULL) {
  if (missing(a) || missing(b) || a <= 0 || b <= 0) {
    rlang::abort("{.arg a} e {.arg b} devem ser positivos.")
  }
  rnp_distribuicao("beta", fun, x = x, q = q, p = p, n = n, shape1 = a, shape2 = b)
}

#' Distribuicao Uniforme continua
#'
#' @inheritParams rnp_distribuicao_normal
#' @param min,max Limites do intervalo.
#' @return Vetor numerico.
#' @examples
#' rnp_distribuicao_uniforme("p", q = 0.5, min = 0, max = 1)
#' @family distribuicoes
#' @export
rnp_distribuicao_uniforme <- function(fun = c("d", "p", "q", "r"), min = 0, max = 1,
                                      x = NULL, q = NULL, p = NULL, n = NULL) {
  if (max <= min) rlang::abort("{.arg max} deve ser maior que {.arg min}.")
  rnp_distribuicao("unif", fun, x = x, q = q, p = p, n = n, min = min, max = max)
}

#' Distribuicao t de Student
#'
#' @inheritParams rnp_distribuicao_normal
#' @param gl Graus de liberdade, positivo.
#' @return Vetor numerico.
#' @examples
#' rnp_distribuicao_t("q", p = 0.975, gl = 10)
#' @family distribuicoes
#' @export
rnp_distribuicao_t <- function(fun = c("d", "p", "q", "r"), gl,
                               x = NULL, q = NULL, p = NULL, n = NULL) {
  if (missing(gl) || gl <= 0) rlang::abort("{.arg gl} deve ser positivo.")
  rnp_distribuicao("t", fun, x = x, q = q, p = p, n = n, df = gl)
}

#' Distribuicao Qui-quadrado
#'
#' @inheritParams rnp_distribuicao_t
#' @return Vetor numerico.
#' @examples
#' rnp_distribuicao_qui_quadrado("q", p = 0.95, gl = 3)
#' @family distribuicoes
#' @export
rnp_distribuicao_qui_quadrado <- function(fun = c("d", "p", "q", "r"), gl,
                                          x = NULL, q = NULL, p = NULL, n = NULL) {
  if (missing(gl) || gl <= 0) rlang::abort("{.arg gl} deve ser positivo.")
  rnp_distribuicao("chisq", fun, x = x, q = q, p = p, n = n, df = gl)
}

#' Distribuicao F
#'
#' @inheritParams rnp_distribuicao_normal
#' @param gl1,gl2 Graus de liberdade do numerador e denominador.
#' @return Vetor numerico.
#' @examples
#' rnp_distribuicao_f("q", p = 0.95, gl1 = 3, gl2 = 20)
#' @family distribuicoes
#' @export
rnp_distribuicao_f <- function(fun = c("d", "p", "q", "r"), gl1, gl2,
                               x = NULL, q = NULL, p = NULL, n = NULL) {
  if (missing(gl1) || missing(gl2) || gl1 <= 0 || gl2 <= 0) {
    rlang::abort("{.arg gl1} e {.arg gl2} devem ser positivos.")
  }
  rnp_distribuicao("f", fun, x = x, q = q, p = p, n = n, df1 = gl1, df2 = gl2)
}

#' Distribuicao Log-normal
#'
#' @inheritParams rnp_distribuicao_normal
#' @param media_log,dp_log Media e desvio-padrao na escala logaritmica.
#' @return Vetor numerico.
#' @examples
#' rnp_distribuicao_lognormal("d", x = 1)
#' @family distribuicoes
#' @export
rnp_distribuicao_lognormal <- function(fun = c("d", "p", "q", "r"),
                                       media_log = 0, dp_log = 1,
                                       x = NULL, q = NULL, p = NULL, n = NULL) {
  if (dp_log <= 0) rlang::abort("{.arg dp_log} deve ser positivo.")
  rnp_distribuicao("lnorm", fun, x = x, q = q, p = p, n = n,
                   meanlog = media_log, sdlog = dp_log)
}

#' Distribuicao Weibull
#'
#' @inheritParams rnp_distribuicao_normal
#' @param forma,escala Parametros de forma (shape) e escala (scale), positivos.
#' @return Vetor numerico.
#' @examples
#' rnp_distribuicao_weibull("p", q = 1, forma = 1.5, escala = 1)
#' @family distribuicoes
#' @export
rnp_distribuicao_weibull <- function(fun = c("d", "p", "q", "r"),
                                     forma, escala = 1,
                                     x = NULL, q = NULL, p = NULL, n = NULL) {
  if (missing(forma) || forma <= 0 || escala <= 0) {
    rlang::abort("{.arg forma} e {.arg escala} devem ser positivos.")
  }
  rnp_distribuicao("weibull", fun, x = x, q = q, p = p, n = n,
                   shape = forma, scale = escala)
}

#' Distribuicao Multinomial
#'
#' @param fun String: `"d"` (massa conjunta) ou `"r"` (amostra).
#' @param tamanho Numero de ensaios (size).
#' @param probs Vetor de probabilidades das categorias (soma 1).
#' @param x Vetor de contagens (para `fun = "d"`), de mesmo tamanho de `probs`.
#' @param n Numero de amostras (para `fun = "r"`).
#' @return Escalar (d) ou matriz de contagens (r).
#' @examples
#' rnp_distribuicao_multinomial("d", tamanho = 10, probs = c(.2, .3, .5),
#'                              x = c(2, 3, 5))
#' rnp_distribuicao_multinomial("r", tamanho = 10, probs = c(.2, .3, .5), n = 4)
#' @family distribuicoes
#' @export
rnp_distribuicao_multinomial <- function(fun = c("d", "r"), tamanho, probs,
                                         x = NULL, n = NULL) {
  fun <- rlang::arg_match(fun)
  abort_inteiro_pos(tamanho, "tamanho")
  if (abs(sum(probs) - 1) > 1e-6) rlang::abort("{.arg probs} deve somar 1.")
  switch(fun,
    d = {
      if (is.null(x) || length(x) != length(probs)) {
        rlang::abort("{.arg x} deve ter o mesmo comprimento de {.arg probs}.")
      }
      stats::dmultinom(x = x, size = tamanho, prob = probs)
    },
    r = {
      abort_inteiro_pos(n %||% 1L, "n")
      t(stats::rmultinom(n = n, size = tamanho, prob = probs))
    }
  )
}

#' Grafico de uma distribuicao de probabilidade
#'
#' Plota a densidade (continuas) ou a massa (discretas) de uma distribuicao,
#' usando os wrappers `rnp_distribuicao_*`.
#'
#' @param dist String com o nome da distribuicao para [rnp_distribuicao()]
#'   (ex.: `"norm"`, `"binom"`, `"pois"`, `"gamma"`).
#' @param ... Parametros da distribuicao (ex.: `mean`, `sd`, `size`, `prob`).
#' @param limites Vetor de 2 valores com o intervalo do eixo x. Se `NULL`,
#'   escolhido automaticamente.
#' @param discreta Logico. Trata como discreta (barras). Se `NULL`, inferido.
#' @param titulo Titulo opcional.
#'
#' @return Objeto `ggplot`.
#'
#' @examples
#' rnp_grafico_distribuicao("norm", mean = 0, sd = 1)
#' rnp_grafico_distribuicao("binom", size = 10, prob = 0.3)
#' @family distribuicoes
#' @export
rnp_grafico_distribuicao <- function(dist, ..., limites = NULL,
                                     discreta = NULL, titulo = NULL) {
  params <- list(...)
  discretas <- c("binom", "pois", "geom", "nbinom", "hyper")
  if (is.null(discreta)) discreta <- dist %in% discretas
  if (is.null(limites)) {
    limites <- if (discreta) {
      c(0, do.call(rnp_distribuicao, c(list(dist, "q", p = 0.999), params)))
    } else {
      c(do.call(rnp_distribuicao, c(list(dist, "q", p = 0.001), params)),
        do.call(rnp_distribuicao, c(list(dist, "q", p = 0.999), params)))
    }
  }
  if (discreta) {
    xs <- seq(floor(limites[1L]), ceiling(limites[2L]), by = 1)
    ys <- do.call(rnp_distribuicao, c(list(dist, "d", x = xs), params))
    dados <- tibble::tibble(x = xs, prob = ys)
    g <- ggplot2::ggplot(dados, ggplot2::aes(x = .data$x, y = .data$prob)) +
      ggplot2::geom_col(fill = rnp_paleta_rnp("rnp_qual", 1), alpha = 0.85) +
      ggplot2::labs(y = "P(X = x)")
  } else {
    xs <- seq(limites[1L], limites[2L], length.out = 512)
    ys <- do.call(rnp_distribuicao, c(list(dist, "d", x = xs), params))
    dados <- tibble::tibble(x = xs, densidade = ys)
    g <- ggplot2::ggplot(dados, ggplot2::aes(x = .data$x, y = .data$densidade)) +
      ggplot2::geom_area(fill = rnp_paleta_rnp("rnp_qual", 1), alpha = 0.5) +
      ggplot2::geom_line(color = rnp_paleta_rnp("rnp_qual", 1), linewidth = 1) +
      ggplot2::labs(y = "Densidade")
  }
  g + rnp_tema_rnp() +
    ggplot2::labs(title = titulo %||% glue::glue("Distribuicao '{dist}'"), x = "x")
}

#' Ajuste de distribuicao por maxima verossimilhanca
#'
#' Ajusta os parametros de uma distribuicao a um vetor de dados via maxima
#' verossimilhanca (otimizacao numerica) e reporta log-verossimilhanca, AIC,
#' BIC e a estatistica de Kolmogorov-Smirnov.
#'
#' @param x Vetor numerico de dados.
#' @param dist String: `"norm"`, `"exp"`, `"gamma"`, `"lnorm"`, `"weibull"`,
#'   `"pois"`.
#' @param digits Inteiro. Casas decimais.
#'
#' @return Uma lista com `parametros` (tibble) e `qualidade` (tibble com
#'   `log_veross`, `aic`, `bic`, `ks_estatistica`, `n`).
#'
#' @examples
#' set.seed(1)
#' rnp_ajuste_distribuicao(rexp(200, 0.5), "exp")
#' rnp_ajuste_distribuicao(rnorm(200, 10, 2), "norm")
#' @family distribuicoes
#' @export
rnp_ajuste_distribuicao <- function(x, dist = c("norm", "exp", "gamma",
                                                "lnorm", "weibull", "pois"),
                                    digits = 4L) {
  abort_numerico(x, "x")
  dist <- rlang::arg_match(dist)
  x <- sem_na(x)
  n <- length(x)
  if (n < 2L) rlang::abort("Necessario n >= 2 observacoes validas.")
  if (dist %in% c("exp", "gamma", "lnorm", "weibull") && any(x <= 0)) {
    rlang::abort("Distribuicao '{dist}' requer dados positivos.")
  }

  fit <- switch(dist,
    norm = list(par = c(mean = mean(x), sd = stats::sd(x)),
                dens = function(x, p) stats::dnorm(x, p[1], p[2]),
                cdf  = function(q, p) stats::pnorm(q, p[1], p[2])),
    exp  = list(par = c(rate = 1 / mean(x)),
                dens = function(x, p) stats::dexp(x, p[1]),
                cdf  = function(q, p) stats::pexp(q, p[1])),
    pois = list(par = c(lambda = mean(x)),
                dens = function(x, p) stats::dpois(x, p[1]),
                cdf  = function(q, p) stats::ppois(q, p[1])),
    gamma = {
      m <- mean(x); v <- stats::var(x)
      ini <- c(shape = m^2 / v, rate = m / v)
      nll <- function(p) if (any(p <= 0)) Inf else
        -sum(stats::dgamma(x, p[1], p[2], log = TRUE))
      op <- stats::optim(ini, nll)
      list(par = stats::setNames(op$par, c("shape", "rate")),
           dens = function(x, p) stats::dgamma(x, p[1], p[2]),
           cdf  = function(q, p) stats::pgamma(q, p[1], p[2]))
    },
    lnorm = list(par = c(meanlog = mean(log(x)), sdlog = stats::sd(log(x))),
                 dens = function(x, p) stats::dlnorm(x, p[1], p[2]),
                 cdf  = function(q, p) stats::plnorm(q, p[1], p[2])),
    weibull = {
      ini <- c(shape = 1.2, scale = mean(x))
      nll <- function(p) if (any(p <= 0)) Inf else
        -sum(stats::dweibull(x, p[1], p[2], log = TRUE))
      op <- stats::optim(ini, nll)
      list(par = stats::setNames(op$par, c("shape", "scale")),
           dens = function(x, p) stats::dweibull(x, p[1], p[2]),
           cdf  = function(q, p) stats::pweibull(q, p[1], p[2]))
    }
  )

  par <- fit$par
  ll <- sum(log(pmax(fit$dens(x, par), .Machine$double.xmin)))
  k <- length(par)
  aic <- -2 * ll + 2 * k
  bic <- -2 * ll + k * log(n)
  # estatistica KS: max |Fn(x) - F(x)|
  xs <- sort(x)
  fn <- seq_len(n) / n
  ks <- max(abs(fn - fit$cdf(xs, par)))

  list(
    parametros = tibble::tibble(
      parametro = names(par),
      estimativa = arredonda(unname(par), digits)
    ),
    qualidade = tibble::tibble(
      log_veross     = arredonda(ll, digits),
      aic            = arredonda(aic, digits),
      bic            = arredonda(bic, digits),
      ks_estatistica = arredonda(ks, digits),
      n              = n
    )
  )
}
