#' Tamanho de efeito
#'
#' Calcula medidas de tamanho de efeito: Cohen's d, Hedges' g, r, eta-quadrado.
#'
#' @param x,y Vetores numericos (para d, g, r) ou objeto \code{aov} (para eta2).
#' @param paired Logico. Amostras pareadas (apenas para d, g, r).
#' @param digits Inteiro.
#'
#' @return tibble com \code{cohens_d}, \code{hedges_g}, \code{r}, \code{eta2}.
#'
#' @examples
#' rnp_tamanho_efeito(rnorm(30, 5), rnorm(30, 5.5))
#' @export
rnp_tamanho_efeito <- function(x, y = NULL, paired = FALSE, digits = 4L) {
  if (inherits(x, "aov")) {
    sm <- summary(x)[[1L]]
    ss_between <- sm$`Sum Sq`[1L]
    ss_total <- sum(sm$`Sum Sq`)
    eta2 <- ss_between / ss_total
    return(tibble::tibble(
      cohens_d = NA_real_,
      hedges_g = NA_real_,
      r        = NA_real_,
      eta2     = eta2
    ) |> dplyr::mutate(dplyr::across(where(is.numeric),
                                     ~ arredonda(.x, digits))))
  }
  abort_numerico(x, "x")
  if (is.null(y)) rlang::abort("{.arg y} necessario para comparacao.")
  abort_numerico(y, "y")
  if (paired) {
    if (length(x) != length(y)) rlang::abort("Amostras pareadas requerem mesmo n.")
    d <- x - y
    m <- mean(d)
    s <- stats::sd(d)
    n <- length(d)
    if (s == 0) return(tibble::tibble(cohens_d = 0, hedges_g = 0, r = 0, eta2 = 0))
    d_cohen <- m / s
    j <- 1 - 3 / (4 * (n - 1) - 1)
    g <- d_cohen * j
    r <- d_cohen / sqrt(d_cohen^2 + (n - 1)^2 / (n - 2))
    eta2 <- d_cohen^2 / (d_cohen^2 + 4)
  } else {
    n1 <- length(x); n2 <- length(y)
    m1 <- mean(x); m2 <- mean(y)
    s1 <- stats::var(x); s2 <- stats::var(y)
    sp <- sqrt(((n1 - 1) * s1 + (n2 - 1) * s2) / (n1 + n2 - 2))
    if (sp == 0) return(tibble::tibble(cohens_d = 0, hedges_g = 0, r = 0, eta2 = 0))
    d_cohen <- (m1 - m2) / sp
    n_total <- n1 + n2
    j <- 1 - 3 / (4 * (n_total - 2) - 1)
    g <- d_cohen * j
    r <- d_cohen / sqrt(d_cohen^2 + (n_total - 2))
    eta2 <- d_cohen^2 / (d_cohen^2 + (n1 + n2 - 2) / (n1 * n2 / (n1 + n2)))
  }
  tibble::tibble(
    cohens_d = d_cohen,
    hedges_g = g,
    r        = r,
    eta2     = eta2
  ) |> dplyr::mutate(dplyr::across(where(is.numeric),
                                   ~ arredonda(.x, digits)))
}

#' Resumo de valores faltantes
#'
#' Mapa de missing por variavel e observacao.
#'
#' @param base data.frame.
#' @param digits Inteiro.
#'
#' @return lista:
#'   * \code{por_variavel}: tibble com variavel, n_faltantes, percentual.
#'   * \code{por_observacao}: tibble com observacao, n_faltantes, percentual.
#'   * \code{padrao}: tibble com padroes de missing (se naniar disponivel).
#'
#' @examples
#' rnp_na_summary(airquality)
#' @export
rnp_na_summary <- function(base, digits = 4L) {
  if (!is.data.frame(base)) rlang::abort("{.arg base} deve ser data.frame.")
  n_obs <- nrow(base)
  n_vars <- ncol(base)
  por_var <- tibble::tibble(
    variavel    = names(base),
    n_faltantes = vapply(base, function(v) sum(is.na(v)), integer(1)),
    percentual  = n_faltantes / n_obs
  )
  por_obs <- tibble::tibble(
    observacao  = seq_len(n_obs),
    n_faltantes = rowSums(is.na(base)),
    percentual  = n_faltantes / n_vars
  )
  padrao <- NULL
  if (tem_pacote("naniar")) {
    padrao <- tryCatch(
      naniar::miss_var_summary(base),
      error = function(e) NULL
    )
  }
  list(
    por_variavel = por_var |> dplyr::mutate(dplyr::across(where(is.numeric),
                                                          ~ arredonda(.x, digits))),
    por_observacao = por_obs |> dplyr::mutate(dplyr::across(where(is.numeric),
                                                            ~ arredonda(.x, digits))),
    padrao = padrao
  )
}

#' Potencia estatistica
#'
#' Wrapper para calculos de potencia usando o pacote \code{pwr}.
#'
#' @param teste String: \code{"t.test"}, \code{"t2n"}, \code{"anova"},
#'   \code{"chisq"}, \code{"r"}, \code{"p"}, \code{"norm"}, \code{"f2"}.
#' @param ... Argumentos do teste (ex.: \code{n}, \code{d}, \code{sig.level}, \code{power}).
#'
#' @return Objeto \code{pwr.test} ou tibble com resultado.
#'
#' @examples
#' rnp_potencia("t.test", d = 0.5, n = 30, sig.level = 0.05)
#' @export
rnp_potencia <- function(teste = c("t.test", "t2n", "anova", "chisq", "r",
                                   "p", "norm", "f2"), ...) {
  precisa_pacote("pwr", "rnp_potencia")
  teste <- rlang::arg_match(teste)
  fname <- paste0("pwr.", teste)
  fn <- utils::getFromNamespace(fname, "pwr")
  fn(...)
}
