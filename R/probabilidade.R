#' Combinatorial utilities
#'
#' Calcula numero de combinacoes, arranjos, permutacoes com e sem
#' repeticao usando aritmetica de lgamma para evitar overflow.
#'
#' @param n Inteiro. Total de elementos.
#' @param k Inteiro. Tamanho do subconjunto.
#' @param repeticao Logico. Com repeticao (TRUE) ou sem (FALSE).
#'
#' @return Escalar numerico (double para suportar grandes valores).
#'
#' @examples
#' rnp_combinacao(10, 3)
#' rnp_combinacao(10, 3, repeticao = TRUE)
#' @export
rnp_combinacao <- function(n, k, repeticao = FALSE) {
  abort_inteiro_pos(n, "n")
  abort_inteiro_pos(k, "k")
  if (!repeticao) {
    if (k > n) return(0)
    exp(lgamma(n + 1) - lgamma(k + 1) - lgamma(n - k + 1))
  } else {
    exp(lgamma(n + k - 1 + 1) - lgamma(k + 1) - lgamma(n - 1 + 1))
  }
}

#' Numero de arranjos
#'
#' @inheritParams rnp_combinacao
#' @return Escalar numerico.
#' @examples
#' rnp_arranjo(10, 3)
#' @export
rnp_arranjo <- function(n, k) {
  abort_inteiro_pos(n, "n")
  abort_inteiro_pos(k, "k")
  if (k > n) return(0)
  exp(lgamma(n + 1) - lgamma(n - k + 1))
}

#' Numero de permutacoes
#'
#' @param n Inteiro. Total de elementos.
#' @param elementos Vetor opcional de frequencias (permutacao com repeticao).
#' @return Escalar numerico.
#' @examples
#' rnp_permutacao(5)
#' rnp_permutacao(7, elementos = c(2, 2, 3))
#' @export
rnp_permutacao <- function(n, elementos = NULL) {
  abort_inteiro_pos(n, "n")
  if (is.null(elementos)) {
    exp(lgamma(n + 1))
  } else {
    elementos <- as.integer(elementos)
    if (anyNA(elementos) || any(elementos < 0L)) {
      rlang::abort("{.arg elementos} deve conter inteiros nao-negativos.")
    }
    if (sum(elementos) != n) {
      rlang::warn("Soma dos elementos != n. Resultado pode nao fazer sentido.")
    }
    exp(lgamma(n + 1) - sum(lgamma(elementos + 1)))
  }
}

#' Probabilidade condicional
#'
#' Calcula P(A|B) = P(A & B) / P(B).
#'
#' @param pa_e_b Numerico em \[0, 1\]. Probabilidade da interseccao.
#' @param pb Numerico em (0, 1]. Probabilidade do evento condicionante.
#' @return Escalar numerico.
#' @examples
#' rnp_probabilidade_condicional(0.12, 0.30)
#' @export
rnp_probabilidade_condicional <- function(pa_e_b, pb) {
  abort_proporcao(pa_e_b, "pa_e_b")
  if (!is.numeric(pb) || length(pb) != 1L || is.na(pb) || pb <= 0 || pb > 1) {
    rlang::abort("{.arg pb} deve estar em (0, 1].")
  }
  pa_e_b / pb
}

#' Distribuicao binomial (wrapper)
#'
#' Atalho para \code{rnp_distribuicao("binom", ...)}.
#'
#' @param fun String: \code{"d"} (massa), \code{"p"} (cumulativa),
#'   \code{"q"} (quantil), \code{"r"} (amostra).
#' @param size Numero de ensaios.
#' @param prob Probabilidade de sucesso.
#' @param x,q,p,n Argumentos conforme \code{fun}.
#' @return Vetor numerico.
#' @examples
#' rnp_distribuicao_binomial("d", x = 0:5, size = 10, prob = .3)
#' rnp_distribuicao_binomial("p", q = 5, size = 10, prob = .3)
#' @export
rnp_distribuicao_binomial <- function(fun = c("d", "p", "q", "r"),
                                      size, prob,
                                      x = NULL, q = NULL, p = NULL, n = NULL) {
  abort_inteiro_pos(size, "size")
  abort_proporcao(prob, "prob")
  rnp_distribuicao("binom", fun, x = x, q = q, p = p, n = n,
                   size = size, prob = prob)
}

#' Distribuicao de Poisson (wrapper)
#'
#' @param lambda Taxa media (positivo).
#' @inheritParams rnp_distribuicao_binomial
#' @return Vetor numerico.
#' @examples
#' rnp_distribuicao_poisson("d", x = 0:5, lambda = 2)
#' @export
rnp_distribuicao_poisson <- function(fun = c("d", "p", "q", "r"),
                                     lambda, x = NULL, q = NULL, p = NULL, n = NULL) {
  if (!is.numeric(lambda) || length(lambda) != 1L || is.na(lambda) || lambda <= 0) {
    rlang::abort("{.arg lambda} deve ser escalar positivo.")
  }
  rnp_distribuicao("pois", fun, x = x, q = q, p = p, n = n, lambda = lambda)
}

#' Distribuicao hipergeometrica (wrapper)
#'
#' @param m Numero de sucessos na populacao.
#' @param nn Numero de fracassos na populacao (n em \code{stats::dhyper}).
#' @param k Tamanho da amostra.
#' @inheritParams rnp_distribuicao_binomial
#' @return Vetor numerico.
#' @examples
#' rnp_distribuicao_hipergeometrica("d", x = 0:3, m = 7, nn = 3, k = 5)
#' @export
rnp_distribuicao_hipergeometrica <- function(fun = c("d", "p", "q", "r"),
                                             m, nn, k,
                                             x = NULL, q = NULL, p = NULL, n = NULL) {
  abort_inteiro_pos(m, "m")
  abort_inteiro_pos(nn, "nn")
  abort_inteiro_pos(k, "k")
  fun <- rlang::arg_match(fun)
  # Chamada direta a stats: o argumento 'n' da hipergeometrica (numero de
  # fracassos na populacao) colide com o 'n' (tamanho de amostra) do wrapper
  # generico, por isso nao usamos rnp_distribuicao() aqui.
  switch(fun,
    d = stats::dhyper(x = x, m = m, n = nn, k = k),
    p = stats::phyper(q = q, m = m, n = nn, k = k),
    q = stats::qhyper(p = p, m = m, n = nn, k = k),
    r = {
      abort_inteiro_pos(n %||% 1L, "n")
      stats::rhyper(nn = n, m = m, n = nn, k = k)
    }
  )
}

#' Distribuicao geometrica (wrapper)
#'
#' @param prob Probabilidade de sucesso em cada ensaio.
#' @inheritParams rnp_distribuicao_binomial
#' @return Vetor numerico.
#' @examples
#' rnp_distribuicao_geom("d", x = 0:5, prob = .2)
#' @export
rnp_distribuicao_geom <- function(fun = c("d", "p", "q", "r"),
                                  prob, x = NULL, q = NULL, p = NULL, n = NULL) {
  abort_proporcao(prob, "prob")
  rnp_distribuicao("geom", fun, x = x, q = q, p = p, n = n, prob = prob)
}

#' Distribuicao binomial negativa (wrapper)
#'
#' @param size Numero de sucessos alvo.
#' @param prob Probabilidade de sucesso em cada ensaio.
#' @inheritParams rnp_distribuicao_binomial
#' @return Vetor numerico.
#' @examples
#' rnp_distribuicao_negbinom("d", x = 0:5, size = 3, prob = .5)
#' @export
rnp_distribuicao_negbinom <- function(fun = c("d", "p", "q", "r"),
                                      size, prob,
                                      x = NULL, q = NULL, p = NULL, n = NULL) {
  abort_inteiro_pos(size, "size")
  abort_proporcao(prob, "prob")
  rnp_distribuicao("nbinom", fun, x = x, q = q, p = p, n = n,
                   size = size, prob = prob)
}
