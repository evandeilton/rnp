#' Momentos amostrais, assimetria e curtose
#'
#' Calcula momentos centrais ate a ordem desejada, alem de media, variancia,
#' desvio-padrao, coeficiente de assimetria (g1) e curtose em excesso (g2),
#' usando backend C++ (RcppArmadillo) numericamente estavel.
#'
#' @param x Vetor numerico.
#' @param ordem Inteiro >= 2. Ordem maxima dos momentos centrais.
#' @param na.rm Logico. Remove NA antes do calculo.
#' @param digits Inteiro. Casas decimais.
#'
#' @return Uma lista com:
#'   * `resumo`: tibble com `media`, `variancia`, `desvio_padrao`,
#'     `assimetria`, `curtose_excesso`, `n`.
#'   * `momentos`: tibble com `ordem` (2..ordem) e `momento_central`.
#'
#' @examples
#' rnp_momentos(rnorm(500))
#' rnp_momentos(mtcars$mpg, ordem = 6)
#' @family descritiva
#' @export
rnp_momentos <- function(x, ordem = 4L, na.rm = TRUE, digits = 4L) {
  abort_numerico(x, "x")
  if (!is.numeric(ordem) || length(ordem) != 1L || ordem < 2) {
    rlang::abort("{.arg ordem} deve ser um inteiro >= 2.")
  }
  if (na.rm) x <- sem_na(x)
  if (length(x) < 2L) rlang::abort("Necessario n >= 2 observacoes validas.")
  m <- momentos_cpp(as.numeric(x), as.integer(ordem))
  resumo <- tibble::tibble(
    media           = m$media,
    variancia       = m$variancia,
    desvio_padrao   = m$desvio_padrao,
    assimetria      = m$assimetria,
    curtose_excesso = m$curtose_excesso,
    n               = m$n
  ) |>
    dplyr::mutate(dplyr::across(where(is.numeric) & !"n",
                                ~ arredonda(.x, digits)))
  momentos <- tibble::tibble(
    ordem           = seq.int(2L, as.integer(ordem)),
    momento_central = arredonda(m$momentos_centrais[-(1:2)], digits)
  )
  .rnp_lista(list(resumo = resumo, momentos = momentos), "Momentos amostrais")
}

#' Numero/limites de classes por regra classica
#'
#' Calcula o numero de classes e os limites para agrupamento de uma variavel
#' continua, segundo a regra escolhida (Sturges, Scott, Freedman-Diaconis,
#' raiz, Rice ou numero fixo).
#'
#' @param x Vetor numerico.
#' @param regra String: `"sturges"`, `"scott"`, `"fd"`, `"sqrt"`, `"rice"`
#'   ou `"fixa"`.
#' @param k Inteiro. Numero de classes quando `regra = "fixa"`.
#' @param digits Inteiro. Casas decimais.
#'
#' @return Uma lista com `n_classes`, `amplitude_classe` e o vetor `limites`.
#'
#' @examples
#' rnp_intervalo_classes(rnorm(1000))
#' rnp_intervalo_classes(mtcars$mpg, regra = "fd")
#' @family descritiva
#' @export
rnp_intervalo_classes <- function(x, regra = c("sturges", "scott", "fd",
                                               "sqrt", "rice", "fixa"),
                                  k = NULL, digits = 4L) {
  abort_numerico(x, "x")
  regra <- rlang::arg_match(regra)
  x <- sem_na(x)
  n <- length(x)
  if (n < 2L) rlang::abort("Necessario n >= 2 observacoes validas.")
  nc <- switch(regra,
    sturges = grDevices::nclass.Sturges(x),
    scott   = grDevices::nclass.scott(x),
    fd      = grDevices::nclass.FD(x),
    sqrt    = ceiling(sqrt(n)),
    rice    = ceiling(2 * n^(1 / 3)),
    fixa    = {
      if (is.null(k)) rlang::abort("{.arg k} requerido quando regra = 'fixa'.")
      abort_inteiro_pos(k, "k")
      as.integer(k)
    }
  )
  nc <- max(1L, as.integer(nc))
  rng <- range(x)
  limites <- seq(rng[1L], rng[2L], length.out = nc + 1L)
  list(
    n_classes        = nc,
    amplitude_classe = arredonda((rng[2L] - rng[1L]) / nc, digits),
    limites          = arredonda(limites, digits)
  )
}

#' Tabela de frequencias para variavel categorica
#'
#' Constroi tabela de frequencias absolutas e relativas (simples e
#' acumuladas) para um vetor categorico ou fator.
#'
#' @param x Vetor categorico, fator ou caractere.
#' @param ordenar Logico. Ordena por frequencia decrescente.
#' @param digits Inteiro. Casas decimais.
#'
#' @return tibble com `categoria`, `fa`, `fr`, `fa_acumulada`, `fr_acumulada`.
#'
#' @examples
#' rnp_tabela_frequencia(mtcars$cyl)
#' rnp_tabela_frequencia(letters[sample(3, 50, TRUE)], ordenar = TRUE)
#' @family descritiva
#' @export
rnp_tabela_frequencia <- function(x, ordenar = FALSE, digits = 4L) {
  if (is.null(x) || !is.null(dim(x))) {
    rlang::abort("{.arg x} deve ser um vetor categorico/fator.")
  }
  tb <- table(x, useNA = "no")
  if (ordenar) tb <- sort(tb, decreasing = TRUE)
  fa <- as.integer(tb)
  total <- sum(fa)
  tibble::tibble(
    categoria    = names(tb),
    fa           = fa,
    fr           = arredonda(fa / total, digits),
    fa_acumulada = cumsum(fa),
    fr_acumulada = arredonda(cumsum(fa) / total, digits)
  )
}

#' Tabela de contingencia (dupla entrada)
#'
#' Tabela de frequencias conjuntas de duas variaveis categoricas, com
#' frequencias relativas total, por linha e por coluna, e marginais.
#' Substitui a antiga `rnp_2freq` (cuja frequencia relativa estava incorreta).
#'
#' @param x,y Vetores categoricos/fatores de mesmo comprimento.
#' @param tipo String: `"fa"` (absoluta), `"fr"` (relativa ao total),
#'   `"fr_linha"` (relativa por linha) ou `"fr_coluna"` (relativa por coluna).
#' @param digits Inteiro. Casas decimais.
#'
#' @return tibble em formato largo com a variavel `x` nas linhas, as categorias
#'   de `y` nas colunas e a marginal `Total`.
#'
#' @examples
#' rnp_tabela_contingencia(mtcars$cyl, mtcars$gear)
#' rnp_tabela_contingencia(mtcars$cyl, mtcars$gear, tipo = "fr_linha")
#' @family descritiva
#' @export
rnp_tabela_contingencia <- function(x, y,
                                    tipo = c("fa", "fr", "fr_linha", "fr_coluna"),
                                    digits = 4L) {
  if (length(x) != length(y)) {
    rlang::abort("{.arg x} e {.arg y} devem ter o mesmo comprimento.")
  }
  tipo <- rlang::arg_match(tipo)
  tb <- table(x, y)
  m <- switch(tipo,
    fa        = tb,
    fr        = tb / sum(tb),
    fr_linha  = sweep(tb, 1L, rowSums(tb), "/"),
    fr_coluna = sweep(tb, 2L, colSums(tb), "/")
  )
  m <- unclass(as.matrix(m))
  dimnames(m) <- list(rownames(m), colnames(m))
  total_col <- switch(tipo,
    fa = rowSums(m), fr = rowSums(m),
    fr_linha = rep(1, nrow(m)), fr_coluna = rowSums(tb) / sum(tb))
  corpo <- tibble::as_tibble(as.data.frame.matrix(m))
  if (tipo != "fa") {
    corpo <- dplyr::mutate(corpo, dplyr::across(dplyr::everything(),
                                                ~ arredonda(.x, digits)))
  }
  out <- tibble::tibble(categoria = rownames(m))
  out <- dplyr::bind_cols(out, corpo)
  out$Total <- arredonda(total_col, digits)
  out
}
