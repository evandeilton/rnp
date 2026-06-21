# Multivariada: analise fatorial, correspondencia, biplot e matriz de dispersao.

#' Analise fatorial exploratoria
#'
#' Estima cargas fatoriais por [stats::factanal()] (maxima verossimilhanca).
#'
#' @param base data.frame ou matriz numerica.
#' @param n_fatores Inteiro. Numero de fatores.
#' @param rotacao String: `"varimax"`, `"promax"` ou `"none"`.
#' @param digits Inteiro.
#'
#' @return Uma lista com `cargas` (tibble), `variancia` (tibble) e `modelo`.
#'
#' @examples
#' rnp_analise_fatorial(mtcars[, c("mpg", "disp", "hp", "drat", "wt", "qsec")],
#'                      n_fatores = 2)$cargas
#' @family multivariada
#' @export
rnp_analise_fatorial <- function(base, n_fatores = 2L,
                                 rotacao = c("varimax", "promax", "none"),
                                 digits = 4L) {
  rotacao <- rlang::arg_match(rotacao)
  abort_inteiro_pos(n_fatores, "n_fatores")
  base <- as.data.frame(base)
  num <- vapply(base, is.numeric, logical(1))
  X <- base[stats::complete.cases(base[, num]), num, drop = FALSE]
  fit <- stats::factanal(X, factors = n_fatores, rotation = rotacao)
  L <- unclass(fit$loadings)
  cargas <- tibble::as_tibble(as.data.frame.matrix(L)) |>
    stats::setNames(paste0("Fator", seq_len(ncol(L))))
  cargas <- dplyr::bind_cols(variavel = rownames(L), cargas) |>
    dplyr::mutate(dplyr::across(where(is.numeric), ~ arredonda(.x, digits)))
  ss <- colSums(L^2)
  variancia <- tibble::tibble(
    fator      = paste0("Fator", seq_along(ss)),
    ss_cargas  = arredonda(ss, digits),
    prop_var   = arredonda(ss / ncol(X), digits)
  )
  list(cargas = cargas, variancia = variancia, modelo = fit)
}

#' Analise de correspondencia (CA)
#'
#' Decompoe uma tabela de contingencia para mapear linhas e colunas em um
#' espaco de baixa dimensao (via SVD dos residuos padronizados).
#'
#' @param tabela Matriz/tabela de contingencia (contagens).
#' @param n_dim Inteiro. Numero de dimensoes a reter.
#' @param digits Inteiro.
#'
#' @return Uma lista com `inercia` (tibble), `coord_linhas` (tibble) e
#'   `coord_colunas` (tibble).
#'
#' @examples
#' rnp_correspondencia(table(mtcars$cyl, mtcars$gear))
#' @family multivariada
#' @export
rnp_correspondencia <- function(tabela, n_dim = 2L, digits = 4L) {
  N <- as.matrix(tabela)
  if (any(N < 0)) rlang::abort("A tabela deve conter contagens nao-negativas.")
  total <- sum(N)
  P <- N / total
  r <- rowSums(P); c <- colSums(P)
  Dr_inv <- diag(1 / sqrt(r)); Dc_inv <- diag(1 / sqrt(c))
  S <- Dr_inv %*% (P - outer(r, c)) %*% Dc_inv
  sv <- svd(S)
  k <- min(n_dim, length(sv$d))
  inercia_total <- sum(sv$d^2)
  coord_l <- Dr_inv %*% sv$u[, seq_len(k), drop = FALSE] %*% diag(sv$d[seq_len(k)], k)
  coord_c <- Dc_inv %*% sv$v[, seq_len(k), drop = FALSE] %*% diag(sv$d[seq_len(k)], k)
  nm <- paste0("Dim", seq_len(k))
  list(
    inercia = tibble::tibble(
      dimensao = seq_len(k),
      inercia  = arredonda(sv$d[seq_len(k)]^2, digits),
      prop     = arredonda(sv$d[seq_len(k)]^2 / inercia_total, digits)),
    coord_linhas = dplyr::bind_cols(
      categoria = rownames(N),
      tibble::as_tibble(as.data.frame.matrix(coord_l)) |> stats::setNames(nm)) |>
      dplyr::mutate(dplyr::across(where(is.numeric), ~ arredonda(.x, digits))),
    coord_colunas = dplyr::bind_cols(
      categoria = colnames(N),
      tibble::as_tibble(as.data.frame.matrix(coord_c)) |> stats::setNames(nm)) |>
      dplyr::mutate(dplyr::across(where(is.numeric), ~ arredonda(.x, digits)))
  )
}

#' Biplot de PCA
#'
#' Constroi um biplot (scores das observacoes + vetores das variaveis) a partir
#' da saida de [rnp_pca()].
#'
#' @param rnp_pca_obj Saida de [rnp_pca()].
#' @param escala Escala dos vetores de carga (fator multiplicativo).
#'
#' @return Objeto `ggplot`.
#'
#' @examples
#' p <- rnp_pca(mtcars[, c("mpg", "disp", "hp", "drat", "wt")])
#' rnp_biplot(p)
#' @family multivariada
#' @export
rnp_biplot <- function(rnp_pca_obj, escala = 1) {
  if (is.null(rnp_pca_obj$scores) || is.null(rnp_pca_obj$loadings)) {
    rlang::abort("Forneca a saida de rnp_pca().")
  }
  sc <- rnp_pca_obj$scores
  ld <- rnp_pca_obj$loadings
  fator <- max(abs(sc$PC1), abs(sc$PC2)) / max(abs(ld$PC1), abs(ld$PC2)) * escala
  ggplot2::ggplot() +
    ggplot2::geom_point(data = sc, ggplot2::aes(.data$PC1, .data$PC2),
                        color = rnp_paleta_rnp("rnp_qual", 1), alpha = 0.7) +
    ggplot2::geom_segment(data = ld,
                          ggplot2::aes(x = 0, y = 0,
                                       xend = .data$PC1 * fator,
                                       yend = .data$PC2 * fator),
                          arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm")),
                          color = "#C73E1D") +
    ggplot2::geom_text(data = ld,
                       ggplot2::aes(.data$PC1 * fator, .data$PC2 * fator,
                                    label = .data$variavel),
                       color = "#C73E1D", size = 3, vjust = -0.3) +
    rnp_tema_rnp() +
    ggplot2::labs(title = "Biplot (PCA)", x = "PC1", y = "PC2")
}

#' Matriz de dispersao
#'
#' Gera uma matriz de graficos de dispersao (pares de variaveis numericas).
#'
#' @param base data.frame ou matriz numerica.
#' @param cor_grupo Nome opcional (string) de coluna categorica para colorir.
#'
#' @return Objeto `ggplot` (facetado).
#'
#' @examples
#' rnp_grafico_dispersao_matriz(iris[, 1:4])
#' @family multivariada
#' @export
rnp_grafico_dispersao_matriz <- function(base, cor_grupo = NULL) {
  base <- as.data.frame(base)
  num <- names(base)[vapply(base, is.numeric, logical(1))]
  if (length(num) < 2L) rlang::abort("Necessario >= 2 variaveis numericas.")
  combos <- utils::combn(num, 2L)
  grupo <- if (!is.null(cor_grupo)) base[[cor_grupo]] else NULL
  d <- purrr::map_dfr(seq_len(ncol(combos)), function(i) {
    vx <- combos[1L, i]; vy <- combos[2L, i]
    tibble::tibble(
      par = paste(vx, "x", vy),
      x = base[[vx]], y = base[[vy]],
      grupo = if (is.null(grupo)) "todos" else as.character(grupo)
    )
  })
  g <- ggplot2::ggplot(d, ggplot2::aes(.data$x, .data$y))
  if (is.null(cor_grupo)) {
    g <- g + ggplot2::geom_point(color = rnp_paleta_rnp("rnp_qual", 1), alpha = 0.6)
  } else {
    g <- g + ggplot2::geom_point(ggplot2::aes(color = .data$grupo), alpha = 0.6)
  }
  g + ggplot2::facet_wrap(~ par, scales = "free") +
    rnp_tema_rnp() +
    ggplot2::labs(title = "Matriz de dispersao", x = NULL, y = NULL)
}
