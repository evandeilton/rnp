#' Analise de componentes principais (PCA)
#'
#' Wrapper de [stats::prcomp()] com saida tidy e variância explicada.
#'
#' @param base data.frame ou matriz numerica.
#' @param scale Logico. Escalonar variaveis (default TRUE).
#' @param n_comp Inteiro. Numero de componentes a reter. Default NULL = todos.
#' @param digits Inteiro.
#'
#' @return lista:
#'   * \code{modelo}: objeto \code{prcomp}.
#'   * \code{scores}: tibble com PC1, PC2, ...
#'   * \code{loadings}: tibble.
#'   * \code{variância}: tibble com PC, variância, variância acumulada,
#'     percentual.
#'
#' @examples
#' rnp_pca(mtcars[, c("mpg", "disp", "hp", "drat", "wt")])
#' @export
rnp_pca <- function(base, scale = TRUE, n_comp = NULL, digits = 4L) {
  if (!is.data.frame(base) && !is.matrix(base)) {
    rlang::abort("{.arg base} deve ser data.frame ou matriz numerica.")
  }
  base <- as.data.frame(base)
  num_cols <- vapply(base, is.numeric, logical(1))
  if (sum(num_cols) < 2L) rlang::abort("Necessario >= 2 colunas numericas.")
  base_num <- base[, num_cols, drop = FALSE]
  base_num <- base_num[stats::complete.cases(base_num), , drop = FALSE]
  if (nrow(base_num) < 3L) rlang::abort("Necessario >= 3 observacoes completas.")
  fit <- stats::prcomp(base_num, center = TRUE, scale. = scale)
  var_total <- sum(fit$sdev^2)
  var_exp <- fit$sdev^2 / var_total
  var_acum <- cumsum(var_exp)
  n_ret <- if (is.null(n_comp)) length(fit$sdev) else min(n_comp, length(fit$sdev))
  scores <- tibble::as_tibble(fit$x[, seq_len(n_ret), drop = FALSE])
  loadings <- tibble::as_tibble(fit$rotation[, seq_len(n_ret), drop = FALSE]) |>
    tibble::rownames_to_column("variavel")
  var_tbl <- tibble::tibble(
    componente = paste0("PC", seq_len(length(fit$sdev))),
    variancia  = fit$sdev^2,
    percentual = var_exp,
    acumulada  = var_acum
  )
  .rnp_lista(list(
    modelo     = fit,
    scores     = scores |> dplyr::mutate(dplyr::across(where(is.numeric),
                                                       ~ arredonda(.x, digits))),
    loadings   = loadings |> dplyr::mutate(dplyr::across(where(is.numeric),
                                                         ~ arredonda(.x, digits))),
    variancia  = var_tbl |> dplyr::mutate(dplyr::across(where(is.numeric),
                                                        ~ arredonda(.x, digits)))
  ), "Analise de componentes principais (PCA)")
}

#' Cluster K-Means
#'
#' Wrapper de [stats::kmeans()] com saida tidy e metricas de avaliação.
#'
#' @param base data.frame ou matriz numerica.
#' @param k Inteiro. Numero de clusters.
#' @param nstart Inteiro. Numero de inicializacoes aleatorias.
#' @param scale Logico. Escalonar variaveis.
#' @param seed Inteiro. Semente aleatoria.
#' @param digits Inteiro.
#'
#' @return lista:
#'   * \code{modelo}: objeto \code{kmeans}.
#'   * \code{clusters}: tibble com cluster atribuido a cada observacao.
#'   * \code{centros}: tibble com centroides.
#'   * \code{metricas}: tibble com wss_total, between_ss, ratio_ss.
#'
#' @examples
#' rnp_kmeans(mtcars[, c("mpg", "hp", "wt")], k = 3)
#' @export
rnp_kmeans <- function(base, k, nstart = 25L, scale = TRUE,
                       seed = 42L, digits = 4L) {
  abort_inteiro_pos(k, "k")
  if (!is.data.frame(base) && !is.matrix(base)) {
    rlang::abort("{.arg base} deve ser data.frame ou matriz.")
  }
  base <- as.data.frame(base)
  num_cols <- vapply(base, is.numeric, logical(1))
  if (sum(num_cols) < 1L) rlang::abort("Necessario >= 1 coluna numerica.")
  base_num <- base[, num_cols, drop = FALSE]
  base_num <- base_num[stats::complete.cases(base_num), , drop = FALSE]
  if (nrow(base_num) < k) rlang::abort("n < k. Reduza k.")
  if (scale) base_num <- as.data.frame(scale(base_num))
  set.seed(seed)
  fit <- stats::kmeans(base_num, centers = k, nstart = nstart)
  wss_total <- sum(fit$withinss)
  between_ss <- fit$betweenss
  ratio_ss <- between_ss / (between_ss + wss_total)
  clusters <- tibble::tibble(
    observacao = seq_len(nrow(base_num)),
    cluster    = fit$cluster
  )
  centros <- tibble::as_tibble(fit$centers) |>
    tibble::rownames_to_column("cluster") |>
    dplyr::mutate(cluster = as.integer(cluster))
  metricas <- tibble::tibble(
    wss_total   = wss_total,
    between_ss  = between_ss,
    ratio_ss    = ratio_ss,
    k           = k,
    nobs        = nrow(base_num)
  )
  .rnp_lista(list(
    modelo   = fit,
    clusters = clusters,
    centros  = centros |> dplyr::mutate(dplyr::across(where(is.numeric),
                                                      ~ arredonda(.x, digits))),
    metricas = metricas |> dplyr::mutate(dplyr::across(where(is.numeric) & !k & !nobs,
                                                       ~ arredonda(.x, digits)))
  ), "Cluster k-medias")
}

#' Distancias entre observacoes
#'
#' Calcula matriz de distancias usando varios metodos.
#'
#' @param base data.frame ou matriz numerica.
#' @param method String: \code{"euclidean"}, \code{"manhattan"},
#'   \code{"minkowski"}, \code{"canberra"}, \code{"mahalanobis"}.
#' @param p Escalar. Potencia (apenas Minkowski).
#' @param digits Inteiro.
#'
#' @return Matriz de distancias (classe \code{dist}).
#'
#' @examples
#' rnp_distancia(mtcars[1:5, c("mpg", "hp")], method = "euclidean")
#' @export
rnp_distancia <- function(base, method = c("euclidean", "manhattan",
                                           "minkowski", "canberra",
                                           "mahalanobis"),
                          p = 2, digits = 4L) {
  method <- rlang::arg_match(method)
  if (!is.data.frame(base) && !is.matrix(base)) {
    rlang::abort("{.arg base} deve ser data.frame ou matriz numerica.")
  }
  base <- as.data.frame(base)
  num_cols <- vapply(base, is.numeric, logical(1))
  if (sum(num_cols) < 1L) rlang::abort("Necessario >= 1 coluna numerica.")
  base_num <- base[, num_cols, drop = FALSE]
  base_num <- base_num[stats::complete.cases(base_num), , drop = FALSE]
  X <- as.matrix(base_num)
  metodo_id <- switch(method,
    euclidean = 1L, manhattan = 2L, minkowski = 3L,
    canberra = 4L, mahalanobis = 5L)
  inv_cov <- matrix(0, 0, 0)
  if (method == "mahalanobis") {
    cov_mat <- stats::cov(X)
    if (det(cov_mat) == 0) {
      rlang::abort("Matriz de covariancia singular. Distancia de Mahalanobis indefinida.")
    }
    inv_cov <- solve(cov_mat)
  }
  mat <- dist_pairwise_cpp(X, metodo_id, p, inv_cov)
  d <- stats::as.dist(mat)
  attr(d, "method") <- method
  d
}

#' Escalonamento multidimensional (MDS)
#'
#' Wrapper de [stats::cmdscale()].
#'
#' @param d Objeto \code{dist} ou matriz de distancias.
#' @param k Inteiro. Numero de dimensoes.
#' @param digits Inteiro.
#'
#' @return lista:
#'   * \code{pontos}: tibble com coordenadas.
#'   * \code{eigenvalues}: vetor.
#'   * \code{GOF}: vetor com goodness-of-fit.
#'
#' @examples
#' d <- dist(mtcars[1:10, c("mpg", "hp", "wt")])
#' rnp_mds(d, k = 2)
#' @export
rnp_mds <- function(d, k = 2, digits = 4L) {
  if (!inherits(d, "dist")) {
    d <- stats::as.dist(d)
  }
  abort_inteiro_pos(k, "k")
  fit <- stats::cmdscale(d, k = k, eig = TRUE, x.ret = TRUE)
  pts <- fit$points
  colnames(pts) <- paste0("Dim", seq_len(ncol(pts)))
  pontos <- tibble::as_tibble(pts)
  .rnp_lista(list(
    pontos        = pontos |> dplyr::mutate(dplyr::across(where(is.numeric),
                                                          ~ arredonda(.x, digits))),
    eigenvalues   = fit$eig,
    GOF           = fit$GOF
  ), "Escalonamento multidimensional (MDS)")
}
