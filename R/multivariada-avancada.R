# Multivariada avancada (FATIA 4): matriz de correlacao com p-valores,
# correlograma, cluster hierarquico, dendrograma, silhueta, LDA, biplot,
# matriz de dispersao, analise fatorial, correspondencia, Hotelling, MANOVA,
# k-medoids, normalidade multivariada e correlacao canonica.

#' Matriz de correlacao com p-valores
#'
#' Calcula a matriz de correlacao (Pearson ou Spearman, via backend C++) e a
#' matriz de p-valores dos testes de correlacao, em formato longo (tidy).
#'
#' @param base data.frame ou matriz numerica.
#' @param metodo String: `"pearson"` ou `"spearman"`.
#' @param digits Inteiro.
#'
#' @return Uma lista com `matriz` (matriz de correlacao), `p_valores` (matriz) e
#'   `tidy` (tibble: `var1`, `var2`, `correlacao`, `p_valor`).
#'
#' @examples
#' rnp_matriz_correlacao(mtcars[, c("mpg", "hp", "wt", "disp")])
#' @family multivariada
#' @export
rnp_matriz_correlacao <- function(base, metodo = c("pearson", "spearman"),
                                  digits = 4L) {
  metodo <- rlang::arg_match(metodo)
  if (!is.data.frame(base) && !is.matrix(base)) {
    rlang::abort("{.arg base} deve ser data.frame ou matriz numerica.")
  }
  base <- as.data.frame(base)
  num <- vapply(base, is.numeric, logical(1))
  if (sum(num) < 2L) rlang::abort("Necessario >= 2 colunas numericas.")
  X <- as.matrix(base[stats::complete.cases(base[, num]), num, drop = FALSE])
  nomes <- colnames(X)
  R <- cor_cpp(X, if (metodo == "pearson") 1L else 2L)
  dimnames(R) <- list(nomes, nomes)
  p <- ncol(X); n <- nrow(X)
  P <- matrix(NA_real_, p, p, dimnames = list(nomes, nomes))
  for (i in seq_len(p)) {
    for (j in seq_len(p)) {
      if (i == j) { P[i, j] <- 0; next }
      P[i, j] <- suppressWarnings(
        stats::cor.test(X[, i], X[, j], method = metodo)$p.value)
    }
  }
  tidy <- tibble::tibble(
    var1 = rep(nomes, times = p),
    var2 = rep(nomes, each = p),
    correlacao = arredonda(as.numeric(R), digits),
    p_valor    = arredonda(as.numeric(P), digits)
  )
  .rnp_lista(list(matriz = round(R, digits), p_valores = round(P, digits),
                  tidy = tidy), "Matriz de correlacao")
}

#' Correlograma (mapa de calor de correlacoes)
#'
#' @param base data.frame ou matriz numerica.
#' @param metodo String: `"pearson"` ou `"spearman"`.
#' @param digits Inteiro.
#'
#' @return Objeto `ggplot`.
#'
#' @examples
#' rnp_grafico_correlograma(mtcars[, c("mpg", "hp", "wt", "disp", "drat")])
#' @family multivariada
#' @export
rnp_grafico_correlograma <- function(base, metodo = c("pearson", "spearman"),
                                     digits = 2L) {
  metodo <- rlang::arg_match(metodo)
  mc <- rnp_matriz_correlacao(base, metodo, digits = digits)
  d <- mc$tidy
  d$var1 <- factor(d$var1, levels = colnames(mc$matriz))
  d$var2 <- factor(d$var2, levels = rev(colnames(mc$matriz)))
  ggplot2::ggplot(d, ggplot2::aes(.data$var1, .data$var2, fill = .data$correlacao)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(ggplot2::aes(label = .data$correlacao), size = 3) +
    ggplot2::scale_fill_gradient2(low = "#C73E1D", mid = "white", high = "#2E86AB",
                                  midpoint = 0, limits = c(-1, 1)) +
    rnp_tema_rnp() +
    ggplot2::labs(title = "Correlograma", x = NULL, y = NULL, fill = "r") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

#' Cluster hierarquico
#'
#' Agrupamento hierarquico aglomerativo via [stats::hclust()], com corte em `k`
#' grupos.
#'
#' @param base data.frame ou matriz numerica.
#' @param k Inteiro. Numero de grupos para o corte.
#' @param metodo_dist String aceito por [rnp_distancia()].
#' @param metodo_lig String de ligacao: `"complete"`, `"average"`, `"single"`,
#'   `"ward.D2"`.
#' @param escalar Logico. Padroniza as variaveis.
#' @param digits Inteiro.
#'
#' @return Uma lista com `modelo` (hclust), `grupos` (tibble: `observacao`,
#'   `grupo`) e `altura_cortes` (vetor das ultimas alturas de fusao).
#'
#' @examples
#' rnp_cluster_hierarquico(mtcars[, c("mpg", "hp", "wt")], k = 3)
#' @family multivariada
#' @export
rnp_cluster_hierarquico <- function(base, k = 2L, metodo_dist = "euclidean",
                                    metodo_lig = "complete", escalar = TRUE,
                                    digits = 4L) {
  abort_inteiro_pos(k, "k")
  base <- as.data.frame(base)
  num <- vapply(base, is.numeric, logical(1))
  X <- base[stats::complete.cases(base[, num]), num, drop = FALSE]
  if (escalar) X <- as.data.frame(scale(X))
  d <- rnp_distancia(X, method = metodo_dist)
  fit <- stats::hclust(d, method = metodo_lig)
  grupos <- stats::cutree(fit, k = k)
  n <- length(fit$height)
  .rnp_lista(list(
    modelo = fit,
    grupos = tibble::tibble(observacao = seq_along(grupos), grupo = unname(grupos)),
    altura_cortes = round(rev(fit$height)[seq_len(min(k, n))], digits)
  ), "Cluster hierarquico")
}

#' Dendrograma
#'
#' @param cluster_hier Saida de [rnp_cluster_hierarquico()] ou objeto `hclust`.
#' @param k Inteiro opcional. Desenha `k` retangulos de corte (informativo).
#'
#' @return Objeto `ggplot`.
#'
#' @examples
#' ch <- rnp_cluster_hierarquico(mtcars[, c("mpg", "hp", "wt")], k = 3)
#' rnp_grafico_dendrograma(ch)
#' @family multivariada
#' @export
rnp_grafico_dendrograma <- function(cluster_hier, k = NULL) {
  fit <- if (inherits(cluster_hier, "hclust")) cluster_hier else cluster_hier$modelo
  if (!inherits(fit, "hclust")) rlang::abort("Entrada deve ser hclust ou saida de rnp_cluster_hierarquico().")
  dd <- stats::as.dendrogram(fit)
  # extrai segmentos do dendrograma manualmente (sem ggdendro)
  segs <- .dendro_segmentos(dd)
  rotulos <- .dendro_rotulos(dd)
  ggplot2::ggplot() +
    ggplot2::geom_segment(data = segs,
                          ggplot2::aes(x = .data$x, y = .data$y,
                                       xend = .data$xend, yend = .data$yend),
                          color = rnp_paleta_rnp("rnp_qual", 1)) +
    ggplot2::geom_text(data = rotulos,
                       ggplot2::aes(x = .data$x, y = -0.02 * max(segs$y),
                                    label = .data$label),
                       angle = 90, hjust = 1, size = 2.5) +
    rnp_tema_rnp() +
    ggplot2::labs(title = "Dendrograma", x = NULL, y = "Altura") +
    ggplot2::theme(axis.text.x = ggplot2::element_blank())
}

# Extrai segmentos (x,y,xend,yend) de um dendrograma recursivamente.
.dendro_segmentos <- function(dd) {
  segs <- list()
  pos <- new.env()
  pos$contador <- 0
  rec <- function(node) {
    if (stats::is.leaf(node)) {
      pos$contador <- pos$contador + 1
      return(list(x = pos$contador, altura = 0))
    }
    filhos <- lapply(node, rec)
    xs <- vapply(filhos, function(f) f$x, numeric(1))
    h <- attr(node, "height")
    for (i in seq_along(filhos)) {
      segs[[length(segs) + 1L]] <<- data.frame(
        x = xs[i], y = filhos[[i]]$altura, xend = xs[i], yend = h)
      segs[[length(segs) + 1L]] <<- data.frame(
        x = xs[i], y = h, xend = mean(xs), yend = h)
    }
    list(x = mean(xs), altura = h)
  }
  rec(dd)
  do.call(rbind, segs)
}

# Rotulos das folhas com sua posicao no eixo x.
.dendro_rotulos <- function(dd) {
  labs <- stats::order.dendrogram(dd)
  nomes <- labels(dd)
  data.frame(x = seq_along(labs), label = nomes, stringsAsFactors = FALSE)
}

#' Analise de silhueta
#'
#' Avalia a qualidade de um agrupamento pelo indice de silhueta (backend C++).
#'
#' @param base data.frame/matriz numerica OU objeto `dist`.
#' @param clusters Vetor de rotulos de cluster.
#' @param metodo_dist String aceito por [rnp_distancia()] (se `base` nao for dist).
#' @param digits Inteiro.
#'
#' @return Uma lista com `silhuetas` (tibble: `observacao`, `cluster`,
#'   `silhueta`) e `media` (silhueta media global).
#'
#' @examples
#' km <- rnp_kmeans(mtcars[, c("mpg", "hp", "wt")], k = 3)
#' rnp_silhueta(mtcars[, c("mpg", "hp", "wt")], km$clusters$cluster)$media
#' @family multivariada
#' @export
rnp_silhueta <- function(base, clusters, metodo_dist = "euclidean", digits = 4L) {
  D <- if (inherits(base, "dist")) as.matrix(base) else {
    as.matrix(rnp_distancia(as.data.frame(base), method = metodo_dist))
  }
  if (length(clusters) != nrow(D)) {
    rlang::abort("{.arg clusters} deve ter comprimento = numero de observacoes.")
  }
  s <- as.numeric(silhueta_cpp(D, as.integer(clusters)))
  .rnp_lista(list(
    silhuetas = tibble::tibble(
      observacao = seq_along(s), cluster = clusters,
      silhueta = arredonda(s, digits)),
    media = arredonda(mean(s), digits)
  ), "Analise de silhueta")
}

#' k-medoids (PAM)
#'
#' Particionamento em torno de medoides (mais robusto que k-means a outliers).
#' Implementacao propria (build + swap simplificado) usando distancias.
#'
#' @param base data.frame ou matriz numerica.
#' @param k Inteiro. Numero de clusters.
#' @param metodo_dist String aceito por [rnp_distancia()].
#' @param escalar Logico.
#' @param max_iter Inteiro.
#' @param digits Inteiro.
#'
#' @return Uma lista com `clusters` (tibble), `medoides` (indices) e
#'   `custo` (soma das distancias aos medoides).
#'
#' @examples
#' rnp_kmedoids(mtcars[, c("mpg", "hp", "wt")], k = 3)$medoides
#' @family multivariada
#' @export
rnp_kmedoids <- function(base, k = 2L, metodo_dist = "euclidean",
                         escalar = TRUE, max_iter = 100L, digits = 4L) {
  abort_inteiro_pos(k, "k")
  base <- as.data.frame(base)
  num <- vapply(base, is.numeric, logical(1))
  X <- base[stats::complete.cases(base[, num]), num, drop = FALSE]
  if (escalar) X <- as.data.frame(scale(X))
  D <- as.matrix(rnp_distancia(X, method = metodo_dist))
  n <- nrow(D)
  if (k >= n) rlang::abort("k deve ser menor que o numero de observacoes.")
  medoides <- sort(sample.int(n, k))
  atribui <- function(med) apply(D[, med, drop = FALSE], 1L, which.min)
  custo_total <- function(med) {
    cl <- atribui(med)
    sum(vapply(seq_len(n), function(i) D[i, med[cl[i]]], numeric(1)))
  }
  custo <- custo_total(medoides)
  for (it in seq_len(max_iter)) {
    melhorou <- FALSE
    for (m in seq_len(k)) {
      for (cand in setdiff(seq_len(n), medoides)) {
        nova <- medoides; nova[m] <- cand
        c_novo <- custo_total(nova)
        if (c_novo < custo) {
          medoides <- sort(nova); custo <- c_novo; melhorou <- TRUE
        }
      }
    }
    if (!melhorou) break
  }
  cl <- atribui(medoides)
  .rnp_lista(list(
    clusters = tibble::tibble(observacao = seq_len(n), cluster = cl),
    medoides = medoides,
    custo = arredonda(custo, digits)
  ), "Cluster k-medoids (PAM)")
}

#' Analise discriminante linear (LDA)
#'
#' LDA de Fisher implementada via algebra (backend C++ para covariancias),
#' devolvendo as funcoes discriminantes e a classificacao.
#'
#' @param formula Formula `grupo ~ x1 + x2 + ...`.
#' @param data data.frame.
#' @param digits Inteiro.
#'
#' @return Uma lista com `discriminantes` (tibble de coeficientes), `predito`
#'   (vetor de classes preditas) e `acuracia` (taxa de acerto no treino).
#'
#' @examples
#' rnp_lda(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
#'         iris)$acuracia
#' @family multivariada
#' @export
rnp_lda <- function(formula, data, digits = 4L) {
  mf <- stats::model.frame(formula, data, na.action = stats::na.omit)
  y <- stats::model.response(mf)
  if (!is.factor(y)) y <- as.factor(y)
  X <- stats::model.matrix(formula, mf)
  inter <- which(colnames(X) == "(Intercept)")
  if (length(inter)) X <- X[, -inter, drop = FALSE]
  niveis <- levels(y)
  p <- ncol(X)
  medias_g <- t(vapply(niveis, function(g) colMeans(X[y == g, , drop = FALSE]), numeric(p)))
  media_geral <- colMeans(X)
  # within-class scatter (Sw) via cov_cpp por grupo
  Sw <- matrix(0, p, p)
  for (g in niveis) {
    Xg <- X[y == g, , drop = FALSE]
    Sw <- Sw + (nrow(Xg) - 1) * cov_cpp(Xg)
  }
  Sw_inv <- solve(Sw)
  coefs <- Sw_inv %*% t(medias_g)  # p x K
  escores <- X %*% coefs
  # ajuste por priori/constante: usa media de score por grupo
  priori <- as.numeric(table(y)) / length(y)
  const <- vapply(seq_along(niveis), function(k)
    -0.5 * as.numeric(medias_g[k, ] %*% Sw_inv %*% medias_g[k, ]) + log(priori[k]),
    numeric(1))
  escores <- sweep(escores, 2L, const, "+")
  pred <- niveis[max.col(escores)]
  tibble_disc <- tibble::as_tibble(as.data.frame(coefs)) |>
    stats::setNames(paste0("LD_", niveis))
  tibble_disc <- dplyr::bind_cols(variavel = colnames(X), tibble_disc) |>
    dplyr::mutate(dplyr::across(where(is.numeric), ~ arredonda(.x, digits)))
  .rnp_lista(list(
    discriminantes = tibble_disc,
    predito = pred,
    acuracia = arredonda(mean(pred == as.character(y)), digits)
  ), "Analise discriminante linear (LDA)")
}

#' Teste T2 de Hotelling (medias multivariadas)
#'
#' Compara o vetor de medias de duas amostras multivariadas (ou contra um vetor
#' de referencia, uma amostra).
#'
#' @param X Matriz/data.frame do grupo 1.
#' @param Y Matriz/data.frame do grupo 2 (duas amostras) ou `NULL`.
#' @param mu0 Vetor de referencia (uma amostra). Default zeros.
#' @param digits Inteiro.
#'
#' @return tibble com `t2`, `estatistica_f`, `gl1`, `gl2`, `p_valor`.
#'
#' @examples
#' rnp_hotelling(iris[1:50, 1:4], iris[51:100, 1:4])
#' @family multivariada
#' @export
rnp_hotelling <- function(X, Y = NULL, mu0 = NULL, digits = 4L) {
  X <- as.matrix(X)
  p <- ncol(X)
  if (!is.null(Y)) {
    Y <- as.matrix(Y)
    n1 <- nrow(X); n2 <- nrow(Y)
    m1 <- colMeans(X); m2 <- colMeans(Y)
    Sp <- ((n1 - 1) * cov_cpp(X) + (n2 - 1) * cov_cpp(Y)) / (n1 + n2 - 2)
    dif <- m1 - m2
    t2 <- (n1 * n2 / (n1 + n2)) * as.numeric(dif %*% solve(Sp) %*% dif)
    gl1 <- p; gl2 <- n1 + n2 - p - 1
    f <- t2 * gl2 / (p * (n1 + n2 - 2))
  } else {
    n1 <- nrow(X)
    if (is.null(mu0)) mu0 <- rep(0, p)
    m1 <- colMeans(X)
    S <- cov_cpp(X)
    dif <- m1 - mu0
    t2 <- n1 * as.numeric(dif %*% solve(S) %*% dif)
    gl1 <- p; gl2 <- n1 - p
    f <- t2 * gl2 / (p * (n1 - 1))
  }
  tibble::tibble(
    t2            = arredonda(t2, digits),
    estatistica_f = arredonda(f, digits),
    gl1           = gl1,
    gl2           = gl2,
    p_valor       = arredonda(stats::pf(f, gl1, gl2, lower.tail = FALSE), digits)
  )
}

#' MANOVA (analise de variancia multivariada)
#'
#' Wrapper de [stats::manova()] reportando as estatisticas de Wilks e Pillai.
#'
#' @param formula Formula `cbind(y1, y2, ...) ~ grupo`.
#' @param data data.frame.
#' @param digits Inteiro.
#'
#' @return tibble com `teste`, `estatistica`, `aprox_f`, `p_valor`.
#'
#' @examples
#' rnp_manova(cbind(Sepal.Length, Petal.Length) ~ Species, iris)
#' @family multivariada
#' @export
rnp_manova <- function(formula, data, digits = 4L) {
  fit <- stats::manova(formula, data = data)
  purrr::map_dfr(c("Wilks", "Pillai"), function(tst) {
    s <- summary(fit, test = tst)$stats
    tibble::tibble(
      teste       = tst,
      estatistica = arredonda(s[1L, 2L], digits),
      aprox_f     = arredonda(s[1L, 3L], digits),
      p_valor     = arredonda(s[1L, ncol(s)], digits)
    )
  })
}

#' Teste de normalidade multivariada (Mardia)
#'
#' Avalia assimetria e curtose multivariadas (teste de Mardia).
#'
#' @param X Matriz/data.frame numerica.
#' @param digits Inteiro.
#'
#' @return tibble com `medida`, `estatistica`, `p_valor`.
#'
#' @examples
#' rnp_normalidade_multivariada(iris[, 1:4])
#' @family multivariada
#' @export
rnp_normalidade_multivariada <- function(X, digits = 4L) {
  X <- as.matrix(X[stats::complete.cases(X), , drop = FALSE])
  n <- nrow(X); p <- ncol(X)
  Xc <- scale(X, scale = FALSE)
  S <- cov_cpp(X) * (n - 1) / n  # MLE da covariancia
  Sinv <- solve(S)
  D <- Xc %*% Sinv %*% t(Xc)
  b1 <- sum(D^3) / n^2
  b2 <- sum(diag(D)^2) / n
  est_assim <- n * b1 / 6
  gl <- p * (p + 1) * (p + 2) / 6
  p_assim <- stats::pchisq(est_assim, gl, lower.tail = FALSE)
  est_curt <- (b2 - p * (p + 2)) / sqrt(8 * p * (p + 2) / n)
  p_curt <- 2 * stats::pnorm(-abs(est_curt))
  tibble::tibble(
    medida      = c("assimetria", "curtose"),
    estatistica = arredonda(c(est_assim, est_curt), digits),
    p_valor     = arredonda(c(p_assim, p_curt), digits)
  )
}

#' Correlacao canonica
#'
#' Encontra as correlacoes canonicas entre dois conjuntos de variaveis.
#'
#' @param X,Y Matrizes/data.frames numericos com o mesmo numero de linhas.
#' @param digits Inteiro.
#'
#' @return tibble com `dimensao` e `correlacao_canonica`.
#'
#' @examples
#' rnp_correlacao_canonica(iris[, 1:2], iris[, 3:4])
#' @family multivariada
#' @export
rnp_correlacao_canonica <- function(X, Y, digits = 4L) {
  X <- as.matrix(X); Y <- as.matrix(Y)
  if (nrow(X) != nrow(Y)) rlang::abort("X e Y devem ter o mesmo numero de linhas.")
  Xc <- scale(X, scale = FALSE); Yc <- scale(Y, scale = FALSE)
  Sxx <- cov_cpp(X); Syy <- cov_cpp(Y)
  Sxy <- stats::cov(Xc, Yc)
  M <- solve(Sxx) %*% Sxy %*% solve(Syy) %*% t(Sxy)
  vals <- Re(eigen(M, only.values = TRUE)$values)
  vals <- vals[vals > 0]
  rho <- sqrt(pmin(pmax(vals, 0), 1))
  tibble::tibble(
    dimensao = seq_along(rho),
    correlacao_canonica = arredonda(rho, digits)
  )
}
