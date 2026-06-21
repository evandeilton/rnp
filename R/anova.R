#' ANOVA de um fator
#'
#' Realiza ANOVA de um fator com saida tidy. Inclui teste de Levene e
#' opcionalmente testes post-hoc.
#'
#' @param x Vetor numerico (resposta) ou formula \code{y ~ grupo}.
#' @param g Vetor de grupos (necessario se x nao for formula).
#' @param post_hoc String opcional: \code{"tukey"}, \code{"duncan"},
#'   \code{"dunnett"}, \code{"scheffe"} ou \code{NA} para nenhum.
#' @param conf Nivel de confianca para post-hoc.
#' @param digits Inteiro.
#' @param data data.frame opcional (usado quando \code{x} e uma formula).
#'
#' @return lista com \code{anova} (tibble), \code{levene} (tibble) e
#'   \code{post_hoc} (tibble ou NULL).
#'
#' @examples
#' rnp_anova(mtcars$mpg, as.factor(mtcars$cyl))
#' rnp_anova(mpg ~ factor(cyl), data = mtcars, post_hoc = "tukey")
#' @export
rnp_anova <- function(x, g = NULL, post_hoc = NA_character_,
                      conf = 0.95, digits = 4L, data = NULL) {
  if (inherits(x, "formula")) {
    mt <- model.frame(x, data = data, na.action = na.omit)
    if (ncol(mt) != 2L) rlang::abort("Formula deve ser y ~ grupo.")
    y <- mt[[1L]]
    g <- as.factor(mt[[2L]])
  } else {
    abort_numerico(x, "x")
    if (is.null(g)) rlang::abort("{.arg g} (grupo) e necessario.")
    g <- as.factor(g)
    y <- x
  }
  if (length(y) != length(g)) rlang::abort("y e g devem ter o mesmo comprimento.")
  if (nlevels(g) < 2L) rlang::abort("ANOVA requer >= 2 grupos.")
  if (anyNA(y) || anyNA(g)) {
    keep <- !is.na(y) & !is.na(g)
    y <- y[keep]; g <- g[keep]
  }
  aov_fit <- stats::aov(y ~ g)
  sm <- summary(aov_fit)[[1L]]
  n_rows <- nrow(sm)
  anova_tbl <- tibble::tibble(
    fonte        = c(rownames(sm), "total"),
    gl           = c(sm$Df, sum(sm$Df)),
    soma_quadrados = c(sm$`Sum Sq`, sum(sm$`Sum Sq`)),
    media_quadrados = c(sm$`Mean Sq`, NA_real_),
    estatistica_F = c(sm$`F value`, rep(NA_real_, n_rows - 1L)),
    p_valor      = c(sm$`Pr(>F)`, rep(NA_real_, n_rows - 1L))
  ) |> dplyr::mutate(dplyr::across(where(is.numeric) & !gl,
                                   ~ arredonda(.x, digits)))
  lev <- rnp_teste_levene(y, g, digits = digits)
  ph <- if (!is.na(post_hoc)) {
    rlang::arg_match(post_hoc, c("tukey", "duncan", "dunnett", "scheffe"))
    switch(post_hoc,
      tukey   = rnp_tukey_hsd(aov_fit, conf = conf, digits = digits),
      duncan  = .duncan_interna(y, g, digits = digits),
      dunnett = .dunnett_interna(y, g, conf = conf, digits = digits),
      scheffe = rnp_scheffe(aov_fit, conf = conf, digits = digits)
    )
  } else NULL
  list(anova = anova_tbl, levene = lev, post_hoc = ph)
}

#' Teste de Tukey HSD
#'
#' @param modelo Objeto \code{aov} ou formula.
#' @param data data.frame (se modelo for formula).
#' @param conf Nivel de confianca.
#' @param digits Inteiro.
#' @return tibble com comparacoes.
#' @examples
#' fit <- aov(mpg ~ factor(cyl), mtcars)
#' rnp_tukey_hsd(fit)
#' @export
rnp_tukey_hsd <- function(modelo, data = NULL, conf = 0.95, digits = 4L) {
  if (inherits(modelo, "formula")) {
    modelo <- stats::aov(modelo, data = data)
  }
  if (!inherits(modelo, "aov")) rlang::abort("{.arg modelo} deve ser aov ou formula.")
  tk <- stats::TukeyHSD(modelo, conf.level = conf)
  out <- purrr::map_dfr(names(tk), function(termo) {
    m <- tk[[termo]]
    tibble::tibble(
      termo           = termo,
      comparacao      = rownames(m),
      diff            = m[, "diff"],
      limite_inferior = m[, "lwr"],
      limite_superior = m[, "upr"],
      p_ajustado      = m[, "p adj"]
    )
  })
  out |> dplyr::mutate(dplyr::across(where(is.numeric),
                                     ~ arredonda(.x, digits)))
}

#' Teste de Scheffe
#'
#' @inheritParams rnp_tukey_hsd
#' @return tibble.
#' @export
rnp_scheffe <- function(modelo, data = NULL, conf = 0.95, digits = 4L) {
  if (inherits(modelo, "formula")) modelo <- stats::aov(modelo, data = data)
  if (!inherits(modelo, "aov")) rlang::abort("{.arg modelo} deve ser aov.")
  sm <- summary(modelo)[[1L]]
  Fcrit <- stats::qf(conf, sm$Df[1L], sm$Df[2L])
  k <- sm$Df[1L] + 1L
  mse <- sm$`Mean Sq`[2L]
  mres <- model.frame(modelo)
  y <- mres[[1L]]
  g <- as.factor(mres[[2L]])
  means <- tapply(y, g, mean)
  n_i <- tapply(y, g, length)
  pairs <- combn(levels(g), 2, simplify = FALSE)
  out <- purrr::map_dfr(pairs, function(p) {
    i <- which(levels(g) == p[1L])
    j <- which(levels(g) == p[2L])
    diff <- means[i] - means[j]
    se <- sqrt(mse * (1 / n_i[i] + 1 / n_i[j]))
    crit <- sqrt((k - 1) * Fcrit) * se
    tibble::tibble(
      comparacao      = paste(p[1L], "-", p[2L]),
      diff            = diff,
      limite_inferior = diff - crit,
      limite_superior = diff + crit
    )
  })
  out |> dplyr::mutate(dplyr::across(where(is.numeric),
                                     ~ arredonda(.x, digits)))
}

# Internal Duncan test (LSD-protected stepwise)
.duncan_interna <- function(y, g, digits = 4L) {
  g <- as.factor(g)
  means <- tapply(y, g, mean)
  n_i <- tapply(y, g, length)
  k <- nlevels(g)
  N <- length(y)
  aov_fit <- stats::aov(y ~ g)
  sm <- summary(aov_fit)[[1L]]
  mse <- sm$`Mean Sq`[2L]
  df_e <- sm$Df[2L]
  ord <- order(means, decreasing = TRUE)
  lv <- levels(g)[ord]
  out <- purrr::map_dfr(combn(seq_along(lv), 2, simplify = FALSE), function(id) {
    i <- id[1L]; j <- id[2L]
    p <- j - i + 1L
    rp <- 1 - stats::pt(-abs(stats::qt(1 - 0.5 * (1 - 0.05^(p - 1)), df_e)), df_e) * 2
    se <- sqrt(mse / 2 * (1 / n_i[lv[i]] + 1 / n_i[lv[j]]))
    diff <- means[lv[i]] - means[lv[j]]
    tibble::tibble(
      comparacao = paste(lv[i], "-", lv[j]),
      diff       = diff,
      se         = se,
      significativo = abs(diff) > se * stats::qt(0.975, df_e)
    )
  })
  out |> dplyr::mutate(dplyr::across(where(is.numeric) & !se,
                                    ~ arredonda(.x, digits)))
}

# Internal Dunnett (against first level as control)
.dunnett_interna <- function(y, g, conf = 0.95, digits = 4L) {
  g <- as.factor(g)
  lv <- levels(g)
  if (length(lv) < 2L) rlang::abort("Dunnett requer >= 2 grupos.")
  control <- lv[1L]
  means <- tapply(y, g, mean)
  n_i <- tapply(y, g, length)
  aov_fit <- stats::aov(y ~ g)
  sm <- summary(aov_fit)[[1L]]
  mse <- sm$`Mean Sq`[2L]
  df_e <- sm$Df[2L]
  k <- length(lv)
  crit <- stats::qtukey(conf, k - 1, df_e) * sqrt(2) |>
    (\(x) if (is.na(x)) stats::qt(0.975, df_e) else x)()
  out <- purrr::map_dfr(lv[-1L], function(ng) {
    diff <- means[ng] - means[control]
    se <- sqrt(mse * (1 / n_i[ng] + 1 / n_i[control]))
    tibble::tibble(
      comparacao      = paste(ng, "-", control),
      diff            = diff,
      limite_inferior = diff - se * crit,
      limite_superior = diff + se * crit
    )
  })
  out |> dplyr::mutate(dplyr::across(where(is.numeric),
                                     ~ arredonda(.x, digits)))
}

#' ANOVA de dois fatores
#'
#' @param formula Formula \code{y ~ A * B} ou \code{y ~ A + B}.
#' @param data data.frame.
#' @param digits Inteiro.
#' @return tibble com a tabela ANOVA expandida.
#' @examples
#' rnp_anova_dois_fatores(breaks ~ wool * tension, warpbreaks)
#' @export
rnp_anova_dois_fatores <- function(formula, data = NULL, digits = 4L) {
  if (!inherits(formula, "formula")) rlang::abort("{.arg formula} deve ser formula.")
  fit <- stats::aov(formula, data = data)
  sm <- summary(fit)[[1L]]
  out <- tibble::tibble(
    fonte            = rownames(sm),
    gl               = sm$Df,
    soma_quadrados   = sm$`Sum Sq`,
    media_quadrados  = sm$`Mean Sq`,
    estatistica_F    = sm$`F value`,
    p_valor          = sm$`Pr(>F)`
  )
  out |> dplyr::mutate(dplyr::across(where(is.numeric) & !gl,
                                     ~ arredonda(.x, digits)))
}

#' Teste de Kruskal-Wallis
#'
#' @param x Vetor numerico ou formula.
#' @param g Vetor de grupos.
#' @param digits Inteiro.
#' @param data data.frame opcional (usado quando \code{x} e uma formula).
#' @return tibble.
#' @examples
#' rnp_kruskal(mtcars$mpg, as.factor(mtcars$cyl))
#' @export
rnp_kruskal <- function(x, g = NULL, digits = 4L, data = NULL) {
  if (inherits(x, "formula")) {
    mt <- model.frame(x, data = data, na.action = na.omit)
    y <- mt[[1L]]; g <- as.factor(mt[[2L]])
  } else {
    abort_numerico(x, "x")
    if (is.null(g)) rlang::abort("{.arg g} necessario.")
    g <- as.factor(g); y <- x
  }
  kt <- stats::kruskal.test(y, g)
  tibble::tibble(
    estatistica = unname(kt$statistic),
    gl          = unname(kt$parameter),
    p_valor     = kt$p.value,
    metodo      = "kruskal-wallis"
  ) |> dplyr::mutate(dplyr::across(where(is.numeric) & !gl,
                                   ~ arredonda(.x, digits)))
}

#' Teste de Mann-Whitney (Wilcoxon duas amostras independentes)
#'
#' @param x,y Vetores numericos.
#' @param lado String.
#' @param digits Inteiro.
#' @return tibble.
#' @examples
#' rnp_mann_whitney(rnorm(20, 5), rnorm(20, 6))
#' @export
rnp_mann_whitney <- function(x, y,
                             lado = c("bilateral", "direita", "esquerda"),
                             digits = 4L) {
  abort_numerico(x, "x"); abort_numerico(y, "y")
  lado <- check_lado(lado)
  alt <- switch(lado, bilateral = "two.sided", direita = "greater", esquerda = "less")
  wt <- stats::wilcox.test(x, y, alternative = alt, exact = NULL, correct = TRUE)
  tibble::tibble(
    estatistica = unname(wt$statistic),
    p_valor     = wt$p.value,
    metodo      = wt$method,
    alternativa = lado
  ) |> dplyr::mutate(dplyr::across(where(is.numeric),
                                   ~ arredonda(.x, digits)))
}

#' Teste de Wilcoxon para amostras pareadas
#'
#' Atalho para Wilcoxon signed-rank.
#'
#' @param x,y Vetores numericos pareados.
#' @param lado String.
#' @param digits Inteiro.
#' @return tibble.
#' @examples
#' rnp_wilcoxon(rnorm(20, 5), rnorm(20, 5.2))
#' @export
rnp_wilcoxon <- function(x, y,
                         lado = c("bilateral", "direita", "esquerda"),
                         digits = 4L) {
  abort_numerico(x, "x"); abort_numerico(y, "y")
  if (length(x) != length(y)) rlang::abort("Amostras pareadas requerem mesmo n.")
  lado <- check_lado(lado)
  alt <- switch(lado, bilateral = "two.sided", direita = "greater", esquerda = "less")
  wt <- stats::wilcox.test(x, y, paired = TRUE, alternative = alt)
  tibble::tibble(
    estatistica = unname(wt$statistic),
    p_valor     = wt$p.value,
    metodo      = wt$method,
    alternativa = lado
  ) |> dplyr::mutate(dplyr::across(where(is.numeric),
                                   ~ arredonda(.x, digits)))
}

#' Teste de Friedman (blocos)
#'
#' @param formula Formula \code{y ~ grupo | bloco}.
#' @param data data.frame.
#' @param digits Inteiro.
#' @return tibble.
#' @examples
#' # delineamento em blocos completo (uma observacao por trat x bloco)
#' df <- data.frame(
#'   resp  = c(1, 2, 3, 2, 3, 1, 3, 1, 2, 1, 3, 2),
#'   trat  = factor(rep(1:3, times = 4)),
#'   bloco = factor(rep(1:4, each = 3))
#' )
#' rnp_teste_friedman(resp ~ trat | bloco, df)
#' @export
rnp_teste_friedman <- function(formula, data = NULL, digits = 4L) {
  ft <- stats::friedman.test(formula, data = data)
  tibble::tibble(
    estatistica = unname(ft$statistic),
    gl          = unname(ft$parameter),
    p_valor     = ft$p.value,
    metodo      = "friedman"
  ) |> dplyr::mutate(dplyr::across(where(is.numeric) & !gl,
                                   ~ arredonda(.x, digits)))
}

#' Coeficiente de concordancia de Kendall (W)
#'
#' @param matriz Matriz com linhas = objetos, colunas = avaliadores.
#' @param digits Inteiro.
#' @return tibble.
#' @export
rnp_teste_kendall_w <- function(matriz, digits = 4L) {
  if (!is.matrix(matriz) && !is.data.frame(matriz)) {
    rlang::abort("{.arg matriz} deve ser matriz ou data.frame numerico.")
  }
  M <- as.matrix(matriz)
  if (!is.numeric(M)) rlang::abort("Matriz deve ser numerica.")
  k <- ncol(M)
  n <- nrow(M)
  if (k < 2L || n < 2L) rlang::abort("Matriz deve ter >= 2 linhas e 2 colunas.")
  R <- t(apply(M, 1L, rank))
  Rj <- colSums(R)
  Sr <- sum((Rj - mean(Rj))^2)
  W <- (12 * Sr) / (k^2 * (n^3 - n))
  chi2 <- k * (n - 1) * W
  df <- n - 1
  p <- stats::pchisq(chi2, df, lower.tail = FALSE)
  tibble::tibble(
    W           = W,
    estatistica = chi2,
    gl          = df,
    p_valor     = p,
    metodo      = "kendall-w"
  ) |> dplyr::mutate(dplyr::across(where(is.numeric) & !gl,
                                   ~ arredonda(.x, digits)))
}

#' Teste de McNemar (pareado binario)
#'
#' @param x,y Vetores binarios (0/1, TRUE/FALSE, ou factor 2 niveis).
#' @param digits Inteiro.
#' @return tibble.
#' @export
rnp_teste_mcnemar <- function(x, y, digits = 4L) {
  if (length(x) != length(y)) rlang::abort("x e y devem ter o mesmo comprimento.")
  tab <- table(factor(x, levels = c(0, 1)), factor(y, levels = c(0, 1)))
  if (any(dim(tab) < 2L)) rlang::abort("x e y devem ser binarios (2 niveis).")
  mt <- stats::mcnemar.test(tab, correct = TRUE)
  tibble::tibble(
    estatistica = unname(mt$statistic),
    gl          = unname(mt$parameter),
    p_valor     = mt$p.value,
    metodo      = "mcnemar"
  ) |> dplyr::mutate(dplyr::across(where(is.numeric) & !gl,
                                   ~ arredonda(.x, digits)))
}
