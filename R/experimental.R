# Delineamento experimental (FATIA 5): ANCOVA, medidas repetidas, blocos
# casualizados (DBC) e contrastes.

#' Analise de covariancia (ANCOVA)
#'
#' Ajusta um modelo com um fator e uma covariavel continua, reportando a tabela
#' ANCOVA (tipo I).
#'
#' @param formula Formula `y ~ fator + covariavel`.
#' @param data data.frame.
#' @param digits Inteiro.
#'
#' @return tibble com a tabela ANOVA do modelo.
#'
#' @examples
#' rnp_ancova(mpg ~ factor(cyl) + wt, mtcars)
#' @family experimental
#' @export
rnp_ancova <- function(formula, data, digits = 4L) {
  fit <- stats::lm(formula, data = data)
  tab <- as.data.frame(stats::anova(fit))
  tibble::tibble(
    fonte       = rownames(tab),
    gl          = tab$Df,
    sq          = arredonda(tab$`Sum Sq`, digits),
    qm          = arredonda(tab$`Mean Sq`, digits),
    estatistica_f = arredonda(tab$`F value`, digits),
    p_valor     = arredonda(tab$`Pr(>F)`, digits)
  )
}

#' ANOVA de medidas repetidas
#'
#' ANOVA com um fator intra-sujeito, ajustada via modelo com estrato de erro
#' por sujeito ([stats::aov()] com `Error()`).
#'
#' @param formula Formula `resposta ~ fator`.
#' @param data data.frame em formato longo.
#' @param sujeito String com o nome da coluna identificadora do sujeito.
#' @param digits Inteiro.
#'
#' @return tibble com a tabela ANOVA do estrato intra-sujeito.
#'
#' @examples
#' df <- data.frame(
#'   resp = c(10, 12, 14, 9, 11, 13, 8, 10, 12),
#'   cond = factor(rep(c("A", "B", "C"), 3)),
#'   suj  = factor(rep(1:3, each = 3)))
#' rnp_anova_medidas_repetidas(resp ~ cond, df, sujeito = "suj")
#' @family experimental
#' @export
rnp_anova_medidas_repetidas <- function(formula, data, sujeito, digits = 4L) {
  if (!sujeito %in% names(data)) {
    rlang::abort("Coluna '{sujeito}' nao encontrada em {.arg data}.")
  }
  termos <- attr(stats::terms(formula), "term.labels")
  resp <- as.character(formula[[2L]])
  fml <- stats::as.formula(
    paste0(resp, " ~ ", paste(termos, collapse = " + "),
           " + Error(", sujeito, "/(", paste(termos, collapse = " + "), "))"))
  fit <- stats::aov(fml, data = data)
  sm <- summary(fit)
  # pega o estrato intra-sujeito (ultimo com o fator)
  estrato <- sm[[length(sm)]][[1L]]
  tibble::tibble(
    fonte         = trimws(rownames(estrato)),
    gl            = estrato$Df,
    sq            = arredonda(estrato$`Sum Sq`, digits),
    qm            = arredonda(estrato$`Mean Sq`, digits),
    estatistica_f = arredonda(estrato$`F value`, digits),
    p_valor       = arredonda(estrato$`Pr(>F)`, digits)
  )
}

#' Delineamento em blocos casualizados (DBC)
#'
#' ANOVA de um experimento em blocos casualizados completos (um fator de
#' tratamento + um fator de bloco).
#'
#' @param resposta Vetor numerico da resposta.
#' @param tratamento Fator de tratamento.
#' @param bloco Fator de bloco.
#' @param digits Inteiro.
#'
#' @return tibble com a tabela ANOVA (tratamento, bloco, residuo).
#'
#' @examples
#' set.seed(1)
#' resp <- rnorm(12, rep(c(5, 7, 9), 4))
#' trat <- factor(rep(1:3, 4)); bloco <- factor(rep(1:4, each = 3))
#' rnp_dbc(resp, trat, bloco)
#' @family experimental
#' @export
rnp_dbc <- function(resposta, tratamento, bloco, digits = 4L) {
  abort_numerico(resposta, "resposta")
  d <- data.frame(y = resposta, trat = as.factor(tratamento),
                  bloco = as.factor(bloco))
  fit <- stats::aov(y ~ trat + bloco, data = d)
  tab <- summary(fit)[[1L]]
  tibble::tibble(
    fonte         = trimws(rownames(tab)),
    gl            = tab$Df,
    sq            = arredonda(tab$`Sum Sq`, digits),
    qm            = arredonda(tab$`Mean Sq`, digits),
    estatistica_f = arredonda(tab$`F value`, digits),
    p_valor       = arredonda(tab$`Pr(>F)`, digits)
  )
}

#' Contrastes lineares
#'
#' Testa contrastes lineares entre medias de grupos de uma ANOVA de um fator.
#'
#' @param resposta Vetor numerico.
#' @param grupo Fator de grupos.
#' @param contrastes Matriz (cada coluna = um contraste; linhas = niveis do
#'   fator, coeficientes somando zero).
#' @param digits Inteiro.
#'
#' @return tibble com `contraste`, `estimativa`, `erro_padrao`, `t`, `p_valor`.
#'
#' @examples
#' # compara grupo 1 vs media de 2 e 3; e grupo 2 vs 3
#' set.seed(1)
#' y <- rnorm(30, rep(c(5, 6, 9), each = 10)); g <- factor(rep(1:3, each = 10))
#' C <- cbind(c(2, -1, -1), c(0, 1, -1))
#' rnp_contrastes(y, g, C)
#' @family experimental
#' @export
rnp_contrastes <- function(resposta, grupo, contrastes, digits = 4L) {
  abort_numerico(resposta, "resposta")
  g <- as.factor(grupo)
  niveis <- levels(g)
  C <- as.matrix(contrastes)
  if (nrow(C) != length(niveis)) {
    rlang::abort("Numero de linhas dos contrastes deve igualar o numero de grupos.")
  }
  medias <- tapply(resposta, g, mean)
  n_i <- tapply(resposta, g, length)
  # QME da ANOVA
  fit <- stats::aov(resposta ~ g)
  qme <- summary(fit)[[1L]]$`Mean Sq`[2L]
  gl <- fit$df.residual
  res <- purrr::map_dfr(seq_len(ncol(C)), function(j) {
    cj <- C[, j]
    if (abs(sum(cj)) > 1e-8) rlang::warn("Contraste {j} nao soma zero.")
    est <- sum(cj * medias)
    se <- sqrt(qme * sum(cj^2 / n_i))
    t <- est / se
    tibble::tibble(
      contraste   = paste0("C", j),
      estimativa  = arredonda(est, digits),
      erro_padrao = arredonda(se, digits),
      t           = arredonda(t, digits),
      p_valor     = arredonda(2 * stats::pt(-abs(t), gl), digits)
    )
  })
  res
}
