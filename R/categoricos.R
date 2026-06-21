# Dados categoricos e concordancia (FATIA 5): Fisher, odds ratio, risco
# relativo e Kappa de Cohen.

#' Teste exato de Fisher
#'
#' Wrapper de [stats::fisher.test()] para tabelas de contingencia.
#'
#' @param tabela Tabela/matriz de contingencia (2x2 ou r x c).
#' @param digits Inteiro.
#'
#' @return tibble com `p_valor` e, no caso 2x2, `odds_ratio`, `ic_inf`,
#'   `ic_sup`.
#'
#' @examples
#' rnp_teste_fisher(matrix(c(8, 2, 1, 5), 2, 2))
#' @family categoricos
#' @export
rnp_teste_fisher <- function(tabela, digits = 4L) {
  tb <- as.matrix(tabela)
  ft <- stats::fisher.test(tb)
  if (all(dim(tb) == c(2L, 2L))) {
    tibble::tibble(
      p_valor    = arredonda(ft$p.value, digits),
      odds_ratio = arredonda(unname(ft$estimate), digits),
      ic_inf     = arredonda(ft$conf.int[1L], digits),
      ic_sup     = arredonda(ft$conf.int[2L], digits)
    )
  } else {
    tibble::tibble(p_valor = arredonda(ft$p.value, digits))
  }
}

#' Razao de chances (odds ratio) com IC
#'
#' Calcula a razao de chances de uma tabela 2x2 e o IC pelo metodo de Woolf
#' (log). A tabela deve ter exposicao nas linhas e desfecho nas colunas, com a
#' primeira coluna sendo o evento.
#'
#' @param tabela Matriz 2x2 de contagens.
#' @param conf Nivel de confianca.
#' @param digits Inteiro.
#'
#' @return tibble com `odds_ratio`, `ic_inf`, `ic_sup`, `log_or`, `ep_log`.
#'
#' @examples
#' rnp_odds_ratio(matrix(c(20, 10, 15, 25), 2, 2, byrow = TRUE))
#' @family categoricos
#' @export
rnp_odds_ratio <- function(tabela, conf = 0.95, digits = 4L) {
  tb <- as.matrix(tabela)
  if (!all(dim(tb) == c(2L, 2L))) rlang::abort("Tabela deve ser 2x2.")
  abort_confianca(conf)
  a <- tb[1L, 1L]; b <- tb[1L, 2L]; c <- tb[2L, 1L]; d <- tb[2L, 2L]
  or <- (a * d) / (b * c)
  log_or <- log(or)
  ep <- sqrt(1 / a + 1 / b + 1 / c + 1 / d)
  z <- stats::qnorm((1 + conf) / 2)
  tibble::tibble(
    odds_ratio = arredonda(or, digits),
    ic_inf     = arredonda(exp(log_or - z * ep), digits),
    ic_sup     = arredonda(exp(log_or + z * ep), digits),
    log_or     = arredonda(log_or, digits),
    ep_log     = arredonda(ep, digits)
  )
}

#' Risco relativo com IC
#'
#' Calcula o risco relativo de uma tabela 2x2 (exposicao nas linhas; primeira
#' coluna = evento) e o IC pela aproximacao log.
#'
#' @param tabela Matriz 2x2 de contagens.
#' @param conf Nivel de confianca.
#' @param digits Inteiro.
#'
#' @return tibble com `risco_relativo`, `ic_inf`, `ic_sup`, `risco_expostos`,
#'   `risco_nao_expostos`.
#'
#' @examples
#' rnp_risco_relativo(matrix(c(20, 80, 10, 90), 2, 2, byrow = TRUE))
#' @family categoricos
#' @export
rnp_risco_relativo <- function(tabela, conf = 0.95, digits = 4L) {
  tb <- as.matrix(tabela)
  if (!all(dim(tb) == c(2L, 2L))) rlang::abort("Tabela deve ser 2x2.")
  abort_confianca(conf)
  a <- tb[1L, 1L]; b <- tb[1L, 2L]; c <- tb[2L, 1L]; d <- tb[2L, 2L]
  r1 <- a / (a + b); r0 <- c / (c + d)
  rr <- r1 / r0
  ep <- sqrt(b / (a * (a + b)) + d / (c * (c + d)))
  z <- stats::qnorm((1 + conf) / 2)
  tibble::tibble(
    risco_relativo     = arredonda(rr, digits),
    ic_inf             = arredonda(exp(log(rr) - z * ep), digits),
    ic_sup             = arredonda(exp(log(rr) + z * ep), digits),
    risco_expostos     = arredonda(r1, digits),
    risco_nao_expostos = arredonda(r0, digits)
  )
}

#' Kappa de Cohen (concordancia entre dois avaliadores)
#'
#' Mede a concordancia entre dois avaliadores categoricos, ajustada pelo acaso.
#'
#' @param avaliador1,avaliador2 Vetores de classificacoes (mesmos niveis).
#' @param ponderado Logico. Kappa ponderado (quadratico) para escalas ordinais.
#' @param digits Inteiro.
#'
#' @return tibble com `kappa`, `concordancia_observada`, `concordancia_esperada`.
#'
#' @examples
#' a1 <- c("a", "b", "a", "c", "b", "a")
#' a2 <- c("a", "b", "a", "c", "c", "a")
#' rnp_kappa(a1, a2)
#' @family categoricos
#' @export
rnp_kappa <- function(avaliador1, avaliador2, ponderado = FALSE, digits = 4L) {
  if (length(avaliador1) != length(avaliador2)) {
    rlang::abort("Os dois avaliadores devem ter o mesmo comprimento.")
  }
  niveis <- sort(union(unique(avaliador1), unique(avaliador2)))
  f1 <- factor(avaliador1, levels = niveis)
  f2 <- factor(avaliador2, levels = niveis)
  M <- table(f1, f2)
  n <- sum(M)
  P <- M / n
  pr <- rowSums(P); pc <- colSums(P)
  k <- length(niveis)
  if (ponderado) {
    w <- outer(seq_len(k), seq_len(k), function(i, j) 1 - ((i - j)^2) / ((k - 1)^2))
    po <- sum(w * P)
    pe <- sum(w * outer(pr, pc))
  } else {
    po <- sum(diag(P))
    pe <- sum(pr * pc)
  }
  kappa <- (po - pe) / (1 - pe)
  tibble::tibble(
    kappa                 = arredonda(kappa, digits),
    concordancia_observada = arredonda(po, digits),
    concordancia_esperada = arredonda(pe, digits)
  )
}
