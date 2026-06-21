# Testes de inferencia (FATIA 2): aderencia/independencia, KS, proporcoes,
# binomial, poder, tamanho de amostra, normalidade, Grubbs, runs e sinais.

#' Teste qui-quadrado (independencia ou aderencia)
#'
#' Com `x` e `y`, testa independencia entre duas categoricas. Com `x`
#' (contagens observadas) e `p` (probabilidades esperadas), testa aderencia.
#'
#' @param x Vetor categorico (independencia) ou de contagens (aderencia).
#' @param y Vetor categorico (independencia). Default `NULL`.
#' @param p Vetor de probabilidades esperadas (aderencia). Default `NULL`.
#' @param corrigir Logico. Correcao de continuidade de Yates (2x2).
#' @param digits Inteiro.
#'
#' @return tibble com `estatistica`, `gl`, `p_valor`, `metodo` e, na
#'   independencia, `v_cramer`.
#'
#' @examples
#' rnp_teste_qui_quadrado(mtcars$cyl, mtcars$gear)
#' rnp_teste_qui_quadrado(c(20, 30, 50), p = c(0.25, 0.25, 0.5))
#' @family inferencia
#' @export
rnp_teste_qui_quadrado <- function(x, y = NULL, p = NULL, corrigir = FALSE,
                                   digits = 4L) {
  if (!is.null(y)) {
    tb <- table(x, y)
    ct <- stats::chisq.test(tb, correct = corrigir)
    v <- sqrt(as.numeric(ct$statistic) / (sum(tb) * (min(dim(tb)) - 1)))
    tibble::tibble(
      estatistica = arredonda(unname(ct$statistic), digits),
      gl          = unname(ct$parameter),
      p_valor     = arredonda(ct$p.value, digits),
      v_cramer    = arredonda(v, digits),
      metodo      = "independencia"
    )
  } else if (!is.null(p)) {
    ct <- stats::chisq.test(x = as.numeric(x), p = p)
    tibble::tibble(
      estatistica = arredonda(unname(ct$statistic), digits),
      gl          = unname(ct$parameter),
      p_valor     = arredonda(ct$p.value, digits),
      metodo      = "aderencia"
    )
  } else {
    rlang::abort("Informe {.arg y} (independencia) ou {.arg p} (aderencia).")
  }
}

#' Teste de aderencia qui-quadrado a uma distribuicao continua
#'
#' Agrupa os dados em classes equiprovaveis sob a distribuicao especificada e
#' compara as frequencias observadas e esperadas via qui-quadrado.
#'
#' @param x Vetor numerico.
#' @param dist String aceita por [rnp_distribuicao()] (ex.: `"norm"`).
#' @param ... Parametros da distribuicao (ex.: `mean`, `sd`).
#' @param k Inteiro. Numero de classes.
#' @param par_estimados Inteiro. Numero de parametros estimados (ajusta os gl).
#' @param digits Inteiro.
#'
#' @return tibble com `estatistica`, `gl`, `p_valor`, `k`.
#'
#' @examples
#' set.seed(1)
#' rnp_teste_aderencia(rnorm(300), "norm", mean = 0, sd = 1, k = 8)
#' @family inferencia
#' @export
rnp_teste_aderencia <- function(x, dist, ..., k = 10L, par_estimados = 0L,
                                digits = 4L) {
  abort_numerico(x, "x")
  x <- sem_na(x)
  n <- length(x)
  probs <- seq(0, 1, length.out = k + 1L)
  limites <- do.call(rnp_distribuicao, c(list(dist, "q", p = probs[-c(1, k + 1L)]), list(...)))
  quebras <- c(-Inf, limites, Inf)
  obs <- as.integer(table(cut(x, breaks = quebras)))
  esp <- rep(n / k, k)  # classes equiprovaveis
  est <- sum((obs - esp)^2 / esp)
  gl <- k - 1L - par_estimados
  tibble::tibble(
    estatistica = arredonda(est, digits),
    gl          = gl,
    p_valor     = arredonda(stats::pchisq(est, gl, lower.tail = FALSE), digits),
    k           = k
  )
}

#' Teste de Kolmogorov-Smirnov
#'
#' Uma amostra (contra uma distribuicao) ou duas amostras.
#'
#' @param x Vetor numerico.
#' @param y Vetor numerico (duas amostras) ou `NULL`.
#' @param dist String com a distribuicao acumulada (ex.: `"pnorm"`) quando
#'   `y` for `NULL`.
#' @param ... Parametros da distribuicao.
#' @param digits Inteiro.
#'
#' @return tibble com `estatistica`, `p_valor`, `metodo`.
#'
#' @examples
#' set.seed(1)
#' rnp_teste_ks(rnorm(100), dist = "pnorm", mean = 0, sd = 1)
#' rnp_teste_ks(rnorm(50), rnorm(50, 1))
#' @family inferencia
#' @export
rnp_teste_ks <- function(x, y = NULL, dist = "pnorm", ..., digits = 4L) {
  abort_numerico(x, "x")
  res <- if (!is.null(y)) {
    suppressWarnings(stats::ks.test(x, y))
  } else {
    suppressWarnings(stats::ks.test(x, dist, ...))
  }
  tibble::tibble(
    estatistica = arredonda(unname(res$statistic), digits),
    p_valor     = arredonda(res$p.value, digits),
    metodo      = if (is.null(y)) "uma amostra" else "duas amostras"
  )
}

#' Teste de proporcoes (uma ou k amostras)
#'
#' Wrapper de [stats::prop.test()].
#'
#' @param sucessos Vetor de sucessos.
#' @param n Vetor de tamanhos amostrais.
#' @param p0 Proporcao sob H0 (apenas uma amostra). Default `NULL`.
#' @param corrigir Logico. Correcao de continuidade.
#' @param digits Inteiro.
#'
#' @return tibble com `estatistica`, `gl`, `p_valor`, `proporcoes`.
#'
#' @examples
#' rnp_teste_proporcoes(c(45, 55), c(100, 100))
#' rnp_teste_proporcoes(45, 100, p0 = 0.5)
#' @family inferencia
#' @export
rnp_teste_proporcoes <- function(sucessos, n, p0 = NULL, corrigir = TRUE,
                                 digits = 4L) {
  res <- if (is.null(p0)) {
    stats::prop.test(sucessos, n, correct = corrigir)
  } else {
    stats::prop.test(sucessos, n, p = p0, correct = corrigir)
  }
  tibble::tibble(
    estatistica = arredonda(unname(res$statistic), digits),
    gl          = unname(res$parameter),
    p_valor     = arredonda(res$p.value, digits),
    proporcoes  = paste(round(res$estimate, digits), collapse = ", ")
  )
}

#' Teste binomial exato
#'
#' Wrapper de [stats::binom.test()].
#'
#' @param sucessos Numero de sucessos.
#' @param n Tamanho da amostra.
#' @param p0 Proporcao sob H0.
#' @param lado String: `"bilateral"`, `"direita"`, `"esquerda"`.
#' @param digits Inteiro.
#'
#' @return tibble com `proporcao`, `p_valor`, `ic_inf`, `ic_sup`, `alternativa`.
#'
#' @examples
#' rnp_teste_binomial(8, 10, p0 = 0.5)
#' @family inferencia
#' @export
rnp_teste_binomial <- function(sucessos, n, p0 = 0.5,
                               lado = c("bilateral", "direita", "esquerda"),
                               digits = 4L) {
  abort_inteiro_pos(n, "n")
  abort_proporcao(p0, "p0")
  lado <- check_lado(lado)
  alt <- switch(lado, bilateral = "two.sided", direita = "greater", esquerda = "less")
  res <- stats::binom.test(sucessos, n, p = p0, alternative = alt)
  tibble::tibble(
    proporcao   = arredonda(unname(res$estimate), digits),
    p_valor     = arredonda(res$p.value, digits),
    ic_inf      = arredonda(res$conf.int[1L], digits),
    ic_sup      = arredonda(res$conf.int[2L], digits),
    alternativa = lado
  )
}

# Poder de um teste t (uma ou duas amostras) via t nao-central.
.poder_t <- function(d, n, alpha, tipo, lado) {
  if (tipo == "duas") {
    gl <- 2 * n - 2
    ncp <- d * sqrt(n / 2)
  } else {
    gl <- n - 1
    ncp <- d * sqrt(n)
  }
  if (lado == "bilateral") {
    tc <- stats::qt(1 - alpha / 2, gl)
    stats::pt(tc, gl, ncp, lower.tail = FALSE) +
      stats::pt(-tc, gl, ncp, lower.tail = TRUE)
  } else {
    tc <- stats::qt(1 - alpha, gl)
    stats::pt(tc, gl, ncp, lower.tail = FALSE)
  }
}

#' Poder de um teste t e curva de poder
#'
#' Calcula o poder de um teste t (uma ou duas amostras) para um tamanho de
#' efeito (d de Cohen) e devolve tambem a curva de poder versus n.
#'
#' @param efeito Tamanho de efeito d de Cohen.
#' @param n Tamanho amostral (por grupo, se duas amostras).
#' @param alpha Nivel de significancia.
#' @param tipo String: `"uma"` ou `"duas"` amostras.
#' @param lado String: `"bilateral"`, `"direita"`, `"esquerda"`.
#' @param digits Inteiro.
#'
#' @return Uma lista com `poder` (tibble) e `grafico` (ggplot da curva de poder).
#'
#' @examples
#' rnp_poder_teste(efeito = 0.5, n = 30, tipo = "duas")$poder
#' @family inferencia
#' @export
rnp_poder_teste <- function(efeito, n, alpha = 0.05,
                            tipo = c("duas", "uma"),
                            lado = c("bilateral", "direita", "esquerda"),
                            digits = 4L) {
  tipo <- rlang::arg_match(tipo)
  lado <- check_lado(lado)
  check_alpha(alpha)
  pot <- .poder_t(efeito, n, alpha, tipo, lado)
  ns <- seq(max(2, n - 25), n + 50)
  curva <- tibble::tibble(
    n = ns,
    poder = vapply(ns, function(nn) .poder_t(efeito, nn, alpha, tipo, lado), numeric(1))
  )
  g <- ggplot2::ggplot(curva, ggplot2::aes(x = .data$n, y = .data$poder)) +
    ggplot2::geom_line(color = rnp_paleta_rnp("rnp_qual", 1), linewidth = 1) +
    ggplot2::geom_hline(yintercept = 0.8, linetype = "dashed", color = "grey50") +
    ggplot2::geom_point(data = tibble::tibble(n = n, poder = pot),
                        color = "red", size = 2) +
    rnp_tema_rnp() +
    ggplot2::labs(title = "Curva de poder", x = "n", y = "Poder")
  list(
    poder = tibble::tibble(efeito = efeito, n = n, alpha = alpha,
                           poder = arredonda(pot, digits), tipo = tipo),
    grafico = g
  )
}

#' Tamanho de amostra por poder (teste t)
#'
#' Encontra o menor `n` que atinge o poder desejado para um teste t.
#'
#' @inheritParams rnp_poder_teste
#' @param poder Poder desejado (ex.: 0.80).
#'
#' @return tibble com `efeito`, `poder_alvo`, `alpha`, `n`, `poder_obtido`.
#'
#' @examples
#' rnp_tamanho_amostra_teste(efeito = 0.5, poder = 0.8, tipo = "duas")
#' @family inferencia
#' @export
rnp_tamanho_amostra_teste <- function(efeito, poder = 0.8, alpha = 0.05,
                                      tipo = c("duas", "uma"),
                                      lado = c("bilateral", "direita", "esquerda"),
                                      digits = 4L) {
  tipo <- rlang::arg_match(tipo)
  lado <- check_lado(lado)
  check_alpha(alpha)
  if (poder <= 0 || poder >= 1) rlang::abort("{.arg poder} deve estar em (0, 1).")
  n <- 2L
  repeat {
    if (.poder_t(efeito, n, alpha, tipo, lado) >= poder || n > 1e6) break
    n <- n + 1L
  }
  tibble::tibble(
    efeito = efeito, poder_alvo = poder, alpha = alpha,
    n = n, poder_obtido = arredonda(.poder_t(efeito, n, alpha, tipo, lado), digits)
  )
}

#' Testes de normalidade (Shapiro, Jarque-Bera, Anderson-Darling)
#'
#' @param x Vetor numerico.
#' @param metodo String: `"shapiro"`, `"jarque_bera"` ou `"anderson_darling"`.
#' @param digits Inteiro.
#'
#' @return tibble com `estatistica`, `p_valor`, `metodo`.
#'
#' @examples
#' set.seed(1)
#' rnp_teste_normalidade(rnorm(200), "jarque_bera")
#' @family inferencia
#' @export
rnp_teste_normalidade <- function(x, metodo = c("shapiro", "jarque_bera",
                                                "anderson_darling"),
                                  digits = 4L) {
  abort_numerico(x, "x")
  metodo <- rlang::arg_match(metodo)
  x <- sem_na(x)
  n <- length(x)
  res <- switch(metodo,
    shapiro = {
      sw <- stats::shapiro.test(x)
      c(unname(sw$statistic), sw$p.value)
    },
    jarque_bera = {
      m <- mean(x); s2 <- mean((x - m)^2)
      S <- mean((x - m)^3) / s2^1.5
      K <- mean((x - m)^4) / s2^2
      jb <- n / 6 * (S^2 + (K - 3)^2 / 4)
      c(jb, stats::pchisq(jb, 2, lower.tail = FALSE))
    },
    anderson_darling = {
      xs <- sort(x)
      z <- stats::pnorm((xs - mean(xs)) / stats::sd(xs))
      z <- pmin(pmax(z, 1e-12), 1 - 1e-12)
      i <- seq_len(n)
      a2 <- -n - sum((2 * i - 1) * (log(z) + log(1 - rev(z)))) / n
      a2s <- a2 * (1 + 0.75 / n + 2.25 / n^2)
      pv <- if (a2s < 0.2) 1 - exp(-13.436 + 101.14 * a2s - 223.73 * a2s^2)
            else if (a2s < 0.34) 1 - exp(-8.318 + 42.796 * a2s - 59.938 * a2s^2)
            else if (a2s < 0.6) exp(0.9177 - 4.279 * a2s - 1.38 * a2s^2)
            else exp(1.2937 - 5.709 * a2s + 0.0186 * a2s^2)
      c(a2, max(0, min(1, pv)))
    }
  )
  tibble::tibble(
    estatistica = arredonda(res[1L], digits),
    p_valor     = arredonda(res[2L], digits),
    metodo      = metodo
  )
}

#' Teste de Grubbs para outlier
#'
#' Detecta um unico outlier (o valor mais extremo) sob normalidade.
#'
#' @param x Vetor numerico.
#' @param digits Inteiro.
#'
#' @return tibble com `valor_extremo`, `estatistica`, `p_valor`.
#'
#' @examples
#' rnp_teste_grubbs(c(rnorm(30), 8))
#' @family inferencia
#' @export
rnp_teste_grubbs <- function(x, digits = 4L) {
  abort_numerico(x, "x")
  x <- sem_na(x)
  n <- length(x)
  if (n < 3L) rlang::abort("Grubbs requer n >= 3.")
  m <- mean(x); s <- stats::sd(x)
  desvios <- abs(x - m)
  i <- which.max(desvios)
  G <- desvios[i] / s
  t2 <- (n * (n - 2) * G^2) / ((n - 1)^2 - n * G^2)
  p <- n * stats::pt(sqrt(t2), df = n - 2, lower.tail = FALSE)
  tibble::tibble(
    valor_extremo = arredonda(x[i], digits),
    estatistica   = arredonda(G, digits),
    p_valor       = arredonda(min(1, p), digits)
  )
}

#' Teste de aleatoriedade (runs / sequencias)
#'
#' Testa a aleatoriedade de uma sequencia pela contagem de sequencias (runs)
#' acima/abaixo da mediana, com aproximacao normal.
#'
#' @param x Vetor numerico.
#' @param digits Inteiro.
#'
#' @return tibble com `runs`, `esperado`, `z`, `p_valor`.
#'
#' @examples
#' set.seed(1)
#' rnp_teste_runs(rnorm(100))
#' @family inferencia
#' @export
rnp_teste_runs <- function(x, digits = 4L) {
  abort_numerico(x, "x")
  x <- sem_na(x)
  med <- stats::median(x)
  s <- sign(x - med)
  s <- s[s != 0]
  n1 <- sum(s > 0); n2 <- sum(s < 0); n <- n1 + n2
  if (n1 == 0 || n2 == 0) rlang::abort("Sequencia sem variacao em torno da mediana.")
  runs <- 1L + sum(s[-1L] != s[-length(s)])
  mu <- 2 * n1 * n2 / n + 1
  sigma2 <- 2 * n1 * n2 * (2 * n1 * n2 - n) / (n^2 * (n - 1))
  z <- (runs - mu) / sqrt(sigma2)
  tibble::tibble(
    runs     = runs,
    esperado = arredonda(mu, digits),
    z        = arredonda(z, digits),
    p_valor  = arredonda(2 * stats::pnorm(-abs(z)), digits)
  )
}

#' Teste dos sinais (nao-parametrico)
#'
#' Testa se a mediana das diferencas (pareado) ou de `x - mu` e zero, contando
#' os sinais e usando a distribuicao binomial.
#'
#' @param x Vetor numerico.
#' @param y Vetor numerico (pareado) ou `NULL`.
#' @param mu Valor de referencia (uma amostra).
#' @param digits Inteiro.
#'
#' @return tibble com `n_positivos`, `n_negativos`, `p_valor`.
#'
#' @examples
#' rnp_teste_sinais(c(5, 6, 7, 4, 8), mu = 5)
#' @family inferencia
#' @export
rnp_teste_sinais <- function(x, y = NULL, mu = 0, digits = 4L) {
  abort_numerico(x, "x")
  d <- if (!is.null(y)) {
    if (length(x) != length(y)) rlang::abort("x e y devem ter o mesmo comprimento.")
    x - y
  } else {
    x - mu
  }
  d <- d[d != 0 & !is.na(d)]
  n <- length(d)
  if (n < 1L) rlang::abort("Sem diferencas nao-nulas.")
  pos <- sum(d > 0)
  p <- stats::binom.test(pos, n, p = 0.5)$p.value
  tibble::tibble(
    n_positivos = pos,
    n_negativos = n - pos,
    p_valor     = arredonda(p, digits)
  )
}
