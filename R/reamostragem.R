# Reamostragem (FATIA 2): bootstrap, jackknife, bootstrap parametrico e teste
# de permutacao. Estatisticas comuns usam backend C++ (C-05); estatisticas
# arbitrarias (funcoes R) usam laco em R.

# Mapeia nome -> id do backend C++ (ou NA se for estatistica customizada).
.estat_id <- function(estatistica) {
  if (is.character(estatistica)) {
    switch(estatistica,
      media = 0L, mediana = 1L, dp = 2L, desvio = 2L, variancia = 3L, var = 3L,
      rlang::abort("Estatistica '{estatistica}' desconhecida. Use media, mediana, dp ou variancia, ou passe uma funcao."))
  } else if (is.function(estatistica)) {
    NA_integer_
  } else {
    rlang::abort("{.arg estatistica} deve ser string ou funcao.")
  }
}

.estat_original <- function(x, estatistica) {
  if (is.function(estatistica)) return(estatistica(x))
  switch(.estat_id(estatistica) + 1L,
    mean(x), stats::median(x), stats::sd(x), stats::var(x))
}

#' Bootstrap (nao parametrico)
#'
#' Gera `B` replicas bootstrap de uma estatistica e estima vies e erro-padrao.
#' Estatisticas comuns (`"media"`, `"mediana"`, `"dp"`, `"variancia"`) usam
#' backend C++; uma funcao R arbitraria tambem e aceita.
#'
#' @param x Vetor numerico.
#' @param estatistica String (`"media"`, `"mediana"`, `"dp"`, `"variancia"`) ou
#'   funcao `function(x) ...`.
#' @param B Inteiro. Numero de replicas.
#' @param na.rm Logico.
#' @param seed Inteiro. Semente.
#' @param digits Inteiro.
#'
#' @return Uma lista com `replicas` (vetor) e `resumo` (tibble com
#'   `estimativa`, `media_boot`, `vies`, `erro_padrao`, `B`).
#'
#' @examples
#' set.seed(1)
#' rnp_bootstrap(rnorm(100), "media", B = 1000)$resumo
#' @family inferencia
#' @export
rnp_bootstrap <- function(x, estatistica = "media", B = 2000L, na.rm = TRUE,
                          seed = 42L, digits = 4L) {
  abort_numerico(x, "x")
  abort_inteiro_pos(B, "B")
  if (na.rm) x <- sem_na(x)
  if (length(x) < 2L) rlang::abort("Necessario n >= 2 observacoes validas.")
  id <- .estat_id(estatistica)
  set.seed(seed)
  reps <- if (is.na(id)) {
    vapply(seq_len(B), function(i) estatistica(sample(x, replace = TRUE)), numeric(1))
  } else {
    as.numeric(bootstrap_stat_cpp(as.numeric(x), as.integer(B), id))
  }
  est0 <- .estat_original(x, estatistica)
  tibble_resumo <- tibble::tibble(
    estimativa  = arredonda(est0, digits),
    media_boot  = arredonda(mean(reps), digits),
    vies        = arredonda(mean(reps) - est0, digits),
    erro_padrao = arredonda(stats::sd(reps), digits),
    B           = B
  )
  list(replicas = reps, resumo = tibble_resumo)
}

#' Intervalo de confianca bootstrap
#'
#' Calcula IC bootstrap pelos metodos percentil, normal, basico ou BCa
#' (bias-corrected and accelerated).
#'
#' @inheritParams rnp_bootstrap
#' @param conf Nivel de confianca.
#' @param tipo String: `"percentil"`, `"normal"`, `"basico"` ou `"bca"`.
#'
#' @return tibble com `estimativa`, `limite_inferior`, `limite_superior`,
#'   `metodo`, `conf`.
#'
#' @examples
#' set.seed(1)
#' rnp_ic_bootstrap(rnorm(100, 10, 2), "media", B = 1000, tipo = "percentil")
#' @family inferencia
#' @export
rnp_ic_bootstrap <- function(x, estatistica = "media", B = 2000L, conf = 0.95,
                             tipo = c("percentil", "normal", "basico", "bca"),
                             na.rm = TRUE, seed = 42L, digits = 4L) {
  tipo <- rlang::arg_match(tipo)
  abort_confianca(conf)
  bt <- rnp_bootstrap(x, estatistica, B = B, na.rm = na.rm, seed = seed,
                      digits = 12L)
  reps <- bt$replicas
  est0 <- bt$resumo$estimativa
  alpha <- 1 - conf
  limites <- switch(tipo,
    percentil = stats::quantile(reps, c(alpha / 2, 1 - alpha / 2), names = FALSE),
    normal = {
      se <- stats::sd(reps); z <- stats::qnorm(1 - alpha / 2)
      vies <- mean(reps) - est0
      c(est0 - vies - z * se, est0 - vies + z * se)
    },
    basico = {
      qs <- stats::quantile(reps, c(alpha / 2, 1 - alpha / 2), names = FALSE)
      c(2 * est0 - qs[2L], 2 * est0 - qs[1L])
    },
    bca = {
      if (na.rm) x <- sem_na(x)
      z0 <- stats::qnorm(mean(reps < est0))
      jk <- if (is.function(estatistica)) {
        vapply(seq_along(x), function(i) estatistica(x[-i]), numeric(1))
      } else {
        as.numeric(jackknife_stat_cpp(as.numeric(x), .estat_id(estatistica)))
      }
      jbar <- mean(jk)
      a <- sum((jbar - jk)^3) / (6 * (sum((jbar - jk)^2))^1.5)
      za <- stats::qnorm(c(alpha / 2, 1 - alpha / 2))
      adj <- stats::pnorm(z0 + (z0 + za) / (1 - a * (z0 + za)))
      stats::quantile(reps, adj, names = FALSE)
    }
  )
  tibble::tibble(
    estimativa      = arredonda(est0, digits),
    limite_inferior = arredonda(limites[1L], digits),
    limite_superior = arredonda(limites[2L], digits),
    metodo          = tipo,
    conf            = conf
  )
}

#' Jackknife (deixa-um-de-fora)
#'
#' Estima vies e erro-padrao de uma estatistica pelo metodo jackknife.
#'
#' @inheritParams rnp_bootstrap
#'
#' @return Uma lista com `replicas` (vetor) e `resumo` (tibble com
#'   `estimativa`, `vies`, `erro_padrao`, `n`).
#'
#' @examples
#' rnp_jackknife(rnorm(50), "media")$resumo
#' @family inferencia
#' @export
rnp_jackknife <- function(x, estatistica = "media", na.rm = TRUE, digits = 4L) {
  abort_numerico(x, "x")
  if (na.rm) x <- sem_na(x)
  n <- length(x)
  if (n < 2L) rlang::abort("Jackknife requer n >= 2.")
  id <- .estat_id(estatistica)
  reps <- if (is.na(id)) {
    vapply(seq_len(n), function(i) estatistica(x[-i]), numeric(1))
  } else {
    as.numeric(jackknife_stat_cpp(as.numeric(x), id))
  }
  est0 <- .estat_original(x, estatistica)
  jbar <- mean(reps)
  vies <- (n - 1) * (jbar - est0)
  ep <- sqrt((n - 1) / n * sum((reps - jbar)^2))
  list(
    replicas = reps,
    resumo = tibble::tibble(
      estimativa  = arredonda(est0, digits),
      vies        = arredonda(vies, digits),
      erro_padrao = arredonda(ep, digits),
      n           = n
    )
  )
}

#' Bootstrap parametrico
#'
#' Ajusta uma distribuicao por maxima verossimilhanca e gera replicas
#' bootstrap reamostrando da distribuicao ajustada.
#'
#' @param x Vetor numerico.
#' @param dist String aceita por [rnp_ajuste_distribuicao()].
#' @param estatistica Funcao `function(x) ...` da estatistica de interesse.
#' @param B Inteiro. Numero de replicas.
#' @param seed Inteiro. Semente.
#' @param digits Inteiro.
#'
#' @return Uma lista com `replicas` e `resumo` (tibble).
#'
#' @examples
#' set.seed(1)
#' rnp_bootstrap_parametrico(rexp(100, 0.5), "exp", function(x) mean(x))$resumo
#' @family inferencia
#' @export
rnp_bootstrap_parametrico <- function(x, dist, estatistica, B = 2000L,
                                      seed = 42L, digits = 4L) {
  abort_numerico(x, "x")
  if (!is.function(estatistica)) rlang::abort("{.arg estatistica} deve ser funcao.")
  abort_inteiro_pos(B, "B")
  x <- sem_na(x)
  n <- length(x)
  aj <- rnp_ajuste_distribuicao(x, dist, digits = 12L)
  par <- aj$parametros$estimativa
  gerador <- switch(dist,
    norm    = function() stats::rnorm(n, par[1], par[2]),
    exp     = function() stats::rexp(n, par[1]),
    pois    = function() stats::rpois(n, par[1]),
    gamma   = function() stats::rgamma(n, par[1], par[2]),
    lnorm   = function() stats::rlnorm(n, par[1], par[2]),
    weibull = function() stats::rweibull(n, par[1], par[2])
  )
  set.seed(seed)
  reps <- vapply(seq_len(B), function(i) estatistica(gerador()), numeric(1))
  est0 <- estatistica(x)
  list(
    replicas = reps,
    resumo = tibble::tibble(
      estimativa  = arredonda(est0, digits),
      media_boot  = arredonda(mean(reps), digits),
      vies        = arredonda(mean(reps) - est0, digits),
      erro_padrao = arredonda(stats::sd(reps), digits),
      B           = B
    )
  )
}

#' Teste de permutacao (diferenca de medias)
#'
#' Testa a igualdade de medias de dois grupos por reamostragem dos rotulos
#' (teste de permutacao), com backend C++. Hipotese nula: distribuicoes iguais.
#'
#' @param x,y Vetores numericos dos dois grupos.
#' @param B Inteiro. Numero de permutacoes.
#' @param lado String: `"bilateral"`, `"direita"`, `"esquerda"`.
#' @param seed Inteiro. Semente.
#' @param digits Inteiro.
#'
#' @return tibble com `diff_observada`, `p_valor`, `B`, `alternativa`.
#'
#' @examples
#' set.seed(1)
#' rnp_teste_permutacao(rnorm(30, 0), rnorm(30, 0.8), B = 2000)
#' @family inferencia
#' @export
rnp_teste_permutacao <- function(x, y, B = 5000L,
                                 lado = c("bilateral", "direita", "esquerda"),
                                 seed = 42L, digits = 4L) {
  abort_numerico(x, "x"); abort_numerico(y, "y")
  lado <- check_lado(lado)
  abort_inteiro_pos(B, "B")
  x <- sem_na(x); y <- sem_na(y)
  dif_obs <- mean(x) - mean(y)
  set.seed(seed)
  dist <- as.numeric(permutacao_difmedias_cpp(as.numeric(x), as.numeric(y),
                                              as.integer(B)))
  p <- switch(lado,
    bilateral = mean(abs(dist) >= abs(dif_obs)),
    direita   = mean(dist >= dif_obs),
    esquerda  = mean(dist <= dif_obs)
  )
  tibble::tibble(
    diff_observada = arredonda(dif_obs, digits),
    p_valor        = arredonda(p, digits),
    B              = B,
    alternativa    = lado
  )
}
