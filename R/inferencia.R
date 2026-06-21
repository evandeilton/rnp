#' Intervalo de confianca para a media
#'
#' IC para a media populacional usando distribuicao t (default) ou Z
#' (quando sigma conhecido ou amostra grande).
#'
#' @param x Vetor numerico.
#' @param conf Nivel de confianca (ex.: 0.95).
#' @param sigma Desvio-padrao populacional, se conhecido. Default NULL -> t.
#' @param na.rm Logico.
#' @param digits Inteiro.
#'
#' @return tibble com colunas \code{media}, \code{erro_padrao},
#'   \code{limite_inferior}, \code{limite_superior}, \code{n},
#'   \code{nivel_confianca}, \code{distribuicao}.
#'
#' @examples
#' rnp_ic_media(rnorm(50, mean = 10, sd = 2))
#' rnp_ic_media(rnorm(50), sigma = 1)
#' @export
rnp_ic_media <- function(x, conf = 0.95, sigma = NULL, na.rm = TRUE, digits = 4L) {
  abort_numerico(x, "x")
  abort_confianca(conf)
  if (na.rm) x <- sem_na(x)
  n <- length(x)
  if (n < 2L) rlang::abort("IC para media requer n >= 2.")
  mx <- mean(x)
  if (!is.null(sigma)) {
    if (!is.numeric(sigma) || length(sigma) != 1L || sigma <= 0) {
      rlang::abort("{.arg sigma} deve ser escalar positivo.")
    }
    se <- sigma / sqrt(n)
    crit <- stats::qnorm((1 + conf) / 2)
    dist <- "normal"
  } else {
    se <- stats::sd(x) / sqrt(n)
    crit <- stats::qt((1 + conf) / 2, df = n - 1L)
    dist <- "t"
  }
  li <- mx - crit * se
  ls <- mx + crit * se
  tibble::tibble(
    media           = mx,
    erro_padrao     = se,
    limite_inferior = li,
    limite_superior = ls,
    n               = n,
    nivel_confianca = conf,
    distribuicao    = dist
  ) |>
    dplyr::mutate(dplyr::across(where(is.numeric),
                                ~ arredonda(.x, digits)))
}

#' Intervalo de confianca para proporcao
#'
#' Metodo de Wald (default), Wilson, Agresti-Coull ou Clopper-Pearson (exact).
#'
#' @param sucesso Numerico. Numero de sucessos.
#' @param n Inteiro. Tamanho da amostra.
#' @param conf Nivel de confianca.
#' @param method String: \code{"wald"}, \code{"wilson"}, \code{"agresti"},
#'   \code{"clopper"}.
#' @param digits Inteiro.
#'
#' @return tibble com \code{proporcao}, \code{limite_inferior},
#'   \code{limite_superior}, \code{metodo}, \code{n}.
#'
#' @examples
#' rnp_ic_proporcao(45, 100)
#' rnp_ic_proporcao(45, 100, method = "wilson")
#' @export
rnp_ic_proporcao <- function(sucesso, n, conf = 0.95,
                             method = c("wald", "wilson", "agresti", "clopper"),
                             digits = 4L) {
  abort_inteiro_pos(n, "n")
  if (!is.numeric(sucesso) || length(sucesso) != 1L ||
      sucesso < 0 || sucesso > n || is.na(sucesso)) {
    rlang::abort("{.arg sucesso} deve estar em [0, n].")
  }
  abort_confianca(conf)
  method <- rlang::arg_match(method)
  p_hat <- sucesso / n
  alpha <- 1 - conf
  z <- stats::qnorm(1 - alpha / 2)

  res <- switch(method,
    wald = {
      se <- sqrt(p_hat * (1 - p_hat) / n)
      c(p_hat - z * se, p_hat + z * se)
    },
    wilson = {
      denom <- 1 + z^2 / n
      center <- (p_hat + z^2 / (2 * n)) / denom
      margin <- z * sqrt(p_hat * (1 - p_hat) / n + z^2 / (4 * n^2)) / denom
      c(center - margin, center + margin)
    },
    agresti = {
      n_tilde <- n + z^2
      p_tilde <- (sucesso + z^2 / 2) / n_tilde
      se <- sqrt(p_tilde * (1 - p_tilde) / n_tilde)
      c(p_tilde - z * se, p_tilde + z * se)
    },
    clopper = {
      c(stats::qbeta(alpha / 2, sucesso, n - sucesso + 1),
        stats::qbeta(1 - alpha / 2, sucesso + 1, n - sucesso))
    }
  )
  res <- pmin(pmax(res, 0), 1)
  tibble::tibble(
    proporcao        = p_hat,
    limite_inferior  = res[1L],
    limite_superior  = res[2L],
    metodo           = method,
    n                = n
  ) |>
    dplyr::mutate(dplyr::across(where(is.numeric) & !n,
                                ~ arredonda(.x, digits)))
}

#' Intervalo de confianca para variancia (chi-quadrado)
#'
#' @param x Vetor numerico.
#' @param conf Nivel de confianca.
#' @param na.rm Logico.
#' @param digits Inteiro.
#' @return tibble com \code{variancia}, \code{limite_inferior},
#'   \code{limite_superior}, \code{n}, \code{gl}.
#' @examples
#' rnp_ic_variancia(rnorm(30, sd = 2))
#' @export
rnp_ic_variancia <- function(x, conf = 0.95, na.rm = TRUE, digits = 4L) {
  abort_numerico(x, "x")
  abort_confianca(conf)
  if (na.rm) x <- sem_na(x)
  n <- length(x)
  if (n < 2L) rlang::abort("IC para variancia requer n >= 2.")
  v <- stats::var(x)
  gl <- n - 1L
  alpha <- 1 - conf
  chi_inf <- stats::qchisq(1 - alpha / 2, df = gl)
  chi_sup <- stats::qchisq(alpha / 2, df = gl)
  li <- gl * v / chi_inf
  ls <- gl * v / chi_sup
  tibble::tibble(
    variancia        = v,
    limite_inferior  = li,
    limite_superior  = ls,
    n                = n,
    gl               = gl
  ) |>
    dplyr::mutate(dplyr::across(where(is.numeric) & !n & !gl,
                                ~ arredonda(.x, digits)))
}

#' Intervalo de confianca para diferenca de medias
#'
#' @param x,y Vetores numericos.
#' @param pareado Logico. Amostras pareadas.
#' @param var_iguais Logico. Assumir variancias iguais (independentes).
#' @param conf Nivel de confianca.
#' @param na.rm Logico.
#' @param digits Inteiro.
#' @return tibble com \code{diff_medias}, \code{erro_padrao},
#'   \code{limite_inferior}, \code{limite_superior}, \code{gl},
#'   \code{metodo}.
#' @examples
#' rnp_ic_diff_medias(rnorm(30, 5), rnorm(30, 5.5))
#' @export
rnp_ic_diff_medias <- function(x, y, pareado = FALSE, var_iguais = FALSE,
                               conf = 0.95, na.rm = TRUE, digits = 4L) {
  abort_numerico(x, "x")
  abort_numerico(y, "y")
  abort_confianca(conf)
  if (pareado) {
    if (length(x) != length(y)) {
      rlang::abort("Amostras pareadas devem ter o mesmo tamanho.")
    }
    d <- x - y
    if (na.rm) d <- sem_na(d)
    n <- length(d)
    if (n < 2L) rlang::abort("IC pareado requer n >= 2.")
    md <- mean(d)
    se <- stats::sd(d) / sqrt(n)
    crit <- stats::qt((1 + conf) / 2, df = n - 1L)
    return(tibble::tibble(
      diff_medias      = md,
      erro_padrao      = se,
      limite_inferior  = md - crit * se,
      limite_superior  = md + crit * se,
      gl               = n - 1L,
      metodo           = "pareado"
    ) |> dplyr::mutate(dplyr::across(where(is.numeric) & !gl,
                                     ~ arredonda(.x, digits))))
  }
  if (na.rm) { x <- sem_na(x); y <- sem_na(y) }
  nx <- length(x); ny <- length(y)
  if (nx < 2L || ny < 2L) rlang::abort("Cada amostra requer n >= 2.")
  mx <- mean(x); my <- mean(y)
  vx <- stats::var(x); vy <- stats::var(y)
  diff <- mx - my
  if (var_iguais) {
    sp2 <- ((nx - 1) * vx + (ny - 1) * vy) / (nx + ny - 2)
    se <- sqrt(sp2 * (1 / nx + 1 / ny))
    gl <- nx + ny - 2
    metodo <- "t (variancias iguais)"
  } else {
    se <- sqrt(vx / nx + vy / ny)
    num <- (vx / nx + vy / ny)^2
    den <- (vx / nx)^2 / (nx - 1) + (vy / ny)^2 / (ny - 1)
    gl <- num / den
    metodo <- "t (Welch)"
  }
  crit <- stats::qt((1 + conf) / 2, df = gl)
  tibble::tibble(
    diff_medias      = diff,
    erro_padrao      = se,
    limite_inferior  = diff - crit * se,
    limite_superior  = diff + crit * se,
    gl               = gl,
    metodo           = metodo
  ) |> dplyr::mutate(dplyr::across(where(is.numeric) & !gl,
                                   ~ arredonda(.x, digits)))
}

#' Teste t (uma ou duas amostras)
#'
#' Wrapper consolidado para [stats::t.test()].
#'
#' @param x,y Vetores numericos (y ausente -> uma amostra).
#' @param mu Hipotese nula para a media.
#' @param pareado Logico.
#' @param var_iguais Logico.
#' @param lado String: \code{"bilateral"}, \code{"direita"}, \code{"esquerda"}.
#' @param conf Nivel de confianca do IC.
#' @param digits Inteiro.
#'
#' @return tibble com \code{estatistica}, \code{gl}, \code{p_valor},
#'   \code{media_x}, \code{media_y}, \code{diff}, \code{ic_inf}, \code{ic_sup},
#'   \code{hipotese_nula}, \code{alternativa}.
#'
#' @examples
#' rnp_teste_t(rnorm(30, mean = 5), mu = 5)
#' rnp_teste_t(rnorm(30, 5), rnorm(30, 5.5))
#' @export
rnp_teste_t <- function(x, y = NULL, mu = 0, pareado = FALSE,
                        var_iguais = FALSE,
                        lado = c("bilateral", "direita", "esquerda"),
                        conf = 0.95, digits = 4L) {
  abort_numerico(x, "x")
  lado <- check_lado(lado)
  alt <- switch(lado, bilateral = "two.sided", direita = "greater", esquerda = "less")
  tt <- stats::t.test(x = x, y = y, alternative = alt, mu = mu,
                      paired = pareado, var.equal = var_iguais,
                      conf.level = conf)
  out <- tibble::tibble(
    estatistica  = unname(tt$statistic),
    gl           = unname(tt$parameter),
    p_valor      = tt$p.value,
    media_x      = unname(tt$estimate[1L]),
    media_y      = if (length(tt$estimate) > 1L) unname(tt$estimate[2L]) else NA_real_,
    diff         = if (!is.null(tt$estimate) && length(tt$estimate) == 2L) unname(tt$estimate[1L] - tt$estimate[2L]) else unname(tt$estimate[1L]) - mu,
    ic_inf       = tt$conf.int[1L],
    ic_sup       = tt$conf.int[2L],
    hipotese_nula = mu,
    alternativa  = lado
  )
  out |> dplyr::mutate(dplyr::across(where(is.numeric),
                                     ~ arredonda(.x, digits)))
}

#' Teste Z para media (sigma conhecido)
#'
#' @param x Vetor numerico.
#' @param mu Media sob H0.
#' @param sigma Desvio-padrao populacional conhecido.
#' @param lado String: bilateral, direita ou esquerda.
#' @param conf Nivel de confianca.
#' @param digits Inteiro.
#' @return tibble.
#' @examples
#' rnp_teste_z_media(rnorm(100, mean = 10, sd = 2), mu = 10, sigma = 2)
#' @export
rnp_teste_z_media <- function(x, mu = 0, sigma,
                              lado = c("bilateral", "direita", "esquerda"),
                              conf = 0.95, digits = 4L) {
  abort_numerico(x, "x")
  if (!is.numeric(sigma) || length(sigma) != 1L || sigma <= 0) {
    rlang::abort("{.arg sigma} deve ser escalar positivo.")
  }
  lado <- check_lado(lado)
  abort_confianca(conf)
  n <- length(x)
  if (n < 1L) rlang::abort("{.arg x} deve ter pelo menos 1 observacao.")
  mx <- mean(x)
  se <- sigma / sqrt(n)
  z <- (mx - mu) / se
  p <- switch(lado,
    bilateral = 2 * stats::pnorm(-abs(z)),
    direita   = stats::pnorm(z, lower.tail = FALSE),
    esquerda  = stats::pnorm(z)
  )
  zcrit <- stats::qnorm((1 + conf) / 2)
  tibble::tibble(
    estatistica  = z,
    p_valor      = p,
    media_x      = mx,
    erro_padrao  = se,
    ic_inf       = mx - zcrit * se,
    ic_sup       = mx + zcrit * se,
    hipotese_nula = mu,
    alternativa  = lado
  ) |> dplyr::mutate(dplyr::across(where(is.numeric),
                                   ~ arredonda(.x, digits)))
}

#' Teste Z para proporcao (uma amostra)
#'
#' @param sucesso Numerico. Numero de sucessos.
#' @param n Inteiro. Tamanho da amostra.
#' @param p0 Proporcao sob H0 (default 0.5).
#' @param lado String.
#' @param conf Nivel de confianca.
#' @param digits Inteiro.
#' @return tibble.
#' @examples
#' rnp_teste_z_proporcao(55, 100, p0 = 0.5)
#' @export
rnp_teste_z_proporcao <- function(sucesso, n, p0 = 0.5,
                                  lado = c("bilateral", "direita", "esquerda"),
                                  conf = 0.95, digits = 4L) {
  abort_inteiro_pos(n, "n")
  if (!is.numeric(sucesso) || sucesso < 0 || sucesso > n || is.na(sucesso)) {
    rlang::abort("{.arg sucesso} deve estar em [0, n].")
  }
  abort_proporcao(p0, "p0")
  lado <- check_lado(lado)
  abort_confianca(conf)
  p_hat <- sucesso / n
  se <- sqrt(p0 * (1 - p0) / n)
  if (se == 0) rlang::abort("Erro-padrao = 0 (p0 = 0 ou 1).")
  z <- (p_hat - p0) / se
  p <- switch(lado,
    bilateral = 2 * stats::pnorm(-abs(z)),
    direita   = stats::pnorm(z, lower.tail = FALSE),
    esquerda  = stats::pnorm(z)
  )
  zcrit <- stats::qnorm((1 + conf) / 2)
  se_hat <- sqrt(p_hat * (1 - p_hat) / n)
  tibble::tibble(
    estatistica   = z,
    p_valor       = p,
    proporcao     = p_hat,
    p0            = p0,
    erro_padrao   = se,
    ic_inf        = pmax(p_hat - zcrit * se_hat, 0),
    ic_sup        = pmin(p_hat + zcrit * se_hat, 1),
    n             = n,
    alternativa   = lado
  ) |> dplyr::mutate(dplyr::across(where(is.numeric) & !n,
                                   ~ arredonda(.x, digits)))
}

#' Teste de variancias (F, Bartlett, Levene, Fligner-Killeen)
#'
#' @param x Vetor numerico ou formula.
#' @param g Vetor de grupos (fator/character). Necessario se x nao for formula.
#' @param method String: \code{"bartlett"}, \code{"levene"}, \code{"fligner"},
#'   \code{"f"} (apenas 2 grupos).
#' @param digits Inteiro.
#' @return tibble com \code{estatistica}, \code{gl1}, \code{gl2}, \code{p_valor},
#'   \code{metodo}.
#' @examples
#' rnp_teste_variancias(mtcars$mpg, as.factor(mtcars$cyl), method = "bartlett")
#' @export
rnp_teste_variancias <- function(x, g = NULL,
                                 method = c("bartlett", "levene", "fligner", "f"),
                                 digits = 4L) {
  method <- rlang::arg_match(method)
  if (inherits(x, "formula")) {
    mt <- model.frame(x, na.action = na.omit)
    if (ncol(mt) != 2L) rlang::abort("Formula deve ser y ~ grupo.")
    y <- mt[[1L]]
    g <- mt[[2L]]
  } else {
    abort_numerico(x, "x")
    if (is.null(g)) rlang::abort("{.arg g} (grupo) e necessario.")
  }
  g <- as.factor(g)
  if (length(x) != length(g)) rlang::abort("x e g devem ter o mesmo comprimento.")
  n_groups <- nlevels(g)
  if (n_groups < 2L) rlang::abort("Necessario >= 2 grupos.")
  res <- switch(method,
    bartlett = {
      bt <- stats::bartlett.test(x, g)
      list(estatistica = unname(bt$statistic), gl1 = unname(bt$parameter),
           gl2 = NA_integer_, p_valor = bt$p.value)
    },
    fligner = {
      ft <- stats::fligner.test(x, g)
      list(estatistica = unname(ft$statistic), gl1 = unname(ft$parameter),
           gl2 = NA_integer_, p_valor = ft$p.value)
    },
    levene = .levene_interna(x, g, center = "median"),
    f = {
      if (n_groups != 2L) rlang::abort("Metodo F requer exatamente 2 grupos.")
      d <- split(x, g)
      vx <- stats::var(d[[1L]]); vy <- stats::var(d[[2L]])
      Fstat <- vx / vy
      df1 <- length(d[[1L]]) - 1
      df2 <- length(d[[2L]]) - 1
      list(estatistica = Fstat, gl1 = df1, gl2 = df2,
           p_valor = 2 * stats::pf(Fstat, df1, df2, lower.tail = FALSE))
    }
  )
  tibble::tibble(
    estatistica = res$estatistica,
    gl1         = res$gl1,
    gl2         = res$gl2,
    p_valor     = res$p_valor,
    metodo      = method
  ) |> dplyr::mutate(dplyr::across(where(is.numeric) & !gl1 & !gl2,
                                   ~ arredonda(.x, digits)))
}

# Internal: Levene test
.levene_interna <- function(x, g, center = c("median", "mean")) {
  center <- rlang::arg_match(center)
  grp <- split(x, g)
  n_i <- lengths(grp)
  N <- sum(n_i)
  k <- length(grp)
  z <- unsplit(lapply(grp, function(v) abs(v - (if (center == "median") stats::median(v) else mean(v)))), g)
  z_dot <- mean(z)
  z_dot_i <- vapply(grp, function(v) mean(abs(v - (if (center == "median") stats::median(v) else mean(v)))), numeric(1))
  SS_between <- sum(n_i * (z_dot_i - z_dot)^2)
  SS_within <- sum(vapply(seq_along(grp), function(i) sum((abs(grp[[i]] - (if (center == "median") stats::median(grp[[i]]) else mean(grp[[i]]))) - z_dot_i[i])^2), numeric(1)))
  Fstat <- (SS_between / (k - 1)) / (SS_within / (N - k))
  list(estatistica = Fstat, gl1 = k - 1, gl2 = N - k,
       p_valor = stats::pf(Fstat, k - 1, N - k, lower.tail = FALSE))
}

#' Teste de Levene (alias direto)
#'
#' Atalho para [rnp_teste_variancias()] com method = "levene".
#'
#' @inheritParams rnp_teste_variancias
#' @return tibble.
#' @examples
#' rnp_teste_levene(mtcars$mpg, as.factor(mtcars$cyl))
#' @export
rnp_teste_levene <- function(x, g = NULL, digits = 4L) {
  rnp_teste_variancias(x = x, g = g, method = "levene", digits = digits)
}

#' Teste de Shapiro-Wilk (normalidade)
#'
#' @param x Vetor numerico.
#' @param digits Inteiro.
#' @return tibble com \code{estatistica}, \code{gl}, \code{p_valor}, \code{metodo}.
#' @examples
#' rnp_teste_shapiro(rnorm(50))
#' @export
rnp_teste_shapiro <- function(x, digits = 4L) {
  abort_numerico(x, "x")
  if (length(x) < 3L || length(x) > 5000L) {
    rlang::abort("Shapiro-Wilk requer 3 <= n <= 5000.")
  }
  sw <- stats::shapiro.test(x)
  tibble::tibble(
    estatistica = unname(sw$statistic),
    gl          = length(x),
    p_valor     = sw$p.value,
    metodo      = "shapiro-wilk"
  ) |> dplyr::mutate(dplyr::across(where(is.numeric) & !gl,
                                   ~ arredonda(.x, digits)))
}
