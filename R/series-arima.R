# Series temporais: ARIMA/SARIMA e testes de raiz unitaria (FATIA 9).
# Apoia-se em stats::arima (base R). ADF/KPSS/VAR/GARCH com implementacao propria.

# Monta a tibble de coeficientes de um ajuste arima.
.arima_coefs <- function(fit, digits) {
  est <- fit$coef
  se <- sqrt(diag(fit$var.coef))
  se <- se[names(est)]
  z <- est / se
  tibble::tibble(
    termo       = names(est),
    estimativa  = arredonda(unname(est), digits),
    erro_padrao = arredonda(unname(se), digits),
    z           = arredonda(unname(z), digits),
    p_valor     = arredonda(unname(2 * stats::pnorm(-abs(z))), digits)
  )
}

.arima_resumo <- function(fit, x, digits) {
  n <- length(x)
  k <- length(fit$coef)
  tibble::tibble(
    log_veross = arredonda(fit$loglik, digits),
    aic        = arredonda(fit$aic, digits),
    bic        = arredonda(fit$aic - 2 * k + k * log(n), digits),
    aicc       = arredonda(fit$aic + 2 * k * (k + 1) / (n - k - 1), digits),
    sigma2     = arredonda(fit$sigma2, digits)
  )
}

#' Ajuste de modelo ARIMA
#'
#' Ajusta um modelo ARIMA(p, d, q) por maxima verossimilhanca
#' ([stats::arima()]), com saida tidy.
#'
#' @param x Vetor numerico ou `ts`.
#' @param ordem Vetor `c(p, d, q)`.
#' @param incluir_media Logico. Inclui media/intercepto (apenas se `d = 0`).
#' @param digits Inteiro.
#'
#' @return Uma lista com `coeficientes`, `modelo` (AIC/BIC/AICc/sigma2),
#'   `objeto` (`Arima`) e `serie` (a serie original).
#'
#' @examples
#' rnp_arima(lh, ordem = c(1, 0, 0))$coeficientes
#' @family series
#' @export
rnp_arima <- function(x, ordem = c(0, 0, 0), incluir_media = TRUE, digits = 4L) {
  if (length(ordem) != 3L) rlang::abort("{.arg ordem} deve ser c(p, d, q).")
  fit <- stats::arima(x, order = ordem,
                      include.mean = incluir_media && ordem[2L] == 0)
  .rnp_lista(list(coeficientes = .arima_coefs(fit, digits),
                  modelo = .arima_resumo(fit, x, digits),
                  objeto = fit, serie = x), "Modelo ARIMA")
}

#' Ajuste de modelo SARIMA (ARIMA sazonal)
#'
#' @param x Vetor numerico ou `ts`.
#' @param ordem Vetor `c(p, d, q)` da parte nao-sazonal.
#' @param sazonal Vetor `c(P, D, Q)` da parte sazonal.
#' @param periodo Periodo sazonal (ex.: 12 para mensal).
#' @param digits Inteiro.
#'
#' @return Mesma estrutura de [rnp_arima()].
#'
#' @examples
#' rnp_sarima(AirPassengers, c(0, 1, 1), c(0, 1, 1), 12)$modelo
#' @family series
#' @export
rnp_sarima <- function(x, ordem = c(0, 0, 0), sazonal = c(0, 0, 0),
                       periodo = 12, digits = 4L) {
  fit <- stats::arima(x, order = ordem,
                      seasonal = list(order = sazonal, period = periodo))
  .rnp_lista(list(coeficientes = .arima_coefs(fit, digits),
                  modelo = .arima_resumo(fit, x, digits),
                  objeto = fit, serie = x), "Modelo SARIMA")
}

#' Selecao automatica de ordem ARIMA
#'
#' Busca em grade a melhor ordem ARIMA(p, d, q) (opcionalmente sazonal) pelo
#' criterio escolhido. Substitui `forecast::auto.arima` com implementacao
#' propria sobre [stats::arima()].
#'
#' @param x Vetor numerico ou `ts`.
#' @param max_p,max_d,max_q Ordens maximas nao-sazonais.
#' @param periodo Periodo sazonal (1 = sem sazonalidade).
#' @param ic String: `"aicc"`, `"aic"` ou `"bic"`.
#' @param digits Inteiro.
#'
#' @return Uma lista como [rnp_arima()], com `selecao` (tibble dos melhores
#'   candidatos) adicional.
#'
#' @examples
#' rnp_auto_arima(lh, max_p = 2, max_q = 2)$modelo
#' @family series
#' @export
rnp_auto_arima <- function(x, max_p = 3, max_d = 2, max_q = 3, periodo = 1,
                           ic = c("aicc", "aic", "bic"), digits = 4L) {
  ic <- rlang::arg_match(ic)
  n <- length(x)
  grade <- expand.grid(p = 0:max_p, d = 0:max_d, q = 0:max_q)
  aval <- function(p, d, q) {
    fit <- tryCatch(
      stats::arima(x, order = c(p, d, q),
                   seasonal = list(order = c(0, 0, 0), period = max(periodo, 1))),
      error = function(e) NULL)
    if (is.null(fit)) return(c(crit = Inf))
    k <- length(fit$coef)
    crit <- switch(ic,
      aic  = fit$aic,
      bic  = fit$aic - 2 * k + k * log(n),
      aicc = fit$aic + 2 * k * (k + 1) / (n - k - 1))
    c(crit = crit)
  }
  crits <- mapply(aval, grade$p, grade$d, grade$q)
  grade$criterio <- arredonda(as.numeric(crits), digits)
  grade <- grade[order(grade$criterio), ]
  melhor <- grade[1L, ]
  out <- rnp_arima(x, ordem = c(melhor$p, melhor$d, melhor$q), digits = digits)
  out$selecao <- tibble::as_tibble(utils::head(grade, 5))
  names(out$selecao)[4L] <- paste0("criterio_", ic)
  out
}

#' Previsao a partir de modelo ARIMA/SARIMA
#'
#' @param modelo Saida de [rnp_arima()]/[rnp_sarima()] (ou objeto `Arima`).
#' @param h Inteiro. Horizonte de previsao.
#' @param conf Nivel de confianca dos intervalos.
#' @param digits Inteiro.
#'
#' @return Uma lista com `previsao` (tibble) e `grafico` (`ggplot`).
#'
#' @examples
#' m <- rnp_arima(lh, c(1, 0, 0))
#' rnp_ts_previsao(m, h = 6)$previsao
#' @family series
#' @export
rnp_ts_previsao <- function(modelo, h = 10, conf = 0.95, digits = 4L) {
  fit <- if (inherits(modelo, "Arima")) modelo else modelo$objeto
  serie <- if (inherits(modelo, "Arima")) NULL else modelo$serie
  abort_inteiro_pos(h, "h")
  abort_confianca(conf)
  pr <- stats::predict(fit, n.ahead = h)
  z <- stats::qnorm((1 + conf) / 2)
  prev <- tibble::tibble(
    passo  = seq_len(h),
    previsao = arredonda(as.numeric(pr$pred), digits),
    ic_inf = arredonda(as.numeric(pr$pred - z * pr$se), digits),
    ic_sup = arredonda(as.numeric(pr$pred + z * pr$se), digits))
  g <- NULL
  if (!is.null(serie)) {
    hist <- tibble::tibble(t = seq_along(serie), valor = as.numeric(serie),
                           tipo = "observado")
    fut <- tibble::tibble(t = length(serie) + prev$passo, valor = prev$previsao,
                          tipo = "previsto", ic_inf = prev$ic_inf, ic_sup = prev$ic_sup)
    g <- ggplot2::ggplot() +
      ggplot2::geom_ribbon(data = fut, ggplot2::aes(.data$t, ymin = .data$ic_inf,
                                                    ymax = .data$ic_sup),
                           fill = "grey70", alpha = 0.4) +
      ggplot2::geom_line(data = hist, ggplot2::aes(.data$t, .data$valor),
                         color = rnp_paleta_rnp("rnp_qual", 1)) +
      ggplot2::geom_line(data = fut, ggplot2::aes(.data$t, .data$valor),
                         color = "red") +
      rnp_tema_rnp() +
      ggplot2::labs(title = "Previsao", x = "Tempo", y = "Valor")
  }
  .rnp_lista(list(previsao = prev, grafico = g), "Previsao de serie temporal")
}

#' Teste de Dickey-Fuller aumentado (ADF)
#'
#' Testa a presenca de raiz unitaria (H0: serie nao-estacionaria). Ajusta a
#' regressao da serie diferenciada sobre o nivel defasado e sobre defasagens da
#' propria diferenca (forma "aumentada"), e compara a estatistica do termo de
#' nivel com valores criticos de Dickey-Fuller.
#'
#' @param x Vetor numerico.
#' @param lag Numero de defasagens aumentadas. Default automatico.
#' @param tipo String: `"constante"` (default) ou `"tendencia"`.
#' @param digits Inteiro.
#'
#' @return tibble com `estatistica`, `lag`, `valor_critico_5`, `p_valor_aprox`,
#'   `estacionaria`.
#'
#' @examples
#' rnp_ts_adf(as.numeric(lh))
#' @family series
#' @export
rnp_ts_adf <- function(x, lag = NULL, tipo = c("constante", "tendencia"),
                       digits = 4L) {
  abort_numerico(x, "x")
  tipo <- rlang::arg_match(tipo)
  x <- as.numeric(x)
  n <- length(x)
  if (is.null(lag)) lag <- trunc((n - 1)^(1 / 3))
  dy <- diff(x)
  m <- length(dy)
  y_lag <- x[lag:(m)]                          # y_{t-1}
  dy_t <- dy[(lag + 1):m]
  X <- cbind(1, y_lag[seq_along(dy_t)])
  if (tipo == "tendencia") X <- cbind(X, seq_along(dy_t))
  if (lag > 0) {
    for (i in seq_len(lag)) {
      X <- cbind(X, dy[(lag + 1 - i):(m - i)])
    }
  }
  fit <- stats::lm.fit(X, dy_t)
  rss <- sum(fit$residuals^2)
  gl <- length(dy_t) - ncol(X)
  sigma2 <- rss / gl
  XtX_inv <- solve(crossprod(X))
  se <- sqrt(sigma2 * diag(XtX_inv))
  estat <- fit$coefficients[2L] / se[2L]       # coef de y_{t-1}
  cv <- if (tipo == "tendencia") c(-3.96, -3.41, -3.12) else c(-3.43, -2.86, -2.57)
  p_aprox <- stats::approx(cv, c(0.01, 0.05, 0.10), xout = estat, rule = 2)$y
  tibble::tibble(
    estatistica     = arredonda(unname(estat), digits),
    lag             = lag,
    valor_critico_5 = cv[2L],
    p_valor_aprox   = arredonda(p_aprox, digits),
    estacionaria    = estat < cv[2L]
  )
}

#' Teste KPSS de estacionariedade
#'
#' Testa H0 de **estacionariedade** (oposto do ADF) em torno de um nivel
#' (ou tendencia), usando a soma acumulada dos residuos e variancia de longo
#' prazo de Newey-West.
#'
#' @param x Vetor numerico.
#' @param tipo String: `"nivel"` (default) ou `"tendencia"`.
#' @param digits Inteiro.
#'
#' @return tibble com `estatistica`, `valor_critico_5`, `estacionaria`.
#'
#' @examples
#' rnp_ts_kpss(as.numeric(lh))
#' @family series
#' @export
rnp_ts_kpss <- function(x, tipo = c("nivel", "tendencia"), digits = 4L) {
  abort_numerico(x, "x")
  tipo <- rlang::arg_match(tipo)
  x <- as.numeric(x)
  n <- length(x)
  if (tipo == "tendencia") {
    res <- stats::lm.fit(cbind(1, seq_len(n)), x)$residuals
    cv <- c(0.119, 0.146, 0.216)
  } else {
    res <- x - mean(x)
    cv <- c(0.347, 0.463, 0.739)
  }
  S <- cumsum(res)
  eta <- sum(S^2) / n^2
  l <- trunc(4 * (n / 100)^0.25)
  s2 <- sum(res^2) / n
  if (l > 0) {
    for (j in seq_len(l)) {
      w <- 1 - j / (l + 1)
      s2 <- s2 + 2 * w * sum(res[(j + 1):n] * res[1:(n - j)]) / n
    }
  }
  estat <- eta / s2
  tibble::tibble(
    estatistica     = arredonda(estat, digits),
    valor_critico_5 = cv[2L],
    estacionaria    = estat < cv[2L]   # H0 estacionariedade: nao rejeita = estacionaria
  )
}

#' Funcao de correlacao cruzada (CCF)
#'
#' @param x,y Vetores numericos.
#' @param lag_max Numero maximo de defasagens.
#' @param digits Inteiro.
#'
#' @return Uma lista com `tabela` (tibble) e `grafico` (`ggplot`).
#'
#' @examples
#' rnp_ts_ccf(as.numeric(mdeaths), as.numeric(fdeaths))$tabela
#' @family series
#' @export
rnp_ts_ccf <- function(x, y, lag_max = NULL, digits = 4L) {
  abort_numerico(x, "x"); abort_numerico(y, "y")
  cc <- stats::ccf(as.numeric(x), as.numeric(y), lag.max = lag_max, plot = FALSE)
  d <- tibble::tibble(lag = as.numeric(cc$lag), ccf = arredonda(as.numeric(cc$acf), digits))
  ci <- 1.96 / sqrt(cc$n.used)
  g <- ggplot2::ggplot(d, ggplot2::aes(.data$lag, .data$ccf)) +
    ggplot2::geom_hline(yintercept = 0, color = "grey50") +
    ggplot2::geom_hline(yintercept = c(-ci, ci), linetype = "dotted", color = "blue") +
    ggplot2::geom_segment(ggplot2::aes(xend = .data$lag, yend = 0),
                          color = rnp_paleta_rnp("rnp_qual", 1)) +
    rnp_tema_rnp() +
    ggplot2::labs(title = "Correlacao cruzada", x = "Lag", y = "CCF")
  .rnp_lista(list(tabela = d, grafico = g), "Correlacao cruzada (CCF)")
}

#' Autorregressao vetorial (VAR)
#'
#' Ajusta um VAR(p) por minimos quadrados equacao-a-equacao e testa causalidade
#' de Granger entre as series.
#'
#' @param dados data.frame/matriz com as series em colunas.
#' @param p Ordem do VAR.
#' @param digits Inteiro.
#'
#' @return Uma lista com `coeficientes` (por equacao) e `granger` (tibble de
#'   testes de causalidade).
#'
#' @examples
#' df <- data.frame(m = as.numeric(mdeaths), f = as.numeric(fdeaths))
#' rnp_ts_var(df, p = 2)$granger
#' @family series
#' @export
rnp_ts_var <- function(dados, p = 1, digits = 4L) {
  dados <- as.matrix(dados)
  k <- ncol(dados); n <- nrow(dados)
  nomes <- colnames(dados) %||% paste0("y", seq_len(k))
  # matriz de defasagens
  Z <- matrix(1, n - p, 1)
  for (i in seq_len(p)) Z <- cbind(Z, dados[(p - i + 1):(n - i), , drop = FALSE])
  Y <- dados[(p + 1):n, , drop = FALSE]
  B <- solve(crossprod(Z), crossprod(Z, Y))   # (1 + k*p) x k
  res <- Y - Z %*% B
  coefs <- purrr::map_dfr(seq_len(k), function(j) {
    tibble::tibble(equacao = nomes[j],
                   termo = c("const", paste0(rep(nomes, p), "_l",
                                             rep(seq_len(p), each = k))),
                   estimativa = arredonda(B[, j], digits))
  })
  # Granger: series i -> j (F comparando RSS restrito x irrestrito)
  granger <- purrr::map_dfr(seq_len(k), function(j) {
    purrr::map_dfr(setdiff(seq_len(k), j), function(i) {
      cols_i <- which((rep(seq_len(k), p)) == i) + 1L   # colunas de Z da serie i
      Z_r <- Z[, -cols_i, drop = FALSE]
      res_r <- Y[, j] - Z_r %*% solve(crossprod(Z_r), crossprod(Z_r, Y[, j]))
      rss_r <- sum(res_r^2); rss_u <- sum(res[, j]^2)
      gl1 <- p; gl2 <- (n - p) - ncol(Z)
      Fstat <- ((rss_r - rss_u) / gl1) / (rss_u / gl2)
      tibble::tibble(causa = nomes[i], efeito = nomes[j],
                     estatistica_f = arredonda(Fstat, digits),
                     p_valor = arredonda(stats::pf(Fstat, gl1, gl2, lower.tail = FALSE), digits))
    })
  })
  .rnp_lista(list(coeficientes = coefs, granger = granger),
             "Autorregressao vetorial (VAR)")
}

#' Modelo de volatilidade GARCH(1,1)
#'
#' Ajusta um GARCH(1,1) por maxima verossimilhanca (inovacoes normais), em que a
#' variancia condicional depende do quadrado do erro anterior e da variancia
#' condicional anterior, com parametros omega, alpha e beta.
#'
#' @param x Vetor numerico (ex.: retornos).
#' @param digits Inteiro.
#'
#' @return Uma lista com `parametros` (tibble: mu, omega, alpha, beta),
#'   `persistencia` (alpha + beta) e `volatilidade` (vetor sigma_t).
#'
#' @examples
#' set.seed(1); r <- rnorm(500) * (1 + 0.3 * abs(rnorm(500)))
#' rnp_ts_garch(r)$parametros
#' @family series
#' @export
rnp_ts_garch <- function(x, digits = 4L) {
  abort_numerico(x, "x")
  x <- as.numeric(x)
  n <- length(x)
  nll <- function(par) {
    mu <- par[1L]; omega <- par[2L]; alpha <- par[3L]; beta <- par[4L]
    if (omega <= 0 || alpha < 0 || beta < 0 || alpha + beta >= 1) return(1e10)
    e <- x - mu
    s2 <- numeric(n); s2[1L] <- stats::var(x)
    for (t in 2:n) s2[t] <- omega + alpha * e[t - 1L]^2 + beta * s2[t - 1L]
    0.5 * sum(log(2 * pi * s2) + e^2 / s2)
  }
  ini <- c(mean(x), 0.1 * stats::var(x), 0.1, 0.8)
  op <- stats::optim(ini, nll, method = "Nelder-Mead")
  par <- op$par
  e <- x - par[1L]
  s2 <- numeric(n); s2[1L] <- stats::var(x)
  for (t in 2:n) s2[t] <- par[2L] + par[3L] * e[t - 1L]^2 + par[4L] * s2[t - 1L]
  .rnp_lista(list(
    parametros = tibble::tibble(
      parametro = c("mu", "omega", "alpha", "beta"),
      estimativa = arredonda(par, digits)),
    persistencia = arredonda(par[3L] + par[4L], digits),
    volatilidade = sqrt(s2)), "Modelo de volatilidade GARCH(1,1)")
}

#' Diagnostico de residuos de modelo de series
#'
#' Ljung-Box e normalidade dos residuos de um ajuste ARIMA/SARIMA.
#'
#' @param modelo Saida de [rnp_arima()]/[rnp_sarima()] (ou objeto `Arima`).
#' @param lag Defasagens para o Ljung-Box.
#' @param digits Inteiro.
#'
#' @return tibble com os testes de diagnostico.
#'
#' @examples
#' rnp_ts_residuos(rnp_arima(lh, c(1, 0, 0)))
#' @family series
#' @export
rnp_ts_residuos <- function(modelo, lag = 10, digits = 4L) {
  fit <- if (inherits(modelo, "Arima")) modelo else modelo$objeto
  res <- as.numeric(stats::residuals(fit))
  k <- length(fit$coef)
  lb <- stats::Box.test(res, lag = lag, type = "Ljung-Box", fitdf = min(k, lag - 1))
  sw <- stats::shapiro.test(res)
  tibble::tibble(
    teste = c("ljung-box", "shapiro-wilk"),
    estatistica = arredonda(c(unname(lb$statistic), unname(sw$statistic)), digits),
    p_valor = arredonda(c(lb$p.value, sw$p.value), digits),
    interpretacao = c(
      if (lb$p.value < 0.05) "residuos correlacionados" else "ruido branco (ok)",
      if (sw$p.value < 0.05) "nao-normal" else "normalidade ok"))
}
