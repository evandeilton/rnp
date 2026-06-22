# Verossimilhanca e testes assintoticos (FATIA 2): EMV, metodo dos momentos,
# informacao de Fisher, IC por verossimilhanca perfilada, testes da razao de
# verossimilhancas / Wald / escore, e estimacao bayesiana conjugada.

# Hessiana numerica por diferencas finitas centradas.
.hessiana_num <- function(f, x, h = 1e-4) {
  k <- length(x)
  H <- matrix(0, k, k)
  for (i in seq_len(k)) {
    for (j in seq_len(k)) {
      xi <- x; xj <- x; xij <- x; xji <- x
      xi[i]  <- xi[i]  + h; xi[j]  <- xi[j]  + h
      xj[i]  <- xj[i]  + h; xj[j]  <- xj[j]  - h
      xij[i] <- xij[i] - h; xij[j] <- xij[j] + h
      xji[i] <- xji[i] - h; xji[j] <- xji[j] - h
      H[i, j] <- (f(xi) - f(xj) - f(xij) + f(xji)) / (4 * h^2)
    }
  }
  (H + t(H)) / 2
}

# Gradiente numerico (diferencas centradas).
.gradiente_num <- function(f, x, h = 1e-5) {
  vapply(seq_along(x), function(i) {
    xp <- x; xm <- x
    xp[i] <- xp[i] + h; xm[i] <- xm[i] - h
    (f(xp) - f(xm)) / (2 * h)
  }, numeric(1))
}

#' Estimador de maxima verossimilhanca (generico)
#'
#' Maximiza uma funcao de log-verossimilhanca via [stats::optim()] e estima
#' erros-padrao pela inversa da informacao de Fisher observada (Hessiana).
#'
#' @param log_veross Funcao `function(theta) ...` que devolve a
#'   log-verossimilhanca (escalar) para o vetor de parametros `theta`.
#' @param inicio Vetor de valores iniciais para `theta`.
#' @param nomes Vetor opcional de nomes dos parametros.
#' @param metodo Metodo de [stats::optim()].
#' @param digits Inteiro.
#'
#' @return Uma lista com `estimativas` (tibble: `parametro`, `estimativa`,
#'   `erro_padrao`, `z`, `ic_inf`, `ic_sup`) e `ajuste` (tibble:
#'   `log_veross`, `aic`, `bic`... `convergiu`).
#'
#' @examples
#' set.seed(1); x <- rnorm(200, 5, 2)
#' ll <- function(th) sum(dnorm(x, th[1], th[2], log = TRUE))
#' rnp_emv(ll, inicio = c(0, 1), nomes = c("media", "dp"))$estimativas
#' @family inferencia
#' @export
rnp_emv <- function(log_veross, inicio, nomes = NULL, metodo = "Nelder-Mead",
                    digits = 4L) {
  if (!is.function(log_veross)) rlang::abort("{.arg log_veross} deve ser funcao.")
  # penaliza regioes inviaveis (log-veross nao-finita) para estabilizar o optim
  nll <- function(th) {
    v <- -log_veross(th)
    if (!is.finite(v)) 1e10 else v
  }
  op <- stats::optim(inicio, nll, method = metodo, hessian = TRUE)
  est <- op$par
  if (is.null(nomes)) nomes <- names(inicio) %||% paste0("theta", seq_along(est))
  H <- op$hessian
  vcov <- tryCatch(solve(H), error = function(e) matrix(NA_real_, length(est), length(est)))
  vdiag <- diag(vcov); vdiag[vdiag < 0] <- NA_real_
  se <- sqrt(vdiag)
  z <- est / se
  ll_max <- -op$value
  k <- length(est)
  tibble_est <- tibble::tibble(
    parametro   = nomes,
    estimativa  = arredonda(est, digits),
    erro_padrao = arredonda(se, digits),
    z           = arredonda(z, digits),
    ic_inf      = arredonda(est - 1.96 * se, digits),
    ic_sup      = arredonda(est + 1.96 * se, digits)
  )
  .rnp_lista(list(
    estimativas = tibble_est,
    ajuste = tibble::tibble(
      log_veross = arredonda(ll_max, digits),
      aic        = arredonda(-2 * ll_max + 2 * k, digits),
      bic        = NA_real_,
      n_par      = k,
      convergiu  = op$convergence == 0
    )
  ), "Estimacao por maxima verossimilhanca")
}

#' Estimacao por metodo dos momentos
#'
#' Estima parametros de distribuicoes comuns igualando momentos amostrais aos
#' teoricos.
#'
#' @param x Vetor numerico.
#' @param dist String: `"norm"`, `"exp"`, `"pois"`, `"gamma"`, `"beta"`,
#'   `"unif"`.
#' @param digits Inteiro.
#'
#' @return tibble com `parametro` e `estimativa`.
#'
#' @examples
#' set.seed(1)
#' rnp_metodo_momentos(rgamma(500, 2, 1), "gamma")
#' @family inferencia
#' @export
rnp_metodo_momentos <- function(x, dist = c("norm", "exp", "pois", "gamma",
                                            "beta", "unif"), digits = 4L) {
  abort_numerico(x, "x")
  dist <- rlang::arg_match(dist)
  x <- sem_na(x)
  m <- mean(x); v <- stats::var(x)
  par <- switch(dist,
    norm = c(media = m, dp = sqrt(v)),
    exp  = c(rate = 1 / m),
    pois = c(lambda = m),
    gamma = c(shape = m^2 / v, rate = m / v),
    beta = {
      cte <- m * (1 - m) / v - 1
      c(shape1 = m * cte, shape2 = (1 - m) * cte)
    },
    unif = c(min = m - sqrt(3 * v), max = m + sqrt(3 * v))
  )
  tibble::tibble(parametro = names(par),
                 estimativa = arredonda(unname(par), digits))
}

#' Informacao de Fisher observada
#'
#' Calcula a matriz de informacao de Fisher observada (Hessiana negativa da
#' log-verossimilhanca) em um ponto, e os erros-padrao assintoticos.
#'
#' @param log_veross Funcao `function(theta) ...` (log-verossimilhanca).
#' @param theta Ponto de avaliacao (tipicamente a EMV).
#' @param digits Inteiro.
#'
#' @return Uma lista com `informacao` (matriz) e `erros_padrao` (vetor).
#'
#' @examples
#' set.seed(1); x <- rnorm(100, 2, 1)
#' ll <- function(th) sum(dnorm(x, th[1], th[2], log = TRUE))
#' rnp_informacao_fisher(ll, c(2, 1))$erros_padrao
#' @family inferencia
#' @export
rnp_informacao_fisher <- function(log_veross, theta, digits = 4L) {
  if (!is.function(log_veross)) rlang::abort("{.arg log_veross} deve ser funcao.")
  H <- .hessiana_num(log_veross, theta)
  info <- -H
  vcov <- tryCatch(solve(info), error = function(e) matrix(NA_real_, length(theta), length(theta)))
  vdiag <- diag(vcov); vdiag[vdiag < 0] <- NA_real_
  .rnp_lista(list(
    informacao   = round(info, digits),
    erros_padrao = arredonda(sqrt(vdiag), digits)
  ), "Informacao de Fisher observada")
}

#' Grafico da funcao de log-verossimilhanca (1 parametro)
#'
#' Plota a log-verossimilhanca em funcao de um parametro escalar num intervalo,
#' marcando a estimativa de maxima verossimilhanca.
#'
#' @param log_veross Funcao escalar `function(theta) ...`.
#' @param intervalo Vetor `c(min, max)` do parametro.
#' @param n_pontos Inteiro. Resolucao da curva.
#' @param titulo Titulo opcional.
#'
#' @return Objeto `ggplot`.
#'
#' @examples
#' set.seed(1); x <- rpois(100, 3)
#' rnp_log_verossimilhanca(function(l) sum(dpois(x, l, log = TRUE)),
#'                         intervalo = c(1, 6))
#' @family inferencia
#' @export
rnp_log_verossimilhanca <- function(log_veross, intervalo, n_pontos = 200L,
                                    titulo = NULL) {
  if (!is.function(log_veross)) rlang::abort("{.arg log_veross} deve ser funcao.")
  if (length(intervalo) != 2L || intervalo[2L] <= intervalo[1L]) {
    rlang::abort("{.arg intervalo} deve ser c(min, max) com min < max.")
  }
  grade <- seq(intervalo[1L], intervalo[2L], length.out = n_pontos)
  ll <- vapply(grade, log_veross, numeric(1))
  emv <- grade[which.max(ll)]
  dados <- tibble::tibble(theta = grade, log_veross = ll)
  ggplot2::ggplot(dados, ggplot2::aes(x = .data$theta, y = .data$log_veross)) +
    ggplot2::geom_line(color = rnp_paleta_rnp("rnp_qual", 1), linewidth = 1) +
    ggplot2::geom_vline(xintercept = emv, linetype = "dashed", color = "red") +
    rnp_tema_rnp() +
    ggplot2::labs(title = titulo %||% "Funcao de log-verossimilhanca",
                  subtitle = glue::glue("EMV ~ {round(emv, 3)}"),
                  x = expression(theta), y = "log L")
}

#' IC por verossimilhanca perfilada (1 parametro)
#'
#' Intervalo de confianca para um parametro escalar baseado na verossimilhanca
#' perfilada: \{theta : 2 (l(hat) - l(theta)) <= qchisq(conf, 1)\}.
#'
#' @param log_veross Funcao escalar `function(theta) ...`.
#' @param emv Estimativa de maxima verossimilhanca.
#' @param intervalo Vetor `c(min, max)` de busca dos limites.
#' @param conf Nivel de confianca.
#' @param digits Inteiro.
#'
#' @return tibble com `emv`, `limite_inferior`, `limite_superior`, `conf`.
#'
#' @examples
#' set.seed(1); x <- rpois(100, 3)
#' ll <- function(l) sum(dpois(x, l, log = TRUE))
#' rnp_ic_verossimilhanca(ll, emv = mean(x), intervalo = c(0.5, 8))
#' @family inferencia
#' @export
rnp_ic_verossimilhanca <- function(log_veross, emv, intervalo, conf = 0.95,
                                   digits = 4L) {
  abort_confianca(conf)
  ll_max <- log_veross(emv)
  corte <- stats::qchisq(conf, df = 1) / 2
  g <- function(th) (ll_max - log_veross(th)) - corte
  li <- tryCatch(stats::uniroot(g, c(intervalo[1L], emv))$root,
                 error = function(e) NA_real_)
  ls <- tryCatch(stats::uniroot(g, c(emv, intervalo[2L]))$root,
                 error = function(e) NA_real_)
  tibble::tibble(
    emv             = arredonda(emv, digits),
    limite_inferior = arredonda(li, digits),
    limite_superior = arredonda(ls, digits),
    conf            = conf
  )
}

#' Teste da razao de verossimilhancas (modelos aninhados)
#'
#' Compara dois modelos aninhados (`lm`/`glm`) pela estatistica
#' 2 (logL_completo - logL_reduzido) ~ qui-quadrado.
#'
#' @param modelo_completo,modelo_reduzido Objetos `lm`/`glm` aninhados.
#' @param digits Inteiro.
#'
#' @return tibble com `estatistica`, `gl`, `p_valor`.
#'
#' @examples
#' m0 <- lm(mpg ~ wt, mtcars)
#' m1 <- lm(mpg ~ wt + hp, mtcars)
#' rnp_teste_razao_veross(m1, m0)
#' @family inferencia
#' @export
rnp_teste_razao_veross <- function(modelo_completo, modelo_reduzido, digits = 4L) {
  ll1 <- stats::logLik(modelo_completo)
  ll0 <- stats::logLik(modelo_reduzido)
  est <- as.numeric(2 * (ll1 - ll0))
  gl <- attr(ll1, "df") - attr(ll0, "df")
  if (gl <= 0) rlang::abort("O modelo completo deve ter mais parametros que o reduzido.")
  tibble::tibble(
    estatistica = arredonda(est, digits),
    gl          = gl,
    p_valor     = arredonda(stats::pchisq(est, gl, lower.tail = FALSE), digits)
  )
}

#' Teste de Wald para coeficientes de um modelo
#'
#' Para cada coeficiente de um `lm`/`glm`, calcula a estatistica de Wald
#' z = beta / ep e o p-valor bilateral.
#'
#' @param modelo Objeto `lm` ou `glm`.
#' @param digits Inteiro.
#'
#' @return tibble com `termo`, `estimativa`, `erro_padrao`, `z`, `p_valor`.
#'
#' @examples
#' rnp_teste_wald(glm(am ~ mpg, mtcars, family = binomial()))
#' @family inferencia
#' @export
rnp_teste_wald <- function(modelo, digits = 4L) {
  co <- summary(modelo)$coefficients
  est <- unname(co[, 1L])
  se <- unname(co[, 2L])
  z <- est / se
  tibble::tibble(
    termo       = rownames(co),
    estimativa  = arredonda(est, digits),
    erro_padrao = arredonda(se, digits),
    z           = arredonda(z, digits),
    p_valor     = arredonda(2 * stats::pnorm(-abs(z)), digits)
  )
}

#' Teste de escore (Rao) para um parametro escalar
#'
#' Testa H0: theta = theta0 usando a estatistica de escore
#' U(theta0)^2 / I(theta0) ~ qui-quadrado(1), com derivadas numericas.
#'
#' @param log_veross Funcao escalar `function(theta) ...`.
#' @param theta0 Valor sob a hipotese nula.
#' @param digits Inteiro.
#'
#' @return tibble com `estatistica`, `gl`, `p_valor`, `theta0`.
#'
#' @examples
#' set.seed(1); x <- rpois(100, 3)
#' ll <- function(l) sum(dpois(x, l, log = TRUE))
#' rnp_teste_score(ll, theta0 = 2.5)
#' @family inferencia
#' @export
rnp_teste_score <- function(log_veross, theta0, digits = 4L) {
  U <- .gradiente_num(log_veross, theta0)
  I <- -.hessiana_num(log_veross, theta0)
  est <- as.numeric(t(U) %*% solve(I) %*% U)
  tibble::tibble(
    estatistica = arredonda(est, digits),
    gl          = length(theta0),
    p_valor     = arredonda(stats::pchisq(est, length(theta0), lower.tail = FALSE), digits),
    theta0      = theta0[1L]
  )
}

#' Estimacao bayesiana conjugada
#'
#' Atualiza distribuicoes a priori conjugadas com os dados para tres modelos
#' classicos: Beta-Binomial, Gama-Poisson e Normal-Normal (variancia conhecida).
#'
#' @param familia String: `"beta_binomial"`, `"gama_poisson"` ou
#'   `"normal_normal"`.
#' @param priori Vetor nomeado de hiperparametros a priori
#'   (`beta_binomial`: `a`, `b`; `gama_poisson`: `a`, `b`;
#'   `normal_normal`: `mu0`, `sigma0`).
#' @param dados Lista com os dados
#'   (`beta_binomial`: `sucessos`, `n`; `gama_poisson`: `soma`, `n`;
#'   `normal_normal`: `x` e `sigma` conhecido).
#' @param conf Nivel de credibilidade do intervalo.
#' @param digits Inteiro.
#'
#' @return tibble com `parametro`, `valor` (hiperparametros a posteriori),
#'   alem de `media_post`, `ic_inf`, `ic_sup`.
#'
#' @examples
#' rnp_bayes_conjugada("beta_binomial", priori = c(a = 1, b = 1),
#'                     dados = list(sucessos = 8, n = 10))
#' @family inferencia
#' @export
rnp_bayes_conjugada <- function(familia = c("beta_binomial", "gama_poisson",
                                            "normal_normal"),
                                priori, dados, conf = 0.95, digits = 4L) {
  familia <- rlang::arg_match(familia)
  abort_confianca(conf)
  alpha <- 1 - conf
  res <- switch(familia,
    beta_binomial = {
      a <- priori[["a"]] + dados$sucessos
      b <- priori[["b"]] + dados$n - dados$sucessos
      list(par = c(a = a, b = b), media = a / (a + b),
           ic = stats::qbeta(c(alpha / 2, 1 - alpha / 2), a, b))
    },
    gama_poisson = {
      a <- priori[["a"]] + dados$soma
      b <- priori[["b"]] + dados$n
      list(par = c(forma = a, taxa = b), media = a / b,
           ic = stats::qgamma(c(alpha / 2, 1 - alpha / 2), a, rate = b))
    },
    normal_normal = {
      x <- dados$x; sig <- dados$sigma; n <- length(x)
      tau0 <- priori[["sigma0"]]^2; mu0 <- priori[["mu0"]]
      prec <- 1 / tau0 + n / sig^2
      mu_n <- (mu0 / tau0 + sum(x) / sig^2) / prec
      sd_n <- sqrt(1 / prec)
      list(par = c(mu = mu_n, sigma = sd_n), media = mu_n,
           ic = stats::qnorm(c(alpha / 2, 1 - alpha / 2), mu_n, sd_n))
    }
  )
  tibble::tibble(
    parametro  = names(res$par),
    valor      = arredonda(unname(res$par), digits),
    media_post = arredonda(res$media, digits),
    ic_inf     = arredonda(res$ic[1L], digits),
    ic_sup     = arredonda(res$ic[2L], digits)
  )
}
