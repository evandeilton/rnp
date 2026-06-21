#' Cadeia de Markov de tempo discreto
#'
#' A partir de uma matriz de transicao, calcula a distribuicao apos `n`
#' passos, a matriz de transicao em `n` passos (P^n) e a distribuicao
#' estacionaria. Usa backend C++ (RcppArmadillo).
#'
#' @param P Matriz de transicao (linhas somam 1).
#' @param estado_inicial Vetor de distribuicao inicial (soma 1) ou indice
#'   inteiro do estado de partida.
#' @param n Inteiro. Numero de passos.
#' @param digits Inteiro. Casas decimais.
#'
#' @return Uma lista com:
#'   * `distribuicao_n`: tibble com a distribuicao apos `n` passos.
#'   * `estacionaria`: tibble com a distribuicao estacionaria.
#'   * `matriz_n`: matriz P^n.
#'
#' @examples
#' P <- matrix(c(0.9, 0.1, 0.5, 0.5), 2, 2, byrow = TRUE)
#' rnp_cadeia_markov(P, estado_inicial = 1, n = 10)
#' @family probabilidade
#' @export
rnp_cadeia_markov <- function(P, estado_inicial = 1L, n = 10L, digits = 4L) {
  if (!is.matrix(P) || nrow(P) != ncol(P)) {
    rlang::abort("{.arg P} deve ser matriz quadrada de transicao.")
  }
  if (any(P < 0) || any(abs(rowSums(P) - 1) > 1e-6)) {
    rlang::abort("Cada linha de {.arg P} deve conter probabilidades que somam 1.")
  }
  abort_inteiro_pos(n, "n")
  d <- nrow(P)
  if (length(estado_inicial) == 1L) {
    abort_inteiro_pos(estado_inicial, "estado_inicial")
    if (estado_inicial > d) rlang::abort("{.arg estado_inicial} fora do numero de estados.")
    pi0 <- numeric(d); pi0[estado_inicial] <- 1
  } else {
    if (length(estado_inicial) != d || abs(sum(estado_inicial) - 1) > 1e-6) {
      rlang::abort("{.arg estado_inicial} deve ser distribuicao de tamanho {d} somando 1.")
    }
    pi0 <- as.numeric(estado_inicial)
  }
  Pn <- markov_npassos_cpp(P, as.integer(n))
  dist_n <- as.numeric(pi0 %*% Pn)
  est <- as.numeric(markov_estacionaria_cpp(P))
  estados <- rownames(P) %||% paste0("E", seq_len(d))
  list(
    distribuicao_n = tibble::tibble(estado = estados,
                                    probabilidade = arredonda(dist_n, digits)),
    estacionaria   = tibble::tibble(estado = estados,
                                    probabilidade = arredonda(est, digits)),
    matriz_n       = round(Pn, digits)
  )
}

#' Passeio aleatorio simples
#'
#' Simula um passeio aleatorio unidimensional com passos +1 (prob. `p`) e
#' -1 (prob. `1 - p`) e devolve o grafico da trajetoria.
#'
#' @param n_passos Inteiro. Numero de passos.
#' @param p Probabilidade de passo positivo.
#' @param seed Inteiro. Semente.
#'
#' @return Objeto `ggplot`.
#'
#' @examples
#' rnp_passeio_aleatorio(500, p = 0.5)
#' @family probabilidade
#' @export
rnp_passeio_aleatorio <- function(n_passos = 1000L, p = 0.5, seed = 42L) {
  abort_inteiro_pos(n_passos, "n_passos")
  abort_proporcao(p, "p")
  set.seed(seed)
  passos <- sample(c(1, -1), n_passos, replace = TRUE, prob = c(p, 1 - p))
  dados <- tibble::tibble(passo = seq_len(n_passos), posicao = cumsum(passos))
  ggplot2::ggplot(dados, ggplot2::aes(x = .data$passo, y = .data$posicao)) +
    ggplot2::geom_line(color = rnp_paleta_rnp("rnp_qual", 1)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    rnp_tema_rnp() +
    ggplot2::labs(title = "Passeio aleatorio", x = "Passo", y = "Posicao")
}

#' Processo de Poisson homogeneo (simulacao)
#'
#' Simula os tempos de chegada de um processo de Poisson de taxa `taxa` no
#' intervalo `\[0, t_max\]`, via tempos entre chegadas exponenciais.
#'
#' @param taxa Taxa media de ocorrencias por unidade de tempo (positivo).
#' @param t_max Horizonte de tempo.
#' @param seed Inteiro. Semente.
#'
#' @return tibble com `evento`, `tempo_chegada` e `intervalo`.
#'
#' @examples
#' rnp_processo_poisson(taxa = 2, t_max = 5)
#' @family probabilidade
#' @export
rnp_processo_poisson <- function(taxa = 1, t_max = 10, seed = 42L) {
  if (taxa <= 0) rlang::abort("{.arg taxa} deve ser positivo.")
  if (t_max <= 0) rlang::abort("{.arg t_max} deve ser positivo.")
  set.seed(seed)
  tempos <- numeric(0)
  t_atual <- 0
  repeat {
    t_atual <- t_atual + stats::rexp(1, rate = taxa)
    if (t_atual > t_max) break
    tempos <- c(tempos, t_atual)
  }
  if (length(tempos) == 0L) {
    return(tibble::tibble(evento = integer(0), tempo_chegada = numeric(0),
                          intervalo = numeric(0)))
  }
  tibble::tibble(
    evento        = seq_along(tempos),
    tempo_chegada = tempos,
    intervalo     = c(tempos[1L], diff(tempos))
  )
}

#' Simulacao pelo metodo da transformacao inversa
#'
#' Gera amostras de uma distribuicao a partir de sua funcao quantil (inversa
#' da acumulada), aplicada a uniformes(0, 1).
#'
#' @param f_inv Funcao quantil (inversa da F), vetorizada.
#' @param n Inteiro. Tamanho da amostra.
#' @param seed Inteiro. Semente.
#'
#' @return Vetor numerico simulado.
#'
#' @examples
#' # Exponencial(taxa = 2): F^{-1}(u) = -log(1 - u) / 2
#' x <- rnp_simula_inversao(function(u) -log(1 - u) / 2, n = 1000)
#' @family probabilidade
#' @export
rnp_simula_inversao <- function(f_inv, n = 1000L, seed = 42L) {
  if (!is.function(f_inv)) rlang::abort("{.arg f_inv} deve ser uma funcao.")
  abort_inteiro_pos(n, "n")
  set.seed(seed)
  u <- stats::runif(n)
  out <- f_inv(u)
  if (length(out) != n) rlang::abort("{.arg f_inv} deve ser vetorizada (devolver n valores).")
  out
}

#' Simulacao pelo metodo de aceitacao-rejeicao
#'
#' Gera amostras de uma densidade alvo `f` usando uma densidade proposta `g`
#' (da qual se sabe amostrar) e uma constante `M` tal que f(x) <= M g(x).
#'
#' @param f Densidade alvo (funcao vetorizada).
#' @param gerador Funcao que recebe `n` e devolve `n` amostras de g.
#' @param densidade_g Densidade da proposta g (funcao vetorizada).
#' @param M Constante de cobertura (M >= sup f/g).
#' @param n Inteiro. Numero de amostras desejadas.
#' @param seed Inteiro. Semente.
#'
#' @return Uma lista com `amostra` (vetor) e `taxa_aceitacao` (escalar).
#'
#' @examples
#' # Alvo: Beta(2,2) usando proposta Uniforme(0,1), M = 1.5
#' set.seed(1)
#' rnp_simula_aceitacao_rejeicao(
#'   f = function(x) dbeta(x, 2, 2),
#'   gerador = function(n) runif(n),
#'   densidade_g = function(x) dunif(x),
#'   M = 1.5, n = 500)$taxa_aceitacao
#' @family probabilidade
#' @export
rnp_simula_aceitacao_rejeicao <- function(f, gerador, densidade_g, M,
                                          n = 1000L, seed = 42L) {
  if (!is.function(f) || !is.function(gerador) || !is.function(densidade_g)) {
    rlang::abort("{.arg f}, {.arg gerador} e {.arg densidade_g} devem ser funcoes.")
  }
  if (!is.numeric(M) || M <= 0) rlang::abort("{.arg M} deve ser positivo.")
  abort_inteiro_pos(n, "n")
  set.seed(seed)
  aceitos <- numeric(0)
  tentativas <- 0L
  while (length(aceitos) < n) {
    lote <- max(n, 1000L)
    y <- gerador(lote)
    u <- stats::runif(lote)
    razao <- f(y) / (M * densidade_g(y))
    if (any(razao > 1 + 1e-8, na.rm = TRUE)) {
      rlang::abort("M insuficiente: f(x) > M g(x) para algum x. Aumente M.")
    }
    aceitos <- c(aceitos, y[u <= razao])
    tentativas <- tentativas + lote
  }
  list(
    amostra        = aceitos[seq_len(n)],
    taxa_aceitacao = n / tentativas
  )
}

#' Integracao por Monte Carlo
#'
#' Estima a integral de `integrando` em `\[a, b\]` por amostragem uniforme,
#' com erro-padrao e intervalo de confianca.
#'
#' @param integrando Funcao vetorizada a integrar.
#' @param limites Vetor `c(a, b)` com os limites de integracao.
#' @param n Inteiro. Numero de pontos de Monte Carlo.
#' @param conf Nivel de confianca do IC.
#' @param seed Inteiro. Semente.
#' @param digits Inteiro. Casas decimais.
#'
#' @return tibble com `estimativa`, `erro_padrao`, `ic_inf`, `ic_sup`, `n`.
#'
#' @examples
#' # integral de x^2 em [0, 1] = 1/3
#' rnp_monte_carlo(function(x) x^2, limites = c(0, 1), n = 1e5)
#' @family probabilidade
#' @export
rnp_monte_carlo <- function(integrando, limites = c(0, 1), n = 10000L,
                            conf = 0.95, seed = 42L, digits = 4L) {
  if (!is.function(integrando)) rlang::abort("{.arg integrando} deve ser funcao.")
  if (length(limites) != 2L || limites[2L] <= limites[1L]) {
    rlang::abort("{.arg limites} deve ser c(a, b) com a < b.")
  }
  abort_inteiro_pos(n, "n")
  abort_confianca(conf)
  set.seed(seed)
  a <- limites[1L]; b <- limites[2L]
  u <- stats::runif(n, a, b)
  fu <- integrando(u)
  largura <- b - a
  estimativa <- largura * mean(fu)
  se <- largura * stats::sd(fu) / sqrt(n)
  z <- stats::qnorm((1 + conf) / 2)
  tibble::tibble(
    estimativa  = arredonda(estimativa, digits),
    erro_padrao = arredonda(se, digits),
    ic_inf      = arredonda(estimativa - z * se, digits),
    ic_sup      = arredonda(estimativa + z * se, digits),
    n           = n
  )
}
