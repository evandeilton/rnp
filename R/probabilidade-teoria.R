#' Teorema de Bayes (particao do espaco amostral)
#'
#' Calcula as probabilidades a posteriori P(H_i | E) a partir das
#' probabilidades a priori P(H_i) e das verossimilhancas P(E | H_i), para
#' uma particao de hipoteses mutuamente exclusivas e exaustivas. O caso
#' classico de dois eventos e obtido com vetores de comprimento 2.
#'
#' @param priori Vetor de probabilidades a priori (deve somar 1).
#' @param verossimilhanca Vetor P(E | H_i), mesmo comprimento de `priori`.
#' @param hipoteses Vetor opcional de rotulos das hipoteses.
#' @param digits Inteiro. Casas decimais.
#'
#' @return tibble com `hipotese`, `priori`, `verossimilhanca`,
#'   `conjunta` (P(H_i e E)) e `posteriori` (P(H_i | E)).
#'
#' @examples
#' # Teste diagnostico: prevalencia 1%, sensibilidade 99%, especificidade 95%
#' rnp_bayes(priori = c(doente = 0.01, sadio = 0.99),
#'           verossimilhanca = c(0.99, 0.05))
#' @family probabilidade
#' @export
rnp_bayes <- function(priori, verossimilhanca, hipoteses = NULL, digits = 4L) {
  abort_numerico(priori, "priori")
  abort_numerico(verossimilhanca, "verossimilhanca")
  if (length(priori) != length(verossimilhanca)) {
    rlang::abort("{.arg priori} e {.arg verossimilhanca} devem ter o mesmo comprimento.")
  }
  if (any(priori < 0) || any(verossimilhanca < 0)) {
    rlang::abort("Probabilidades nao podem ser negativas.")
  }
  if (abs(sum(priori) - 1) > 1e-6) {
    rlang::warn("As probabilidades a priori nao somam 1; verifique a particao.")
  }
  if (is.null(hipoteses)) {
    hipoteses <- names(priori) %||% paste0("H", seq_along(priori))
  }
  priori <- unname(priori)
  verossimilhanca <- unname(verossimilhanca)
  conjunta <- priori * verossimilhanca
  evidencia <- sum(conjunta)
  if (evidencia == 0) rlang::abort("Evidencia com probabilidade total nula.")
  posteriori <- conjunta / evidencia
  tibble::tibble(
    hipotese        = as.character(hipoteses),
    priori          = arredonda(priori, digits),
    verossimilhanca = arredonda(verossimilhanca, digits),
    conjunta        = arredonda(conjunta, digits),
    posteriori      = arredonda(posteriori, digits)
  )
}

#' Distribuicao conjunta discreta: marginais, momentos e dependencia
#'
#' A partir de uma matriz de probabilidades conjuntas P(X = x, Y = y),
#' calcula as distribuicoes marginais, E\[X\], E\[Y\], variancias,
#' covariancia e correlacao. Os valores de X e Y sao lidos dos nomes de
#' linha/coluna (se numericos) ou informados explicitamente.
#'
#' @param p Matriz de probabilidades conjuntas (linhas = X, colunas = Y).
#' @param valores_x,valores_y Vetores numericos com os valores de X e Y.
#'   Se `NULL`, usa os nomes de linha/coluna convertidos para numero.
#' @param digits Inteiro. Casas decimais.
#'
#' @return Uma lista com `marginal_x`, `marginal_y` (tibbles) e `resumo`
#'   (tibble com `e_x`, `e_y`, `var_x`, `var_y`, `cov_xy`, `cor_xy`).
#'
#' @examples
#' p <- matrix(c(0.1, 0.2, 0.2, 0.5), 2, 2,
#'             dimnames = list(c("0", "1"), c("0", "1")))
#' rnp_distribuicao_conjunta(p)
#' @family probabilidade
#' @export
rnp_distribuicao_conjunta <- function(p, valores_x = NULL, valores_y = NULL,
                                      digits = 4L) {
  if (!is.matrix(p) && !is.data.frame(p)) {
    rlang::abort("{.arg p} deve ser matriz de probabilidades conjuntas.")
  }
  p <- as.matrix(p)
  if (any(p < 0) || abs(sum(p) - 1) > 1e-6) {
    rlang::abort("As entradas de {.arg p} devem ser >= 0 e somar 1.")
  }
  vx <- valores_x %||% as.numeric(rownames(p)) %||% seq_len(nrow(p))
  vy <- valores_y %||% as.numeric(colnames(p)) %||% seq_len(ncol(p))
  if (anyNA(vx) || anyNA(vy)) {
    rlang::abort("Valores de X/Y nao numericos: informe {.arg valores_x}/{.arg valores_y}.")
  }
  px <- rowSums(p)
  py <- colSums(p)
  e_x <- sum(vx * px)
  e_y <- sum(vy * py)
  var_x <- sum((vx - e_x)^2 * px)
  var_y <- sum((vy - e_y)^2 * py)
  e_xy <- sum(outer(vx, vy) * p)
  cov_xy <- e_xy - e_x * e_y
  cor_xy <- if (var_x > 0 && var_y > 0) cov_xy / sqrt(var_x * var_y) else NA_real_
  .rnp_lista(list(
    marginal_x = tibble::tibble(x = vx, p = arredonda(px, digits)),
    marginal_y = tibble::tibble(y = vy, p = arredonda(py, digits)),
    resumo = tibble::tibble(
      e_x = e_x, e_y = e_y, var_x = var_x, var_y = var_y,
      cov_xy = cov_xy, cor_xy = cor_xy
    ) |> dplyr::mutate(dplyr::across(where(is.numeric), ~ arredonda(.x, digits)))
  ), "Distribuicao conjunta discreta")
}

#' Esperanca e variancia condicionais E\[Y | X = x\]
#'
#' Para uma distribuicao conjunta discreta, calcula a esperanca e a
#' variancia de Y condicionadas a cada valor de X.
#'
#' @inheritParams rnp_distribuicao_conjunta
#'
#' @return tibble com `x`, `e_y_dado_x` e `var_y_dado_x`.
#'
#' @examples
#' p <- matrix(c(0.1, 0.2, 0.2, 0.5), 2, 2,
#'             dimnames = list(c("0", "1"), c("0", "1")))
#' rnp_esperanca_condicional(p)
#' @family probabilidade
#' @export
rnp_esperanca_condicional <- function(p, valores_x = NULL, valores_y = NULL,
                                      digits = 4L) {
  if (!is.matrix(p) && !is.data.frame(p)) {
    rlang::abort("{.arg p} deve ser matriz de probabilidades conjuntas.")
  }
  p <- as.matrix(p)
  vx <- valores_x %||% as.numeric(rownames(p)) %||% seq_len(nrow(p))
  vy <- valores_y %||% as.numeric(colnames(p)) %||% seq_len(ncol(p))
  px <- rowSums(p)
  e_cond <- vapply(seq_len(nrow(p)), function(i) {
    if (px[i] == 0) return(NA_real_)
    sum(vy * p[i, ] / px[i])
  }, numeric(1))
  v_cond <- vapply(seq_len(nrow(p)), function(i) {
    if (px[i] == 0) return(NA_real_)
    sum((vy - e_cond[i])^2 * p[i, ] / px[i])
  }, numeric(1))
  tibble::tibble(
    x            = vx,
    e_y_dado_x   = arredonda(e_cond, digits),
    var_y_dado_x = arredonda(v_cond, digits)
  )
}

#' Lei dos Grandes Numeros (simulacao)
#'
#' Simula a convergencia da media amostral para a media teorica conforme o
#' tamanho da amostra cresce, ilustrando a Lei dos Grandes Numeros.
#'
#' @param gerador Funcao de geracao que recebe `n` e devolve `n` valores
#'   (ex.: `function(n) rnorm(n)`). Default: lancamentos de um dado honesto.
#' @param n_max Inteiro. Numero maximo de observacoes.
#' @param media_teorica Valor de referencia (linha horizontal). Se `NULL`,
#'   tenta estimar por uma amostra grande.
#' @param seed Inteiro. Semente.
#'
#' @return Objeto `ggplot` com a media acumulada versus n.
#'
#' @examples
#' rnp_lei_grandes_numeros(function(n) rbinom(n, 1, 0.3), media_teorica = 0.3)
#' @family probabilidade
#' @export
rnp_lei_grandes_numeros <- function(gerador = function(n) sample(1:6, n, TRUE),
                                    n_max = 1000L, media_teorica = NULL,
                                    seed = 42L) {
  abort_inteiro_pos(n_max, "n_max")
  set.seed(seed)
  x <- gerador(n_max)
  if (!is.numeric(x) || length(x) != n_max) {
    rlang::abort("{.arg gerador} deve devolver um vetor numerico de comprimento n.")
  }
  if (is.null(media_teorica)) media_teorica <- mean(gerador(1e5))
  dados <- tibble::tibble(
    n            = seq_len(n_max),
    media_acum   = cumsum(x) / seq_len(n_max)
  )
  ggplot2::ggplot(dados, ggplot2::aes(x = .data$n, y = .data$media_acum)) +
    ggplot2::geom_line(color = rnp_paleta_rnp("rnp_qual", 1)) +
    ggplot2::geom_hline(yintercept = media_teorica, linetype = "dashed",
                        color = "red") +
    rnp_tema_rnp() +
    ggplot2::labs(title = "Lei dos Grandes Numeros",
                  x = "Tamanho da amostra (n)", y = "Media acumulada")
}

#' Teorema Central do Limite (simulacao)
#'
#' Gera `n_amostras` amostras de tamanho `n` a partir de um gerador qualquer
#' e exibe o histograma das medias padronizadas comparado a Normal(0, 1),
#' ilustrando o TCL.
#'
#' @param gerador Funcao que recebe `n` e devolve `n` valores. Default:
#'   distribuicao exponencial (assimetrica), para evidenciar o efeito.
#' @param n Inteiro. Tamanho de cada amostra.
#' @param n_amostras Inteiro. Numero de amostras (medias).
#' @param seed Inteiro. Semente.
#'
#' @return Objeto `ggplot`.
#'
#' @examples
#' rnp_tcl_simulacao(function(n) rexp(n), n = 30, n_amostras = 1000)
#' @family probabilidade
#' @export
rnp_tcl_simulacao <- function(gerador = function(n) rexp(n),
                              n = 30L, n_amostras = 1000L, seed = 42L) {
  abort_inteiro_pos(n, "n")
  abort_inteiro_pos(n_amostras, "n_amostras")
  set.seed(seed)
  medias <- vapply(seq_len(n_amostras), function(i) mean(gerador(n)), numeric(1))
  z <- (medias - mean(medias)) / stats::sd(medias)
  dados <- tibble::tibble(z = z)
  ggplot2::ggplot(dados, ggplot2::aes(x = .data$z)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                            bins = 30,
                            fill = rnp_paleta_rnp("rnp_qual", 1),
                            color = "white", alpha = 0.8) +
    ggplot2::stat_function(fun = stats::dnorm, color = "red", linewidth = 1) +
    rnp_tema_rnp() +
    ggplot2::labs(
      title = "Teorema Central do Limite",
      subtitle = glue::glue("Medias de {n_amostras} amostras de tamanho n = {n}"),
      x = "Media padronizada", y = "Densidade")
}
