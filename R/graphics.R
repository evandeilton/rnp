#' Tema ggplot2 do projeto R NA PRATICA
#'
#' Tema minimalista com cores e tipografia educacionais.
#'
#' @param base_size Escalar. Tamanho base da fonte.
#' @param base_family String. Familia de fonte.
#'
#' @return Objeto \code{theme} do ggplot2.
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) + geom_point() + rnp_tema_rnp()
#' @export
rnp_tema_rnp <- function(base_size = 11, base_family = "") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = base_size * 1.2, hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = base_size, hjust = 0.5, color = "grey40"),
      axis.title = ggplot2::element_text(face = "bold", size = base_size),
      axis.text = ggplot2::element_text(size = base_size * 0.9, color = "grey30"),
      panel.grid.major = ggplot2::element_line(color = "grey90", linewidth = 0.3),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(face = "bold", size = base_size * 0.9),
      legend.text = ggplot2::element_text(size = base_size * 0.85),
      strip.text = ggplot2::element_text(face = "bold", size = base_size),
      strip.background = ggplot2::element_rect(fill = "grey95", color = NA)
    )
}

#' Paletas de cores do projeto R NA PRATICA
#'
#' Paletas qualitativa, sequencial e divergente.
#'
#' @param nome String: \code{"rnp_qual"}, \code{"rnp_seq"}, \code{"rnp_div"}.
#' @param n Inteiro. Numero de cores.
#'
#' @return Vetor de cores (hex).
#'
#' @examples
#' rnp_paleta_rnp("rnp_qual", 5)
#' rnp_paleta_rnp("rnp_seq", 10)
#' @export
rnp_paleta_rnp <- function(nome = c("rnp_qual", "rnp_seq", "rnp_div"), n = NULL) {
  nome <- rlang::arg_match(nome)
  paletas <- list(
    rnp_qual = c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#3B1F2B",
                 "#44BBA4", "#E94F37", "#393E41", "#8D6A9F", "#5BC0EB"),
    rnp_seq  = c("#F7FBFF", "#DEEBF7", "#C6DBEF", "#9ECAE1", "#6BAED6",
                 "#4292C6", "#2171B5", "#08519C", "#08306B"),
    rnp_div  = c("#D73027", "#F46D43", "#FDAE61", "#FEE090", "#FFFFBF",
                 "#E0F3F8", "#ABD9E9", "#74ADD1", "#4575B4")
  )
  p <- paletas[[nome]]
  if (is.null(n)) return(p)
  abort_inteiro_pos(n, "n")
  if (n > length(p)) {
    rlang::warn("n > {length(p)}. Interpolando cores.")
    grDevices::colorRampPalette(p)(n)
  } else {
    p[seq_len(n)]
  }
}

#' Grafico de barras
#'
#' Wrapper ggplot2 para grafico de barras.
#'
#' @param base data.frame.
#' @param x Nome da coluna (string) para eixo x.
#' @param y Nome opcional da coluna (string) para eixo y. Se NULL, conta.
#' @param fill Nome opcional da coluna para preenchimento.
#' @param position String: \code{"dodge"}, \code{"stack"}, \code{"fill"}.
#' @param titulo String. Titulo do grafico.
#' @param xlab,ylab String. Rotulos dos eixos.
#' @param tema Objeto \code{theme} do ggplot2. Default \code{rnp_tema_rnp()}.
#'
#' @return Objeto ggplot.
#'
#' @examples
#' rnp_grafico_barras(mtcars, "cyl")
#' @export
rnp_grafico_barras <- function(base, x, y = NULL, fill = NULL,
                               position = c("dodge", "stack", "fill"),
                               titulo = NULL, xlab = NULL, ylab = NULL,
                               tema = rnp_tema_rnp()) {
  if (!is.data.frame(base)) rlang::abort("{.arg base} deve ser data.frame.")
  if (!is.character(x) || length(x) != 1L) rlang::abort("{.arg x} deve ser string.")
  if (!x %in% names(base)) rlang::abort("Coluna '{x}' nao encontrada.")
  position <- rlang::arg_match(position)
  aes_args <- list(x = ggplot2::sym(x))
  if (!is.null(fill)) aes_args$fill <- ggplot2::sym(fill)
  p <- ggplot2::ggplot(base, do.call(ggplot2::aes, aes_args))
  if (is.null(y)) {
    p <- p + ggplot2::geom_bar(position = position, fill = if (is.null(fill)) rnp_paleta_rnp("rnp_qual", 1) else NULL)
  } else {
    aes_args$y <- ggplot2::sym(y)
    p <- ggplot2::ggplot(base, do.call(ggplot2::aes, aes_args)) +
      ggplot2::geom_col(position = position, fill = if (is.null(fill)) rnp_paleta_rnp("rnp_qual", 1) else NULL)
  }
  p + tema +
    ggplot2::labs(title = titulo, x = xlab, y = ylab)
}

#' Grafico de boxplot
#'
#' @param base data.frame.
#' @param x Nome da coluna (string) para eixo x (categorica).
#' @param y Nome da coluna (string) para eixo y (numerica).
#' @param fill Nome opcional da coluna para preenchimento.
#' @param titulo,xlab,ylab String.
#' @param tema Objeto \code{theme}.
#'
#' @return Objeto ggplot.
#'
#' @examples
#' rnp_grafico_boxplot(mtcars, "cyl", "mpg")
#' @export
rnp_grafico_boxplot <- function(base, x, y, fill = NULL,
                                titulo = NULL, xlab = NULL, ylab = NULL,
                                tema = rnp_tema_rnp()) {
  if (!is.data.frame(base)) rlang::abort("{.arg base} deve ser data.frame.")
  if (!x %in% names(base) || !y %in% names(base)) {
    rlang::abort("Colunas '{x}' ou '{y}' nao encontradas.")
  }
  aes_args <- list(x = ggplot2::sym(x), y = ggplot2::sym(y))
  if (!is.null(fill)) aes_args$fill <- ggplot2::sym(fill)
  ggplot2::ggplot(base, do.call(ggplot2::aes, aes_args)) +
    ggplot2::geom_boxplot(fill = if (is.null(fill)) rnp_paleta_rnp("rnp_qual", 1) else NULL) +
    tema +
    ggplot2::labs(title = titulo, x = xlab, y = ylab)
}

#' Grafico de dispersao
#'
#' @param base data.frame.
#' @param x,y Nomes das colunas (strings).
#' @param cor Nome opcional da coluna para cor.
#' @param tamanho Escalar ou nome de coluna para tamanho dos pontos.
#' @param suavizar Logico. Adicionar linha de suavizacao (loess).
#' @param titulo,xlab,ylab String.
#' @param tema Objeto \code{theme}.
#'
#' @return Objeto ggplot.
#'
#' @examples
#' rnp_grafico_dispersao(mtcars, "wt", "mpg")
#' @export
rnp_grafico_dispersao <- function(base, x, y, cor = NULL, tamanho = 2,
                                  suavizar = FALSE,
                                  titulo = NULL, xlab = NULL, ylab = NULL,
                                  tema = rnp_tema_rnp()) {
  if (!is.data.frame(base)) rlang::abort("{.arg base} deve ser data.frame.")
  if (!x %in% names(base) || !y %in% names(base)) {
    rlang::abort("Colunas '{x}' ou '{y}' nao encontradas.")
  }
  aes_args <- list(x = ggplot2::sym(x), y = ggplot2::sym(y))
  if (!is.null(cor)) aes_args$color <- ggplot2::sym(cor)
  if (is.character(tamanho)) aes_args$size <- ggplot2::sym(tamanho)
  p <- ggplot2::ggplot(base, do.call(ggplot2::aes, aes_args)) +
    ggplot2::geom_point(size = if (is.numeric(tamanho)) tamanho else NULL,
                        color = if (is.null(cor)) rnp_paleta_rnp("rnp_qual", 1) else NULL)
  if (suavizar) p <- p + ggplot2::geom_smooth(method = "loess", se = TRUE, color = "grey30")
  p + tema +
    ggplot2::labs(title = titulo, x = xlab, y = ylab)
}

#' Grafico de histograma
#'
#' @param base data.frame.
#' @param x Nome da coluna (string).
#' @param bins Inteiro. Numero de bins.
#' @param fill Cor de preenchimento.
#' @param densidade Logico. Mostrar densidade em vez de contagem.
#' @param titulo,xlab,ylab String.
#' @param tema Objeto \code{theme}.
#'
#' @return Objeto ggplot.
#'
#' @examples
#' rnp_grafico_histograma(mtcars, "mpg")
#' @export
rnp_grafico_histograma <- function(base, x, bins = 30, fill = rnp_paleta_rnp("rnp_qual", 1),
                                   densidade = FALSE,
                                   titulo = NULL, xlab = NULL, ylab = NULL,
                                   tema = rnp_tema_rnp()) {
  if (!is.data.frame(base)) rlang::abort("{.arg base} deve ser data.frame.")
  if (!x %in% names(base)) rlang::abort("Coluna '{x}' nao encontrada.")
  aes_args <- list(x = ggplot2::sym(x))
  if (densidade) aes_args$y <- ggplot2::after_stat(density)
  ggplot2::ggplot(base, do.call(ggplot2::aes, aes_args)) +
    ggplot2::geom_histogram(bins = bins, fill = fill, color = "white", alpha = 0.8) +
    tema +
    ggplot2::labs(title = titulo, x = xlab, y = if (is.null(ylab)) (if (densidade) "Densidade" else "Contagem") else ylab)
}

#' Grafico QQ (quantil-quantil)
#'
#' @param x Vetor numerico.
#' @param distribuicao String: \code{"norm"}, \code{"t"}, \code{"chisq"}, etc.
#' @param ... Argumentos da distribuicao (ex.: \code{df} para t).
#' @param titulo,xlab,ylab String.
#' @param tema Objeto \code{theme}.
#'
#' @return Objeto ggplot.
#'
#' @examples
#' rnp_grafico_qq(rnorm(100))
#' @export
rnp_grafico_qq <- function(x, distribuicao = "norm", ...,
                           titulo = NULL, xlab = "Quantis Teoricos",
                           ylab = "Quantis Amostrais",
                           tema = rnp_tema_rnp()) {
  abort_numerico(x, "x")
  x <- sem_na(x)
  n <- length(x)
  if (n < 3L) rlang::abort("QQ plot requer n >= 3.")
  probs <- (seq_len(n) - 0.5) / n
  fname <- paste0("q", distribuicao)
  qfun <- tryCatch(utils::getFromNamespace(fname, "stats"),
                   error = function(e) NULL)
  if (is.null(qfun)) rlang::abort("Distribuicao '{distribuicao}' sem funcao quantil em 'stats'.")
  teoricos <- qfun(probs, ...)
  dados <- tibble::tibble(teorico = teoricos, amostral = sort(x))
  ggplot2::ggplot(dados, ggplot2::aes(x = .data$teorico, y = .data$amostral)) +
    ggplot2::geom_point(color = rnp_paleta_rnp("rnp_qual", 1), size = 2) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
    tema +
    ggplot2::labs(title = titulo, x = xlab, y = ylab)
}

#' Grafico de violino
#'
#' @param base data.frame.
#' @param x Nome da coluna (string) para eixo x (categorica).
#' @param y Nome da coluna (string) para eixo y (numerica).
#' @param fill Nome opcional da coluna para preenchimento.
#' @param titulo,xlab,ylab String.
#' @param tema Objeto \code{theme}.
#'
#' @return Objeto ggplot.
#'
#' @examples
#' rnp_grafico_violino(mtcars, "cyl", "mpg")
#' @export
rnp_grafico_violino <- function(base, x, y, fill = NULL,
                                titulo = NULL, xlab = NULL, ylab = NULL,
                                tema = rnp_tema_rnp()) {
  if (!is.data.frame(base)) rlang::abort("{.arg base} deve ser data.frame.")
  if (!x %in% names(base) || !y %in% names(base)) {
    rlang::abort("Colunas '{x}' ou '{y}' nao encontradas.")
  }
  aes_args <- list(x = ggplot2::sym(x), y = ggplot2::sym(y))
  if (!is.null(fill)) aes_args$fill <- ggplot2::sym(fill)
  ggplot2::ggplot(base, do.call(ggplot2::aes, aes_args)) +
    ggplot2::geom_violin(fill = if (is.null(fill)) rnp_paleta_rnp("rnp_qual", 1) else NULL,
                         trim = FALSE, alpha = 0.7) +
    ggplot2::geom_boxplot(width = 0.1, fill = "white", alpha = 0.5) +
    tema +
    ggplot2::labs(title = titulo, x = xlab, y = ylab)
}
