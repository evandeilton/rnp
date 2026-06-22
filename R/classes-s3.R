# Sistema de classes S3 do rnp (Fase A do polimento).
#
# As funcoes que retornam uma lista de componentes (tabelas, escalares, modelos,
# graficos) sao envolvidas por `.rnp_lista()`, que apenas anexa a classe
# `rnp_resultado` e um titulo. A estrutura subjacente continua sendo uma `list`,
# de modo que `$`, `[[`, `is.list()`, `names()` etc. seguem funcionando — apenas
# a *impressao* fica mais elegante, via `print.rnp_resultado()`.

# Converte um nome tecnico em rotulo legivel: "hazard_ratio" -> "Hazard ratio".
.titulo_legivel <- function(x) {
  x <- gsub("_", " ", x)
  paste0(toupper(substring(x, 1, 1)), substring(x, 2))
}

# Anexa a classe rnp_resultado e o titulo a uma lista de resultados.
.rnp_lista <- function(x, titulo = NULL) {
  attr(x, "rnp_titulo") <- titulo
  class(x) <- c("rnp_resultado", "list")
  x
}

# Classes de objetos-modelo que devem ser resumidas em uma linha (nunca impressas
# por extenso) no print do resultado.
.classes_modelo <- c(
  "lm", "glm", "coxph", "survreg", "survfit", "Arima", "gam", "lme", "nls",
  "prcomp", "kmeans", "hclust", "polr", "negbin", "factanal", "manova",
  "loess", "HoltWinters", "stl", "decomposed.ts", "dist", "model_fit",
  "workflow", "recipe", "rset", "rsplit"
)

#' Imprime um resultado do rnp
#'
#' Metodo de impressao para os objetos de classe `rnp_resultado` (listas de
#' componentes devolvidas por varias funcoes do pacote). Imprime cada componente
#' de forma legivel: tabelas sao mostradas; objetos-modelo e graficos sao
#' resumidos em uma linha; escalares aparecem em linha.
#'
#' @param x Objeto de classe `rnp_resultado`.
#' @param ... Ignorado.
#'
#' @return `x`, de forma invisivel.
#' @exportS3Method base::print
print.rnp_resultado <- function(x, ...) {
  titulo <- attr(x, "rnp_titulo")
  if (!is.null(titulo)) cli::cli_h1(titulo)
  nomes <- names(x)
  if (is.null(nomes)) nomes <- rep("", length(x))
  for (i in seq_along(x)) {
    el <- x[[i]]
    rotulo <- if (nzchar(nomes[i])) .titulo_legivel(nomes[i]) else paste0("[", i, "]")
    if (inherits(el, "ggplot")) {
      cli::cli_text("{.strong {rotulo}}: grafico {.code ggplot} (use {.code $`{nomes[i]}`} para exibir)")
    } else if (inherits(el, .classes_modelo)) {
      cli::cli_text("{.strong {rotulo}}: objeto {.cls {class(el)[1]}}")
    } else if (is.data.frame(el)) {
      cli::cli_h3(rotulo)
      print(el)
    } else if (is.atomic(el) && length(el) <= 10L) {
      cli::cli_text("{.strong {rotulo}}: {.val {el}}")
    } else if (is.atomic(el)) {
      cli::cli_text("{.strong {rotulo}}: vetor de {length(el)} valores")
    } else if (is.list(el)) {
      cli::cli_text("{.strong {rotulo}}: lista de {length(el)} elemento{?s}")
    } else {
      cli::cli_text("{.strong {rotulo}}: {.cls {class(el)[1]}}")
    }
  }
  invisible(x)
}

# ---------------------------------------------------------------------------
# Integracao com o ecossistema broom (genericos do pacote `generics`).
# A convencao do rnp facilita: o componente de "termos" (coeficientes,
# estimativas, ...) vira `tidy()`; o de "resumo do ajuste" (modelo, qualidade,
# ...) vira `glance()`. Assim, os resultados do rnp conversam com o tidymodels.
# ---------------------------------------------------------------------------

#' @importFrom generics tidy
#' @export
generics::tidy

#' @importFrom generics glance
#' @export
generics::glance

#' Tabela de termos (tidy) de um resultado do rnp
#'
#' Extrai a tabela de coeficientes/estimativas de um objeto `rnp_resultado`,
#' no formato esperado pelo `broom`/`tidymodels`.
#'
#' @param x Objeto de classe `rnp_resultado`.
#' @param ... Ignorado.
#' @return Um [tibble][tibble::tibble] com um termo por linha.
#' @exportS3Method generics::tidy
tidy.rnp_resultado <- function(x, ...) {
  for (nm in c("coeficientes", "estimativas", "parametros", "tidy",
               "silhuetas", "curva", "tabela")) {
    if (is.data.frame(x[[nm]])) return(tibble::as_tibble(x[[nm]]))
  }
  for (el in x) if (is.data.frame(el)) return(tibble::as_tibble(el))
  rlang::abort("Nenhuma tabela de termos ('tidy') encontrada neste resultado.")
}

#' Resumo do ajuste (glance) de um resultado do rnp
#'
#' Extrai a tabela de medidas-resumo do modelo de um objeto `rnp_resultado`
#' (uma linha), no formato esperado pelo `broom`/`tidymodels`.
#'
#' @param x Objeto de classe `rnp_resultado`.
#' @param ... Ignorado.
#' @return Um [tibble][tibble::tibble] de uma linha.
#' @exportS3Method generics::glance
glance.rnp_resultado <- function(x, ...) {
  for (nm in c("modelo", "qualidade", "ajuste", "resumo", "metricas",
               "variancia")) {
    if (is.data.frame(x[[nm]])) return(tibble::as_tibble(x[[nm]]))
  }
  rlang::abort("Nenhum resumo de ajuste ('glance') encontrado neste resultado.")
}
