#' Leitura rapida de bases de dados delimitadas
#'
#' Wrapper de [readr::read_delim()] otimizado para os microdados do INEP
#' (delimitador padrao `"|"` e codificacao `Latin-1`).
#'
#' @param base Local e nome do arquivo.
#' @param sep Separador de colunas.
#' @param dec Separador decimal.
#' @param encoding Codificacao do arquivo.
#' @param nrows Numero de linhas a ler (`Inf` = todas).
#' @param verbose Logico. Exibe mensagens de progresso/diagnostico.
#' @param select Vetor de colunas de interesse (default: todas).
#' @param ... Argumentos extras para [readr::read_delim()].
#'
#' @return Um [tibble][tibble::tibble].
#'
#' @examples
#' \dontrun{
#' rnp_read("dados.csv", sep = "|", encoding = "Latin-1")
#' }
#' @export
rnp_read <- function(base, sep = "|", dec = ".", encoding = "Latin-1", nrows = Inf,
                     verbose = TRUE, select = NULL, ...) {
  n_max <- if (is.finite(nrows)) as.integer(nrows) else Inf
  readr::read_delim(
    file = base,
    delim = sep,
    locale = readr::locale(decimal_mark = dec, encoding = encoding),
    n_max = n_max,
    col_select = if (is.null(select)) NULL else tidyselect::all_of(select),
    show_col_types = FALSE,
    progress = verbose,
    ...
  )
}

#' Estrutura de um objeto (glance)
#'
#' Resume a estrutura de um vetor, `data.frame` ou `tibble`: classe de cada
#' variavel, numero de observacoes e contagem de valores faltantes. Substitui
#' a antiga `rnp_atributos`, agora com saida em `tibble`.
#'
#' @param obj Vetor, `data.frame` ou `tibble`.
#'
#' @return tibble com `variavel`, `classe`, `n`, `n_faltantes`,
#'   `p_faltantes`.
#'
#' @examples
#' rnp_estrutura(mtcars)
#' rnp_estrutura(airquality$Ozone)
#' @export
rnp_estrutura <- function(obj) {
  if (is.null(dim(obj))) {
    return(tibble::tibble(
      variavel    = "x",
      classe      = class(obj)[1L],
      n           = length(obj),
      n_faltantes = sum(is.na(obj)),
      p_faltantes = if (length(obj)) sum(is.na(obj)) / length(obj) else NA_real_
    ))
  }
  if (!is.data.frame(obj)) obj <- as.data.frame(obj)
  tibble::tibble(
    variavel    = names(obj),
    classe      = vapply(obj, function(v) class(v)[1L], character(1)),
    n           = nrow(obj),
    n_faltantes = vapply(obj, function(v) sum(is.na(v)), integer(1)),
    p_faltantes = vapply(obj, function(v) sum(is.na(v)) / length(v), numeric(1))
  )
}

#' Medias (aritmetica, geometrica, harmonica e quadratica)
#'
#' Calcula, de forma unificada, as principais medias de posicao, com suporte
#' a ponderacao. Consolida as antigas `media_aritmetica`, `media_geometrica`
#' e `media_harmonica`.
#'
#' @param x Vetor numerico.
#' @param peso Vetor opcional de pesos (mesmo comprimento de `x`).
#' @param tipo Vetor com um ou mais de `"aritmetica"`, `"geometrica"`,
#'   `"harmonica"`, `"quadratica"` (default: todas).
#' @param na.rm Logico. Remove NA (e pesos correspondentes) antes do calculo.
#' @param digits Inteiro. Casas decimais.
#'
#' @return tibble com colunas `tipo` e `valor`.
#'
#' @details Medias geometrica e harmonica exigem `x > 0`.
#'
#' @examples
#' rnp_medias(c(2, 4, 8))
#' rnp_medias(c(2, 4, 8), peso = c(1, 2, 1), tipo = c("aritmetica", "harmonica"))
#' @family descritiva
#' @export
rnp_medias <- function(x, peso = NULL,
                       tipo = c("aritmetica", "geometrica", "harmonica", "quadratica"),
                       na.rm = TRUE, digits = 4L) {
  abort_numerico(x, "x")
  tipo <- rlang::arg_match(tipo, multiple = TRUE)
  if (!is.null(peso)) {
    abort_numerico(peso, "peso")
    if (length(peso) != length(x)) {
      rlang::abort("{.arg peso} deve ter o mesmo comprimento de {.arg x}.")
    }
  }
  if (na.rm) {
    keep <- !is.na(x) & (if (is.null(peso)) TRUE else !is.na(peso))
    x <- x[keep]
    if (!is.null(peso)) peso <- peso[keep]
  }
  if (length(x) < 1L) rlang::abort("Sem observacoes validas.")
  w <- if (is.null(peso)) rep(1, length(x)) else peso
  sw <- sum(w)
  precisa_pos <- any(c("geometrica", "harmonica") %in% tipo)
  if (precisa_pos && any(x <= 0)) {
    rlang::abort("Medias geometrica/harmonica exigem todos os valores positivos.")
  }
  calc <- function(t) switch(t,
    aritmetica = sum(w * x) / sw,
    geometrica = exp(sum(w * log(x)) / sw),
    harmonica  = sw / sum(w / x),
    quadratica = sqrt(sum(w * x^2) / sw)
  )
  tibble::tibble(
    tipo  = tipo,
    valor = arredonda(vapply(tipo, calc, numeric(1)), digits)
  )
}

#' Dados do censo ensino superior INEP 2017 (Cursos)
#'
#' @description Base de dados do INEP para o censo brasileiro do ensino superior (cursos).
#' @name dm_curso
#' @docType data
#' @author INEP
#' @references \url{http://inep.gov.br/microdados}
#' @keywords dm_curso
#' @examples
#' require(rnp)
#' str(dm_curso)
NULL

#' Dados do censo ensino superior INEP 2017 (IES)
#'
#' @description Base de dados do INEP para o censo brasileiro do ensino superior (IES).
#' @name dm_ies
#' @docType data
#' @author INEP
#' @references \url{http://inep.gov.br/microdados}
#' @keywords dm_ies
#' @examples
#' require(rnp)
#' str(dm_ies)
NULL

#' Dados do censo ensino superior INEP 2017 (Docente)
#'
#' @description Base de dados do INEP para o censo brasileiro do ensino superior (docentes).
#' @name dm_docente
#' @docType data
#' @author INEP
#' @references \url{http://inep.gov.br/microdados}
#' @keywords dm_docente
#' @examples
#' require(rnp)
#' str(dm_docente)
NULL

#' Dados do censo ensino superior INEP 2017 (Local da oferta)
#'
#' @description Base de dados do INEP para o censo brasileiro do ensino superior (locais de oferta).
#' @name dm_local
#' @docType data
#' @author INEP
#' @references \url{http://inep.gov.br/microdados}
#' @keywords dm_local
#' @examples
#' require(rnp)
#' str(dm_local)
NULL
