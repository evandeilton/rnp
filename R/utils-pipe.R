#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' Extras do stats para estatistica
#'
#' @name "ex1"
# @export
#' @importFrom "stats" "IQR" "addmargins" "chisq.test" "cor" "median" "na.exclude" "na.omit" "quantile" "sd"
NULL

#' Extras do utils install.packages
#'
#' @name "ex2"
# @export
#' @importFrom "utils" "install.packages"
NULL


# variaveis globais
if(getRversion() >= "2.15.1")  utils::globalVariables(
  c('::',':','.','VAR_NOME','VAR_CATEGORIA','VAR_DESCRICAO','ORD','NOME DA VARI??VEL', 'VAR_DESCRICAO_CATEGORIAS','ORDEM','VAR_DESCRICAO_CATEGORIAS_FIX','ano_in')
)
