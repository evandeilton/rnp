#' Operador pipe e outros recursos
#'
#' Veja \code{magrittr::\link[magrittr]{\%>\%}} para mais detalhes.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @importFrom "stats" "IQR" "addmargins" "chisq.test" "cor" "median" "na.exclude" "na.omit" "quantile" "sd" "weighted.mean"
#' @importFrom "utils" "install.packages"
#' @usage lhs \%>\% rhs
NULL

# variaveis globais
utils::globalVariables(c('::',':','.','VAR_NOME','VAR_CATEGORIA','VAR_DESCRICAO','ORD','NOME DA VARIAVEL', 'VAR_DESCRICAO_CATEGORIAS','ORDEM','VAR_DESCRICAO_CATEGORIAS_FIX','ano_in'))

