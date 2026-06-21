#' rnp: Recursos estatisticos para o projeto R NA PRATICA
#'
#' Pacote didatico com funcoes estatisticas de producao: descritiva,
#' inferencia, regressao, multivariada, series temporais, probabilidade,
#' amostragem e visualizacao ggplot2. Inclui ferramentas para dados do
#' censo do ensino superior do INEP.
#'
#' @section Modulos:
#'
#' * [rnp_descritiva()], [rnp_quantis()], [rnp_outliers()]
#' * [rnp_tabela_classes()], [rnp_distribuicao()]
#' * [rnp_ic_media()], [rnp_teste_t()], [rnp_anova()]
#' * [rnp_correlacao_teste()], [rnp_regressao()]
#' * [rnp_pca()], [rnp_kmeans()]
#' * [rnp_ts_decomposicao()], [rnp_ts_arima()]
#' * [rnp_combinacao()], [rnp_distribuicao_binomial()]
#' * [rnp_grafico_dispersao()], [rnp_tema_rnp()]
#' * [rnp_amostra_simples()], [rnp_tamanho_amostra_media()]
#' * [rnp_tamanho_efeito()], [rnp_na_summary()]
#'
#' @author Jose E. Lopes \email{evandeilton@@gmail.com}
#' @keywords internal
#' @aliases rnp-package
#' @useDynLib rnp, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom stats density is.ts model.frame model.matrix na.exclude na.omit nobs
#' @importFrom utils combn
"_PACKAGE"

# Suppress R CMD check NOTE for global variables used in NSE
utils::globalVariables(
  names = c(
    ".", ".data", "variavel", "classe", "fa", "fr",
    "Faa", "Fra", "Valor", "Estatistica",
    "Var1", "Var2", "Freq", "where",
    # nomes usados em avaliacao nao-padrao (NSE) nos pipelines dplyr/ggplot
    "ano_in", "url_in", "cluster", "nobs", "n", "n_faltantes",
    "df_residuos", "gl_residuos", "gl1", "gl2", "indice", "lags",
    "observacao", "se", "studentizado", "density"
  )
)

# Allowed clipboard
# The following block is used to prevent no visible binding
# when called from dplyr pipelines with NSE.
NULL

#' Opcoes do pacote rnp
#'
#' @param ... nome=valor
#' @return Lista invisivel de opcoes anteriores.
#' @export
rnp_options <- function(...) {
  old <- options(...)
  invisible(old)
}

.onLoad <- function(libname, pkgname) {
  op <- options()
  op_rnp <- list(
    rnp.digits  = 4L,
    rnp.lang    = "pt-BR",
    rnp.theme   = "rnp",
    rnp.palette = "rnp_qual"
  )
  toset <- !(names(op_rnp) %in% names(op))
  if (any(toset)) options(op_rnp[toset])
  invisible()
}
