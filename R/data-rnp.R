# Documentacao dos conjuntos de dados didaticos do rnp.
# Sao dados SIMULADOS (sementes fixas em data-raw/datasets.R), criados para
# ilustrar os metodos do pacote em contextos de engenharia. Nao representam
# medicoes reais.

#' Resistencia de corpos de prova de concreto (didatico)
#'
#' Dados **simulados** de ensaios de compressao de corpos de prova de concreto,
#' para ilustrar estatistica descritiva, ANOVA e regressao.
#'
#' @format Um `data.frame` com 90 linhas e 3 variaveis:
#' \describe{
#'   \item{resistencia}{Resistencia a compressao (MPa).}
#'   \item{dias_cura}{Tempo de cura (7, 14 ou 28 dias).}
#'   \item{tipo_cimento}{Tipo de cimento (`CP-II`, `CP-IV`, `CP-V`).}
#' }
#' @details Dados simulados para fins didaticos; ver `data-raw/datasets.R`.
#' @examples
#' rnp_descritiva(rnp_concreto$resistencia)
#' rnp_anova(resistencia ~ tipo_cimento, data = rnp_concreto)
"rnp_concreto"

#' Defeitos por lote de producao (didatico)
#'
#' Dados **simulados** de contagem de defeitos por lote, por turno e maquina,
#' para ilustrar a distribuicao de Poisson, superdispersao e analise de dados
#' categoricos.
#'
#' @format Um `data.frame` com 120 linhas e 4 variaveis:
#' \describe{
#'   \item{defeitos}{Numero de defeitos no lote (contagem).}
#'   \item{turno}{Turno de producao (`manha`, `tarde`, `noite`).}
#'   \item{maquina}{Maquina (`A`, `B`, `C`).}
#'   \item{tamanho_lote}{Tamanho do lote (100, 200 ou 500 pecas).}
#' }
#' @details Dados simulados para fins didaticos; ver `data-raw/datasets.R`.
#' @examples
#' rnp_glm(defeitos ~ turno + maquina, data = rnp_defeitos, familia = "poisson")$coeficientes
"rnp_defeitos"

#' Tempo de vida de componentes (didatico)
#'
#' Dados **simulados** de tempo ate a falha de componentes, com censura a
#' direita, para ilustrar analise de sobrevivencia e confiabilidade.
#'
#' @format Um `data.frame` com 150 linhas e 4 variaveis:
#' \describe{
#'   \item{tempo}{Tempo observado ate a falha ou censura (horas).}
#'   \item{evento}{Indicador: 1 = falhou; 0 = censurado.}
#'   \item{fornecedor}{Fornecedor do componente (`Nacional`, `Importado`).}
#'   \item{temperatura}{Temperatura de operacao (graus Celsius).}
#' }
#' @details Dados simulados para fins didaticos; ver `data-raw/datasets.R`.
#' @examples
#' rnp_kaplan_meier(rnp_vida_util$tempo, rnp_vida_util$evento,
#'                  grupo = rnp_vida_util$fornecedor)$mediana
"rnp_vida_util"
