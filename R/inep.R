#' Download dados do INEP
#'
#' Baixa microdados do censo do ensino superior do INEP.
#'
#' @param ano Ano (1995-2023).
#' @param url URL customizada (sobrescreve ano).
#' @param salvar Caminho de destino.
#'
#' @return Arquivo .zip baixado.
#'
#' @examples
#' \dontrun{
#' rnp_get_inep_censo(ano = 2020)
#' }
#' @export
rnp_get_inep_censo <- function(ano = 2020, url = NULL, salvar = NULL){
  fn_aux <- function(url, file){
    utils::download.file(url = url, destfile = file, mode = "wb", quiet = FALSE)
    invisible(file)
  }
  s <- seq(1995, 2023, by = 1)
  if(is.null(ano) & is.null(url)){
    cat("Baixando dados do censo de 2020!\n")
    anos <- 2020
    nm <- paste0("base_", anos, ".zip")
  } else if(is.null(ano) & !is.null(url)){
    cat("Ignorando ano, pois voce passou urls.\n")
    anos <- NULL
    nm <- sapply(url, function(i){
      o <- unlist(stringr::str_split(i, pattern = "\\/"))
      unname(o[length(o)])
    })
  } else if(!is.null(ano) & is.null(url)) {
    if(!ano %in% s) {
      rlang::abort("Ano deve estar entre 1995 e 2023 ou passe uma url valida.")
    } else {
      anos <- ano
      nm <- paste0("base_", anos, ".zip")
    }
  } else if(!is.null(ano) & !is.null(url)){
    cat("Ignorando ano, pois voce passou urls.\n")
    anos <- NULL
    nm <- sapply(url, function(i){
      o <- unlist(stringr::str_split(i, pattern = "\\/"))
      unname(o[length(o)])
    })
  } else {
    anos <- ano
    nm <- paste0("dados_", anos, ".zip")
  }
  url_base <- 'http://download.inep.gov.br/microdados/'
  urls_inep <- c(
    paste0(url_base, 'micro_censo_edu_superior', 1995:2003, '.zip'),
    paste0(url_base, 'microdados_censo_educacao_superior_', 2004:2006, '.zip'),
    paste0(url_base, 'microdados_educacao_superior_', 2007, '.zip'),
    paste0(url_base, 'micro_censo_edu_superior', 2008, '.zip'),
    paste0(url_base, 'microdados_censo_superior_', 2009:2016, '.zip'),
    paste0(url_base, 'microdados_educacao_superior_', 2017, '.zip'),
    paste0(url_base, 'microdados_educacao_superior_', 2018:2023, '.zip')
  )
  inep_url <- data.frame(url_in = urls_inep,
                         ano_in = as.integer(stringr::str_extract(urls_inep, "[0-9]{4}")),
                         stringsAsFactors = FALSE)
  get_url <- if(!is.null(url)) {
    url
  } else {
    inep_url %>%
      dplyr::filter(ano_in %in% anos) %>%
      dplyr::pull(url_in)
  }
  lapply(seq_along(get_url), function(i) {
    fn_aux(url = get_url[[i]], file = ifelse(is.null(salvar), nm[[i]], paste0(salvar, nm[[i]])))
  })
}

#' Obtem classes das variaveis do dicionario INEP
#'
#' Extrai categorias e descricoes do dicionario de dados do INEP.
#'
#' @param caminho Caminho do arquivo Excel.
#' @param aba Nome ou numero da aba.
#' @param pula_linha Linhas a pular.
#' @param retorna_lista Logico.
#'
#' @return data.frame.
#'
#' @examples
#' \dontrun{
#' rnp_get_classes_inep("Dicionario_de_Dados.xlsx", aba = "DM_IES")
#' }
#' @export
rnp_get_classes_inep <- function(caminho, aba = 1, pula_linha = 1, retorna_lista = FALSE){
  precisa_pacote("readxl", "rnp_get_classes_inep")
  dic <- readxl::read_excel(path = caminho, sheet = aba, col_names = TRUE,
                            skip = pula_linha, trim_ws = TRUE) %>%
    dplyr::select(1:7)
  tp <- dic %>%
    dplyr::select(1:6) %>%
    dplyr::filter(!is.na(.data$ORD)) %>%
    dplyr::filter(!is.na(.data$`NOME DA VARIAVEL`)) %>%
    rlang::set_names(c("ORDEM","VAR_NOME","VAR_DESCRICAO","VAR_TIPO","VAR_TAMANHO","VAR_DESCRICAO_CATEGORIAS")) %>%
    dplyr::mutate(VAR_DESCRICAO_CATEGORIAS_FIX = stringr::str_replace_all(.data$VAR_DESCRICAO_CATEGORIAS,
                                                                          pattern = "\\r|\\n",
                                                                          replacement = "|"))
  li <- lapply(split(tp, tp$VAR_NOME), function(i) {
    temp <- i %>% dplyr::select(.data$ORDEM, .data$VAR_NOME, .data$VAR_DESCRICAO_CATEGORIAS_FIX)
    o <- if(nrow(temp) == 0) {
      data.frame(ORDEM = as.numeric(temp$ORDEM), VAR_NOME = temp$VAR_NOME,
                 VAR_CATEGORIA = as.numeric(0), VAR_DESCRICAO = as.character(0))
    } else {
      va <- stringr::str_squish(unlist(stringr::str_split(temp$VAR_DESCRICAO_CATEGORIAS_FIX, "\\|\\|")))
      de <- stringr::str_extract(va, "^[0-9]+")
      data.frame(ORDEM = as.numeric(temp$ORDEM), VAR_NOME = temp$VAR_NOME,
                 VAR_CATEGORIA = as.numeric(de), VAR_DESCRICAO = as.character(va))
    }
    return(o)
  })
  oo <- if(retorna_lista) {
    li
  } else {
    suppressWarnings(dplyr::bind_rows(li) %>% dplyr::arrange(.data$ORDEM))
  }
  return(oo)
}

#' Aplica classes do dicionario INEP na base
#'
#' Junta descricoes das categorias com os codigos numericos.
#'
#' @param base Base de dados INEP.
#' @param classes Base de classes (de [rnp_get_classes_inep()]).
#'
#' @return data.frame com colunas _DESC adicionadas.
#'
#' @examples
#' \dontrun{
#' classes <- rnp_get_classes_inep("Dicionario.xlsx", aba = "DM_IES")
#' base <- rnp_read("DM_IES.csv", sep = "|")
#' base_desc <- rnp_aplica_classes(base, classes)
#' }
#' @export
rnp_aplica_classes <- function(base, classes){
  nm_da <- colnames(base)
  nm_cl <- unname(sapply(split(classes, classes$VAR_NOME), function(i){
    tmp <- if(nrow(i) == 1) NA else unique(i$VAR_NOME)
    tmp
  }))
  nm_cl <- as.character(na.exclude(nm_cl))
  lda <- lapply(nm_cl, function(i, ...){
    va <- base %>% dplyr::select(dplyr::all_of(i))
    cl <- classes %>%
      dplyr::filter(.data$VAR_NOME == i) %>%
      dplyr::transmute(.data$VAR_CATEGORIA, .data$VAR_DESCRICAO) %>%
      rlang::set_names(c(colnames(va)[1], paste0("DESC_",colnames(va)[1])))
    va %>% dplyr::left_join(cl, by = i)
  })
  out <- dplyr::bind_cols(base[,!colnames(base) %in% nm_cl],
                          dplyr::bind_cols(lda))
  return(out)
}
