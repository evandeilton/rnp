#' Tabela de frequências
#' @param x vetor numérico ou caractere de entrada
#' @param sortd  TRUE para ordenar
#' @param digits Número de dígitos para arredondar os decimais
#' @details Calcula tabela de freência simples. Quando o vetor passado for caractere ou fator, a função trabalha com as frequências
#' e quando é passado um vetor numérico, ela utiliza função \code{\link{cut}} com apoio de  \code{\link{quantile}} para categorizar
#' os dados utilizando os quartis.
#' @return data.frame
#' @author LOPES, J. E
#' @export
rnp_freq <- function(x, sortd = FALSE, digits = 4){
  if (missing(x)){
    stop("Informe o vetor x!\n")
  } else if(!is.null(dim(x))){
    stop("x devem ser vetor!\n")
  }

  if(is.numeric(x)){
    #cat("Variavel numerica, utilizando a funcao quantile() para faixas.\n")
    quebras = unique(round(quantile(x), 2))
    t1 <- table(cut(x, breaks = quebras, include.lowest = TRUE))
    #names(t1) <- paste(names(quebras), names(t1), sep = "_")
  } else {
    if(sortd){
      t1 <- sort(table(x), decreasing = TRUE)
    } else {
      t1 <- table(x)
    }
  }

  t2 <- data.frame(classe = names(t1),
                   Fa = as.numeric(unname(t1)))
  t2$Fr  <- round(t2$Fa / sum(t2$Fa), digits = digits)
  t2$Faa <- round(cumsum(t2$Fa), digits = digits)
  t2$Fra <- round(t2$Faa / sum(t2$Fa), digits = digits)
  colnames(t2) <- c("classe","fa","fr","Faa", "Fra")
  t2$classe <- gsub(pattern = "\\[|\\]|\\(|\\)", replacement = "", x = t2$classe)
  t2$classe <- gsub(pattern = ",", replacement = "--", x = t2$classe)

  return(t2)
}

#' Tabela de frequências dupla entrada
#' @param x vetor caractere/fator de entrada
#' @param y vetor caractere/fator de entrada
#' @param digits total de digitos decimais na saida
#' @param percents se TRUE, retorna também os percentuais de linha e de colunas.
#' @details faz tabela de dupla entrada com base em dois vetores de entrada.
#' @return data.frame
#' @author LOPES, J. E
#' @export
rnp_2freq <- function(x, y, digits = 4, percents = FALSE){
  if(missing(x) & missing(y)) {
    stop("Informe x e y!\n")
  } else if(missing(y)) {
    if(!is.null(dim(x))) {
      stop("x devem ser vetor!\n")
    }
    tt <- table(x, deparse.level = 2)
  } else if (missing(x)){
    if(!is.null(dim(x))) {
      stop("y devem ser vetor!\n")
    }
    tt <- table(y, deparse.level = 2)
  } else {
    if(!is.null(dim(x)) | !is.null(dim(y))) {
      stop("x e y devem ser vetor!\n")
    }
    tt <- table(x, y, deparse.level = 2)
  }

  t1 <- addmargins(tt)
  t1 <- as.matrix(t1)
  t2 <- t1/max(t1)

  li <- matrix(NA, nrow = nrow(t1), ncol = ncol(t1))
  for(i in 1:nrow(t1)){
    li[i, ] <- t1[i,] / max(t1[i,])
  }

  co <- matrix(NA, nrow = nrow(t1), ncol = ncol(t1))
  for(j in 1:ncol(t1)){
    co[,j] <- t1[,j] / max(t1[,j])
  }

  colnames(li) <- colnames(co) <- colnames(t1)
  rownames(li) <- rownames(co) <- rownames(t1)

  if(percents){
    o <- rbind(data.frame(cbind(Tipo = "fa", Classe = as.character(rownames(t1)), as.matrix(t1)),
                          check.names = FALSE, stringsAsFactors = FALSE),
               data.frame(cbind(Tipo = "fr", Classe = as.character(rownames(t2)), round(as.matrix(t2), digits = digits)),
                          check.names = FALSE, stringsAsFactors = FALSE),
               data.frame(cbind(Tipo = "fr_lin", Classe = as.character(rownames(li)), round(as.matrix(li), digits = digits)),
                          check.names = FALSE, stringsAsFactors = FALSE),
               data.frame(cbind(Tipo = "fr_col", Classe = as.character(rownames(co)), round(as.matrix(co), digits = digits)),
                          check.names = FALSE, stringsAsFactors = FALSE)
			  )
  } else {
    o <- rbind(data.frame(cbind(Tipo = "fa", Classe = as.character(rownames(t1)), round(as.matrix(t1), digits = digits)),
                          check.names = FALSE), stringsAsFactors = FALSE)
  }
  rownames(o) <- NULL
  o$Classe <- as.character(o$Classe)
  o[which(o$Classe == "Sum"), ]$Classe <- "Total"
  colnames(o)[which(colnames(o) == "Sum")] <- "Total"

  O <- data.frame(sapply(o[,1:2], function(x) as.character(x)),
                  sapply(o[,-c(1:2)], function(x) as.numeric(as.character(x))),
                  check.names = FALSE,
                  stringsAsFactors = FALSE)

  if(percents) {
	O <- O[order(O$Classe, O$Tipo), ]
	out <- O[!(O$Classe == "Total" & O$Tipo %in% c("fr_col","fr_lin")), ]
  } else {
	out <- O
  }
  rownames(out) <- NULL
  colnames(out)[2] <- "Classe X/Y"
  return(out)
}

#' Sumário estatístico
#' @param x vetor numérico de entrada
#' @param digits total de digitos decimais na saida
#' @details Calcula estatísticas descritivas de um vetor numérico informado
#' @return Vetor nomeado
#' @author LOPES, J. E
#' @export
rnp_summary <- function(x, digits = 4) {
  o <- c(N        = length(x),
         Soma     = sum(x, na.rm = TRUE),
         Nmis     = sum(is.na(x)),
         Min      = min(x),
         Q1       = unname(quantile(x, probs = 0.25)),
         Media    = mean(x),
         Mediana  = median(x),
         Q3       = unname(quantile(x, probs = 0.75)),
         Max      = max(x),
         DevPad   = sd(x),
         #IQR      = IQR(x),
         cv       = sd(x)/mean(x))
  return(round(o, digits = digits))
}

#' Sumário estatístico geral
#' @param base data.frame de entrada
#' @details Calcula estatísticas descritivas para todos os vetores numpericos de uma base de dados.
#' Se tiver variáveis categóricas, a função os ignora.
#' @return data.frame com as estatísticas por variáveis.
#' @author LOPES, J. E
#' @export
rnp_summary_all <- function(base){
  cl <- sapply(base, function(x) class(x)[1])
  nu <- cl[which(cl %in% c("numeric","integer"))]
  sa <- sapply(base[,names(nu)], function(x) rnp_summary(x))
  sa <- as.data.frame(sa)
  return(sa)
}


#' Leitura rápida de bases de dados
#' @param base Local e nome com a extensão da base de dados de entrada
#' @param sep Separador de colunas
#' @param dec Separador decimal
#' @param encoding Codificação dos dados, padrão é Latin-1. UTF-8 também é muito útil
#' @param nrows Número de linhas a extrair. Se Inf, trás todas
#' @param verbose Se TRUE, exibe logs do que está aocontecendo em tempo de extração
#' @param showProgress Se TRUE, exibe o andamento do processo de leitura
#' @param ... Passagem de argumentos ectras para a função \code{\link{fread}}
#' @return tibla com os dados importados
#' @author LOPES, J. E
#' @importFrom data.table fread
#' @importFrom dplyr as_tibble
#' @export
rnp_read <- function(base, sep = "|", dec = ".", encoding = "Latin-1", nrows = Inf,
                     verbose = TRUE, showProgress = TRUE, ...) {
  data.table::fread(input = base, sep = sep, dec = dec, encoding = encoding, header = TRUE,
                    stringsAsFactors = FALSE, verbose = verbose, showProgress = showProgress) %>%
    dplyr::as_tibble()
}

#' Extrai atributos de um objeto
#' @param obj qualquer objeto em R
#' @return data.frame com lista de atributos
#' @author LOPES, J. E
#' @export
rnp_atributos <- function(obj) {
  o <- data.frame(classeBase  = paste0(class(obj)[1], collapse = "|"),
                  comprimento = if (is.null(dim(obj))) length(obj) else paste(dim(obj)[1], "linhas e", dim(obj)[2], "colunas"),
                  variaveis   =   if (is.null(dim(obj))) 0 else paste0(colnames(obj)),
                  classeVars  = if (is.null(dim(obj))) 0 else sapply(obj, class)
                  )
  rownames(o) <- NULL
  return(o)
}


rnp_freq2 <- function(x, y, digits = 4, type = c("n","pct"), chisqt = FALSE){
  match.arg(type, c("N","n","pct","PCT"), several.ok = TRUE)
  type <- unique(tolower(type))
  if(missing(x) & missing(y)) {
    stop("Informe pelo menos um vetor, x ou y!")
  } else if(!missing(x) & missing(y)){
    out <- rnp_freq(x, digits = digits)
  } else if(missing(x) & !missing(y)){
    out <- rnp_freq(y, digits = digits)
  } else if(!missing(x) & !missing(y)) {
    t1 <- table(x, y)
    if(chisqt) {
      ch <- chisq.test(t1, simulate.p.value = TRUE, B = 500)
    }

    out <- lapply(type, function(i) {
      if(i == "n") {
        t2 <- addmargins(t1)
      } else if(i == "pct") {
        t2 <- round(addmargins(prop.table(t1)), digits = digits)
      }
      t3 <- as.data.frame.matrix(t2)
      t3 <- data.frame("Classes x / y" = rownames(t3),
                       Tipo = ifelse(i == "n", "fa", "fr"),
                       t3, stringsAsFactors = FALSE, check.names = FALSE)
      rownames(t3) <- NULL
      t3$`Classes x / y`[length(t3$`Classes x / y`)] <- "Total"
      colnames(t3)[length(colnames(t3))] <- "Total"
      return(t3)
    })
    out <- do.call("rbind", out)
  }
  return(out)
}




#' Aplica classes na base do INEP correspondente
#' @description Esta função recebe como entrada a base de dados do censo do INEP,
#' podendo ser "DM_CURSO","DM_IES","DM_LOCAL_OFERTA","DM_DOCENTE" ou "DM_ALUNO" e
#' sua base de classes extraidas com a função rnp_get_classes_inep() e aplica
#' na base apenas para as variáveis que possuem descrição de classes.
#' @details Se uma variável não possui descrição de classe então e ala não é processada
#' o data.frame de saída possui novas colunas com prefixo "_DESC" que possui a descrição
#' da categoria e ao lado a variável original
#' @param base base de dados censo INEP
#' @param classes base de dados com informações das classes obtidas do dicionário de dados do INEP.
#' @author LOPES, J. E.
#' @import dplyr
#' @importFrom magrittr set_colnames
#' @examples
#' \dontrun{
#' nn <- c("DM_CURSO","DM_IES","DM_LOCAL_OFERTA","DM_DOCENTE")
#' L <- plyr::llply(nn[1], function(base){
#'   classes <- rnp_get_classes_inep(caminho = "Dados/AJUDA/ANEXOS/ANEXO I - Dicionário de Dados e Tabelas Auxiliares/Dicionário_de_Dados.xlsx",
#'                                   aba = base, retorna_lista = FALSE)
#'   base <- rnp_read(base = paste0("Dados/CSV/", base, ".CSV"),
#'                    sep = "|",
#'                    dec = ".",
#'                    header = TRUE,
#'                    encoding = "Latin-1",
#'                    verbose = FALSE,
#'                    showProgress = FALSE)
#'   oo <- rnp_aplica_classes(base = base, classes = classes)
#'   return(oo)
#' }, .progress = "text")
#' names(L) <- nn
#' }
#' @export
rnp_aplica_classes <- function(base, classes){
  # Seleciona nomes da base de dados. Vamos tratar apenas os nomes que das
  # variaveis que possuem descrição de classes
  nm_da <- colnames(base)

  # Separando apenas as variáveis que possuem categorias com descrição.
  # na base de classes
  nm_cl <- unname(sapply(split(classes, classes$VAR_NOME), function(i){
    tmp <- if(nrow(i) == 1) {
      NA
    } else {
      unique(i$VAR_NOME)
    }
    tmp
  }))
  nm_cl <- as.character(na.exclude(nm_cl))

  # Criando novas variáveis com as descrições das categorias

  lda <- lapply(nm_cl, function(i, ...){
    va <- base %>%
      dplyr::select(i)

    cl <- classes %>%
      dplyr::filter(VAR_NOME == i) %>%
      dplyr::transmute(VAR_CATEGORIA, VAR_DESCRICAO) %>%
      magrittr::set_colnames(., c(colnames(va)[1], paste0("DESC_",colnames(va)[1])))

    va <- va %>%
      dplyr::left_join(cl, by = i)
  })

  out <- dplyr::bind_cols(base[,!colnames(base)%in%nm_cl],
                          dplyr::bind_cols(lda))
  return(out)
}

#' Obtem as classes das variáveis censo INEP
#' @description
#' Obtem as classes das variáveis do diconário de dados do censo INEP.
#' @details
#' Exige que passe o caminho para o dicionário de dados em excel e o nome da
#' aba correspondente ou o número da mesma, ex. 1 = primeira aba e assim por diante.
#' A função faz a leitura dos dados utilizando readxl::read_excel() e retorna
#' um data.frame com 4 aolunas contendo ordem, nome, categoria e descrição
#' para todas as variáveis da base informada.
#' OBS.: Testado apenas nos dados do censo de 2017.
#' @param caminho caminho do arquivo Excel do dicionário de dados do INEP
#' @param aba aba da planilha correspondente aos dados que deseja
#' @param pula_linha quantidade de linhas que deseja pular
#' @param retorna_lista TRUE se quer obter uma lista de data.frames, sendo
#' um para cada variável ou base já agregada.
#' @author LOPES, J. E.
#' @import dplyr
#' @importFrom magrittr set_colnames
#' @examples
#' \dontrun{
#' nn <- c("DM_CURSO","DM_IES","DM_LOCAL_OFERTA","DM_DOCENTE")
#' L <- plyr::llply(nn[1], function(base){
#'   classes <- rnp_get_classes_inep(caminho = "Dados/AJUDA/ANEXOS/ANEXO I - Dicionário de Dados e Tabelas Auxiliares/Dicionário_de_Dados.xlsx",
#'                                   aba = base, retorna_lista = FALSE)
#'   base <- rnp_read(base = paste0("Dados/CSV/", base, ".CSV"),
#'                    sep = "|",
#'                    dec = ".",
#'                    header = TRUE,
#'                    encoding = "Latin-1",
#'                    verbose = FALSE,
#'                    showProgress = FALSE)
#'   oo <- rnp_aplica_classes(base = base, classes = classes)
#'   return(oo)
#' }, .progress = "text")
#' names(L) <- nn
#' }
#' @export
rnp_get_classes_inep <- function(caminho, aba = 1, pula_linha = 1, retorna_lista = FALSE){
  # Dicionário de dados
    dic_dm_aluno <- readxl::read_excel(path = caminho,
                                       sheet = aba, col_names = TRUE, skip = pula_linha, trim_ws = TRUE) %>%
      select(1:7)
  # Seleciona colunas de interesse e arruma as
  tp <- dic_dm_aluno %>%
    dplyr::select(1:6) %>%
    dplyr::filter(!is.na(`ORD`)) %>%
    dplyr::filter(!is.na(`NOME DA VARIÁVEL`)) %>%
    magrittr::set_names(., c("ORDEM","VAR_NOME","VAR_DESCRICAO","VAR_TIPO","VAR_TAMANHO","VAR_DESCRICAO_CATEGORIAS")) %>%
    dplyr::mutate(VAR_DESCRICAO_CATEGORIAS_FIX = stringr::str_replace_all(VAR_DESCRICAO_CATEGORIAS, pattern = "\\r|\\n", replacement = "|"))

  li <- lapply(split(tp, tp$VAR_NOME), function (i) {
    temp <- i %>%
      dplyr::select(ORDEM, VAR_NOME, VAR_DESCRICAO_CATEGORIAS_FIX)

    o <- if(nrow(temp) == 0) {
      data.frame(ORDEM = as.numeric(temp$ORDEM), VAR_NOME = temp$VAR_NOME, VAR_CATEGORIA = as.numeric(0), VAR_DESCRICAO = as.character(0))
    } else {
      va <- stringr::str_squish(unlist(stringr::str_split(temp$VAR_DESCRICAO_CATEGORIAS_FIX, "\\|\\|")))
      de <- stringr::str_extract(va, "^[0-9]+")
      data.frame(ORDEM = as.numeric(temp$ORDEM), VAR_NOME = temp$VAR_NOME, VAR_CATEGORIA = as.numeric(de), VAR_DESCRICAO = as.character(va))
    }
    return(o)
  })
  oo <- if(retorna_lista) {
    li
  } else {
    suppressWarnings(dplyr::bind_rows(li) %>%
                       dplyr::arrange(ORDEM))
  }
  return(oo)
}


#' Trata erros
#' @description Trata qualquer erro em chamadas de funções. Se der erro, sai um objeto da classe 'try-error'
#' @param code Qualquer código R passado
#' @param silent Rodar silenciosamente? TRUE ou FALSE
#' @return Objeto de entrada
#' @author LOPES, J. E
#' @export
rnp_try_error <- function(code, silent = TRUE) {
  W <- NULL
  w.handler <- function(w) {
    W <<- w
    invokeRestart("muffleWarning")
  }
  withCallingHandlers(tryCatch(code, error = function(c) {
    msg <- conditionMessage(c)
    if (!silent)
      message(c)
    invisible(structure(msg, class = "try-error"))
  }), warning = w.handler)
}


#' Download dados do INEP
#' @description Recebe uma data ou uma url do portal de microdados do INEP e
#' baixa os dados no local em que a sessão do R foi carregada.
#' @details  Quando passado apenas uma data a função baixa os dados do INEP
#' da data correspondente para o censo de educação superior, desde que esta
#' data seja entre 1995 e 2017. Caso a data esteja fora deste intervalo, a função
#' baixará os dados do censo de 2017.
#' Se uma url com final .zip for passada, a função ignora a data, caso tenha
#' sido informada e baixa o arquivo da url informada.
#' @param ano ano formato numérico, ex. 2010, 2016
#' @param url url, vetor ou lista de urls completas para baixar os dados.
#' @param salvar caminho onde deseja salvar os dados baixados. Se NULL, a função
#' baixa na pasta onde a sessão ativa do R foi iniciada. Execute \code{\link{getwd}} para saber o local.
#' @return arquivo .zip ou da extenção da url passada.
#' @examples
#' \dontrun{
#' require(rnp)
#' require(dplyr)
#' url_in = 'http://download.inep.gov.br/microdados/micro_censo_edu_superior1995.zip'
#' rnp_get_inep_censo(url = url_in)
#' rnp_get_inep_censo()
#' }
#' @author LOPES, J. E.
#' @import dplyr RCurl
#' @importFrom stringr str_extract
#' @export
rnp_get_inep_censo <- function(ano = 2017, url = NULL, salvar = NULL){
  fn_aux <- function(url, file){
    f = RCurl::CFILE(file, mode="wb")
    a = RCurl::curlPerform(url = url, writedata = f@ref, noprogress=FALSE)
    RCurl::close(f)
    return(a)
  }
  s <- seq(1995, 2017, by = 1)
  if(is.null(ano) & is.null(url)){
    cat("Baixando dados do censo de 2017!\n")
    anos <- max(s)
    nm <- paste0("base_", anos, ".zip")
  } else if(is.null(ano) & !is.null(url)){
    cat("Ignorando a data, pois você passou urls.\n")
    anos <- NULL
    nm <- sapply(url, function(i){
      o <- unlist(stringr::str_split(i, pattern = "\\/"))
      unname(o[length(o)])
    })
  } else if(!is.null(ano) & is.null(url)) {
    if(!ano %in% s) {
      stop("Passe valores para anos entre 1995 e 2017 ou uma url do arquivo .zip válida.\n")
    } else {
      anos <- ano
      nm <- paste0("base_", anos, ".zip")
    }
  } else if(!is.null(ano) & !is.null(url)){
    cat("Ignorando a data, pois você passou urls.\n")
    anos <- NULL
    nm <- sapply(url, function(i){
      o <- unlist(stringr::str_split(i, pattern = "\\/"))
      unname(o[length(o)])
    })
  } else {
    anos <- ano
    nm <- paste0("dados_", anos, ".zip")
  }
  url_in <- c('http://download.inep.gov.br/microdados/micro_censo_edu_superior1995.zip',
              'http://download.inep.gov.br/microdados/micro_censo_edu_superior1996.zip',
              'http://download.inep.gov.br/microdados/micro_censo_edu_superior1997.zip',
              'http://download.inep.gov.br/microdados/micro_censo_edu_superior1998.zip',
              'http://download.inep.gov.br/microdados/micro_censo_edu_superior1999.zip',
              'http://download.inep.gov.br/microdados/micro_censo_edu_superior2000.zip',
              'http://download.inep.gov.br/microdados/micro_censo_edu_superior2001.zip',
              'http://download.inep.gov.br/microdados/micro_censo_edu_superior2002.zip',
              'http://download.inep.gov.br/microdados/micro_censo_edu_superior2003.zip',
              'http://download.inep.gov.br/microdados/microdados_censo_educacao_superior_2004.zip',
              'http://download.inep.gov.br/microdados/microdados_censo_educacao_superior_2005.zip',
              'http://download.inep.gov.br/microdados/microdados_educacao_superior_2006.zip',
              'http://download.inep.gov.br/microdados/microdados_educacao_superior_2007.zip',
              'http://download.inep.gov.br/microdados/micro_censo_edu_superior2008.zip',
              'http://download.inep.gov.br/microdados/microdados_censo_superior_2009.zip',
              'http://download.inep.gov.br/microdados/microdados_censo_superior_2010.zip',
              'http://download.inep.gov.br/microdados/microdados_censo_superior_2011.zip',
              'http://download.inep.gov.br/microdados/microdados_censo_superior_2012.zip',
              'http://download.inep.gov.br/microdados/microdados_censo_superior_2013.zip',
              'http://download.inep.gov.br/microdados/microdados_censo_superior_2014.zip',
              'http://download.inep.gov.br/microdados/microdados_censo_superior_2015.zip',
              'http://download.inep.gov.br/microdados/microdados_censo_superior_2016.zip',
              'http://download.inep.gov.br/microdados/microdados_educacao_superior_2017.zip')

  inep_url <- data.frame(url_in = as.character(url_in),
                         ano_in = as.numeric(stringr::str_extract(url_in, "[0-9]{4}")), stringsAsFactors = FALSE)

  get_url <- if(!is.null(url)) {
    url
  } else {
    inep_url %>%
      dplyr::filter(ano_in %in% anos) %>%
      dplyr::select(url_in) %>%
      unlist() %>%
      unname()
  }
  lapply(seq_along(get_url), function(i) {
    fn_aux(url = get_url[[i]], file = ifelse(is.null(salvar), nm[[i]], paste0(salvar, nm[[i]])))
  })
}

#' Estatísticas descritivas por grupo
#' @description
#' Calcula estatísticas descritivas por grupo. Ela recebe como entrada um data.frame
#' o nome da variável numérica e um vetor ou lista de nomes das variáveis que serão utilizadas
#' como grupos. A função trabalha com apoio da função \code{\link{ddply}} e aceita muitos
#' grupos.
#' @param base data.frame com as variáveis de entrada
#' @param variavel o nome da variável numérica entre aspas
#' @param grupos lista ou vetor de nomes das estatísticas de agrupamento na ordem em que deseja
#' obter os resultados.
#' @param digits total de digitos para arredondar os decimais.
#' @return
#' As estatísticas de saída desta função são: total (N), soma, número de missing (Nmis),
#' minimo, primeiro quartil (Q1), mediana (Q2), terceiro quartil (Q3), máximo, desvio pacrão (devpad)
#' e coeficiente de variação (cv) em um data.frame agrupadas conforme as classes das variáveis de
#' agrupamento.
#' @author LOPES, J. E.
#' @examples
#' rnp_summary_by(base = mtcars, variavel = "wt", grupos = "gear")
#' rnp_summary_by(base = mtcars, variavel = "wt", grupos = c("gear","cyl"))
#' rnp_summary_by(base = mtcars, variavel = "wt",
#'                grupos = list("gear","cyl"), digits = 2)
#' @importFrom plyr ddply
#' @export
rnp_summary_by <- function(base, variavel, grupos, digits = 3) {
  variavel <- if(length(variavel) > 1){
    stop("Informe apenas uma variavel numerica")
  } else as.character(variavel)
  if(length(grupos) > 10){
    cat("Cuidado, muitos grupos podem gerar dados confusos!")
  }
  grupos <- as.character(unlist(grupos))
  out <- plyr::ddply(.data = base,
                     .variables = grupos,
                     .fun = function(xx){
                       rnp::rnp_summary(xx[,variavel], digits = digits)
                     })
  return(out)
}

#' Dados do censo ensino superior INEP 2017 (Cursos)
#' @description
#' Este conjunto de dados faz parte da base de dados do INEP para o censo
#' brasileiro do ensino superior. Nesta parte temos os dados dos cursos
#' mapeados pelo censo naquele ano.
#' Para obter dados de outros anos consulte o link do site do INEP ou
#' a função \code{\link{rnp_get_inep_censo}}
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
#' @description
#' Este conjunto de dados faz parte da base de dados do INEP para o censo
#' brasileiro do ensino superior. Nesta parte temos os dados das IES
#' Instituições de Ensino Superior mapeados pelo censo naquele ano.
#' Para obter dados de outros anos consulte o link do site do INEP ou
#' a função \code{\link{rnp_get_inep_censo}}
#' @name dm_ies
#' @docType data
#' @author INEP
#' @references \url{http://inep.gov.br/microdados}
#' @keywords dm_ies
#' @examples
#' require(rnp)
#' str(dm_ies)
NULL
