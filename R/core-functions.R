#' Tabela de frequencias
#' @param x vetor numerico ou caractere de entrada
#' @param sortd  TRUE para ordenar
#' @param digits Numero de digitos para arredondar os decimais
#' @details Calcula tabela de freencia simples. Quando o vetor passado for caractere ou fator, a funcao trabalha com as frequencias
#' e quando e passado um vetor numerico, ela utiliza funcao \code{\link{cut}} com apoio de  \code{\link{quantile}} para categorizar
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
    quebras = unique(round(quantile(x, na.rm = TRUE), 2))
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

#' Tabela de frequencias dupla entrada
#' @param x vetor caractere/fator de entrada
#' @param y vetor caractere/fator de entrada
#' @param digits total de digitos decimais na saida
#' @param percents se TRUE, retorna tambem os percentuais de linha e de colunas.
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

#' Sumario estatistico
#' @param x vetor numerico de entrada
#' @param digits total de digitos decimais na saida
#' @details Calcula estatisticas descritivas de um vetor numerico informado
#' @return Vetor nomeado
#' @author LOPES, J. E
#' @export
rnp_summary <- function(x, digits = 4) {
  o <- c(N        = length(x),
         Soma     = sum(x, na.rm = TRUE),
         Nmis     = sum(is.na(x)),
         Min      = min(x, na.rm = TRUE),
         Q1       = unname(quantile(x, probs = 0.25, na.rm = TRUE)),
         Media    = mean(x, na.rm = TRUE),
         Mediana  = median(x, na.rm = TRUE),
         Q3       = unname(quantile(x, probs = 0.75, na.rm = TRUE)),
         Max      = max(x, na.rm = TRUE),
         DevPad   = sd(x, na.rm = TRUE),
         IQR      = IQR(x, na.rm = TRUE),
         cv       = sd(x, na.rm = TRUE)/mean(x, na.rm = TRUE))
  return(round(o, digits = digits))
}

#' Sumario estatistico geral
#' @param base data.frame de entrada
#' @details Calcula estatisticas descritivas para todos os vetores numpericos de uma base de dados.
#' Se tiver variaveis categoricas, a funcao os ignora.
#' @return data.frame com as estatisticas por variaveis.
#' @author LOPES, J. E
#' @export
rnp_summary_all <- function(base){
  cl <- sapply(base, function(x) class(x)[1])
  nu <- cl[which( cl %in% c("numeric","integer","ts","xts","mts"))]
  ca <- cl[which(!cl %in% c("numeric","integer","ts","xts","mts"))]
  p1 <- p2 <- NULL
  if(length(cl) > 0){
    p1 <- do.call("rbind",
                  lapply(names(nu), function(i) {
                    data.frame(variavel = i, t(rnp_summary(base[,i])), stringsAsFactors = FALSE)
                  }))
  }
  if(length(ca) > 0){
    p2 <- do.call("rbind",
                  lapply(names(ca), FUN = function(i) {
                    data.frame(variavel = i, rnp_freq(base[,i]), stringsAsFactors = FALSE)
                    }))
  }
  return(list(num = p1, cat = p2))
}


#' Leitura rapida de bases de dados
#' @param base Local e nome com a extensao da base de dados de entrada
#' @param sep Separador de colunas
#' @param dec Separador decimal
#' @param encoding Codificacao dos dados, padrao e Latin-1. UTF-8 tambem e muito util
#' @param nrows Numero de linhas a extrair. Se Inf, tras todas
#' @param verbose Se TRUE, exibe logs do que esta aocontecendo em tempo de extracao
#' @param showProgress Se TRUE, exibe o andamento do processo de leitura
#' @param select Uma lista de variaveis de interesse pode ser passada para simplificar a saida.
#' @param ... Passagem de argumentos ectras para a funcao \code{\link{fread}}
#' @return tibla com os dados importados
#' @author LOPES, J. E
#' @export
rnp_read <- function(base, sep = "|", dec = ".", encoding = "Latin-1", nrows = Inf,
                     verbose = TRUE, showProgress = TRUE, select = NULL, ...) {
  data.table::fread(input = base, sep = sep, dec = dec, encoding = encoding, header = TRUE, nrows = nrows,
                    stringsAsFactors = FALSE, verbose = verbose, showProgress = showProgress, select = select)
}

#' Extrai atributos de um objeto
#' @param obj qualquer objeto em R
#' @param ests Se TRUE, retorna estatisticas do vetor ou das variaveis
#' @return data.frame com lista de atributos
#' @author LOPES, J. E
#' @export
rnp_atributos <- function(obj, ests = FALSE) {
  o <- data.frame(classeBase  = paste0(class(obj)[1], collapse = "|"),
                  comprimento = if (is.null(dim(obj))) length(obj) else paste(dim(obj)[1], "linhas e", dim(obj)[2], "colunas"),
                  variaveis   =   if (is.null(dim(obj))) 0 else paste0(colnames(obj)),
                  classeVars  = if (is.null(dim(obj))) 0 else sapply(obj, class)
                  )
  rownames(o) <- NULL
  if(ests) {
    logico <- c("numeric","integer","ts","xts","zoo")
    p1 <- p2 <- c(variaveis = NA, Nmis = NA, Min = NA, Q1 = NA, Media = NA, Mediana = NA, Q3 = NA, Max = NA, DevPad = NA, IQR = NA, cv = NA)
    a <- if(is.null(dim(obj)) & class(obj)[1] %in% logico){
      cbind(o, t(rnp_summary(obj))[,-c(1,2)])
    } else if(is.null(dim(obj)) & !class(obj)[1] %in% logico){
      cbind(o, Nmis = sum(is.na(obj)))
    } else if (!is.null(dim(obj))){
      num <- names(obj)[ sapply(obj, class) %in% logico]
      cha <- names(obj)[!sapply(obj, class) %in% logico]
      if(length(num) > 0){
        p1  <- rnp_summary_all(obj[,num])[[1]]
        p1  <- data.frame(variaveis = names(p1), t(p1)[,-c(1,2)])
      }
      if(length(cha) > 0){
        p2  <- sapply(obj[,cha], function(i) sum(is.na(i) | i %in% c(NULL, NaN, NA, -Inf, +Inf)))
        p2  <- data.frame(variaveis = names(p2), Nmis = p2, Min = NA, Q1 = NA, Media = NA, Mediana = NA, Q3 = NA, Max = NA, DevPad = NA, IQR = NA, cv = NA)
      }
      merge(o, rbind(p1, p2), by = "variaveis")
    } else {
      o
    }
    return(a)
  } else {
    return(o)
  }
}

#' Aplica classes na base do INEP correspondente
#' @description Esta funcao recebe como entrada a base de dados do censo do INEP,
#' podendo ser "DM_CURSO","DM_IES","DM_LOCAL_OFERTA","DM_DOCENTE" ou "DM_ALUNO" e
#' sua base de classes extraidas com a funcao 'rnp_get_classes_inep' e aplica
#' na base apenas para as variaveis que possuem descricao de classes.
#' @details Se uma variavel nao possui descricao de classe entao e ala nao e processada
#' o data.frame de saida possui novas colunas com prefixo "_DESC" que possui a descricao
#' da categoria e ao lado a variavel original
#' @param base base de dados censo INEP
#' @param classes base de dados com informacoes das classes obtidas do dicionario de dados do INEP.
#' @author LOPES, J. E.
#' @examples
#' \dontrun{
#' nn <- c("DM_CURSO","DM_IES","DM_LOCAL_OFERTA","DM_DOCENTE")
#' L <- plyr::llply(nn[1], function(base){
#'   classes <- rnp_get_classes_inep(caminho = "Dicionario_de_Dados.xlsx",
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
  # variaveis que possuem descricao de classes
  nm_da <- colnames(base)

  # Separando apenas as variaveis que possuem categorias com descricao.
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

  # Criando novas variaveis com as descricoes das categorias

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

#' Obtem as classes das variaveis censo INEP
#' @description
#' Obtem as classes das variaveis do diconario de dados do censo INEP.
#' @details
#' Exige que passe o caminho para o dicionario de dados em excel e o nome da
#' aba correspondente ou o numero da mesma, ex. 1 = primeira aba e assim por diante.
#' A funcao faz a leitura dos dados utilizando 'read_excel' e retorna
#' um data.frame com 4 aolunas contendo ordem, nome, categoria e descricao
#' para todas as variaveis da base informada.
#' OBS.: Testado apenas nos dados do censo de 2017.
#' @param caminho caminho do arquivo Excel do dicionario de dados do INEP
#' @param aba aba da planilha correspondente aos dados que deseja
#' @param pula_linha quantidade de linhas que deseja pular
#' @param retorna_lista TRUE se quer obter uma lista de data.frames, sendo
#' um para cada variavel ou base ja agregada.
#' @author LOPES, J. E.
#' @examples
#' \dontrun{
#' nn <- c("DM_CURSO","DM_IES","DM_LOCAL_OFERTA","DM_DOCENTE")
#' L <- plyr::llply(nn[1], function(base){
#'   classes <- rnp_get_classes_inep(caminho = "Dicionario_de_Dados.xlsx",
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
  # Dicionario de dados
    dic_dm_aluno <- readxl::read_excel(path = caminho,
                                       sheet = aba, col_names = TRUE, skip = pula_linha, trim_ws = TRUE) %>%
      dplyr::select(1:7)
  # Seleciona colunas de interesse e arruma as
  tp <- dic_dm_aluno %>%
    dplyr::select(1:6) %>%
    dplyr::filter(!is.na("ORD")) %>%
    dplyr::filter(!is.na("NOME DA VARIAVEL")) %>%
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
#' @description Trata qualquer erro em chamadas de funcoes. Se der erro, sai um objeto da classe 'try-error'
#' @param code Qualquer codigo R passado
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
#' baixa os dados no local em que a sessao do R foi carregada.
#' @details  Quando passado apenas uma data a funcao baixa os dados do INEP
#' da data correspondente para o censo de educacao superior, desde que esta
#' data seja entre 1995 e 2017. Caso a data esteja fora deste intervalo, a funcao
#' baixara os dados do censo de 2017.
#' Se uma url com final .zip for passada, a funcao ignora a data, caso tenha
#' sido informada e baixa o arquivo da url informada.
#' @param ano ano formato numerico, ex. 2010, 2016
#' @param url url, vetor ou lista de urls completas para baixar os dados.
#' @param salvar caminho onde deseja salvar os dados baixados. Se NULL, a funcao
#' baixa na pasta onde a sessao ativa do R foi iniciada. Execute \code{\link{getwd}} para saber o local.
#' @return arquivo .zip ou da extencao da url passada.
#' @examples
#' \dontrun{
#' require(rnp)
#' require(dplyr)
#' url_in = 'http://download.inep.gov.br/microdados/micro_censo_edu_superior1995.zip'
#' rnp_get_inep_censo(url = url_in)
#' rnp_get_inep_censo()
#' }
#' @author LOPES, J. E.
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
    cat("Ignorando a data, pois voce passou urls.\n")
    anos <- NULL
    nm <- sapply(url, function(i){
      o <- unlist(stringr::str_split(i, pattern = "\\/"))
      unname(o[length(o)])
    })
  } else if(!is.null(ano) & is.null(url)) {
    if(!ano %in% s) {
      stop("Passe valores para anos entre 1995 e 2017 ou uma url do arquivo .zip valida.\n")
    } else {
      anos <- ano
      nm <- paste0("base_", anos, ".zip")
    }
  } else if(!is.null(ano) & !is.null(url)){
    cat("Ignorando a data, pois voce passou urls.\n")
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

#' Estatisticas descritivas por grupo
#' @description
#' Calcula estatisticas descritivas por grupo. Ela recebe como entrada um data.frame
#' o nome da variavel numerica e um vetor ou lista de nomes das variaveis que serao utilizadas
#' como grupos. A funcao trabalha com apoio da funcao \code{plyr::\link[plyr]{ddply}} e aceita muitos
#' grupos.
#' @param base data.frame com as variaveis de entrada
#' @param variavel o nome da variavel numerica entre aspas
#' @param grupos lista ou vetor de nomes das estatisticas de agrupamento na ordem em que deseja
#' obter os resultados.
#' @param digits total de digitos para arredondar os decimais.
#' @return
#' As estatisticas de saida desta funcao sao: total (N), soma, numero de missing (Nmis),
#' minimo, primeiro quartil (Q1), mediana (Q2), terceiro quartil (Q3), maximo, desvio pacrao (devpad)
#' e coeficiente de variacao (cv) em um data.frame agrupadas conforme as classes das variaveis de
#' agrupamento.
#' @author LOPES, J. E.
#' @examples
#' rnp_summary_by(base = mtcars, variavel = "wt", grupos = "gear")
#' rnp_summary_by(base = mtcars, variavel = "wt", grupos = c("gear","cyl"))
#' rnp_summary_by(base = mtcars, variavel = "wt",
#'                grupos = list("gear","cyl"), digits = 2)
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
                       rnp_summary(xx[,variavel], digits = digits)
                     })
  return(out)
}

#' Dados do censo ensino superior INEP 2017 (Cursos)
#' @description
#' Este conjunto de dados faz parte da base de dados do INEP para o censo
#' brasileiro do ensino superior. Nesta parte temos os dados dos cursos
#' mapeados pelo censo naquele ano.
#' Trata-se de uma amostras das variaveis, porem o total de observacoes
#' esta completo.
#' Para obter dados de outros anos consulte o link do site do INEP ou
#' a funcao \code{\link{rnp_get_inep_censo}}
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
#' Instituicoes de Ensino Superior mapeados pelo censo naquele ano.
#' Trata-se de uma amostras das variaveis, porem o total de observacoes
#' esta completo.
#' Para obter dados de outros anos consulte o link do site do INEP ou
#' a funcao \code{\link{rnp_get_inep_censo}}
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
#' @description
#' Este conjunto de dados faz parte da base de dados do INEP para o censo
#' brasileiro do ensino superior. Nesta parte temos os dados dos docentes
#' das Instituicoes de Ensino Superior mapeados pelo censo naquele ano.
#' Trata-se de uma amostras das variaveis, porem o total de observacoes
#' esta completo.
#' Para obter dados de outros anos consulte o link do site do INEP ou
#' a funcao \code{\link{rnp_get_inep_censo}}
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
#' @description
#' Este conjunto de dados faz parte da base de dados do INEP para o censo
#' brasileiro do ensino superior.
#' Nesta parte temos os dados dos locais de oferta do curso das
#' Instituicoes de Ensino Superior mapeados pelo censo naquele ano.
#' Trata-se de uma amostras das variaveis, porem o total de observacoes
#' esta completo.
#' Para obter dados de outros anos consulte o link do site do INEP ou
#' a funcao \code{\link{rnp_get_inep_censo}}
#' @name dm_local
#' @docType data
#' @author INEP
#' @references \url{http://inep.gov.br/microdados}
#' @keywords dm_docente
#' @examples
#' require(rnp)
#' str(dm_docente)
NULL



#' Carregar ou instalar pacotes CRAN
#' @description
#' Carega pacotes necesarios pra trabalhar com o R NA PRATICA
#' @param pacotes Lista ou vetr e nomes de pacotes separados por aspas e ponto e virgula.
#' @examples
#' require(rnp)
#' rnp_load_packages()
#' @export
rnp_load_packages <- function(pacotes =  c("tidyverse","lubridate", "magrittr","broom","stringr",
                                           "plotly","ggplot2","data.table","DT", "formatR", "svglite",
                                           "tufte","kableExtra","ggmosaic","prettydoc")){
  lapply(pacotes, function(i) {
    if (system.file(package = i) == '') {
      cat("Pacote ", i, "nao localizado. Tentando instalar, se existir no CRAN...\n")
      rnp_try_error(install.packages(i, dependencies = TRUE, verbose = TRUE))
    }
  })

  lapply(pacotes, function(i) {
    rnp_try_error(require(i, character.only = TRUE, warn.conflicts = TRUE, quietly = TRUE))
    }
  )

  # Instalar rnp
  if(!require(rnp)){
    devtools::install_github("evandeilton/rnp")
  }
}

#' Estatistica de associacao
#'
#' @description Recebe dois vetores categoricos de tamanhos iguais e calcula as estatisticas
#' Qui-quadrado, V de Cramer e o coeficiante de contingencia para a tabela de contingencia
#' das duas variaveis criada pela funcao \code{\link{table}}.
#'
#' @param x variavel um
#' @param y variavel dois
#' @param ... argumentos passados para a funcao \code{\link{chisq.test}}
#' @author LOPES, J. E.
#' @export
rnp_associacao <- function(x, y, ...){
  oo <- table(x, y)
  ch <- chisq.test(oo)
  rr <- sum(ch$expected)*(min(dim(ch$expected))-1)
  vv <- sqrt(ch$statistic / rr)
  cc <- sqrt((min(dim(ch$observed)))/(min(dim(ch$observed))-1))*sqrt(ch$statistic/(ch$statistic+sum(ch$observed)))
  out <- unname(c(ch$statistic, vv, cc))
  names(out) <- c("Qui-quadrado","V-Cramer","C-Contingencia")
  return(out)
}


#' Calculo de correlacao
#'
#' @description Esta funcao recebe como entrada uma base de dados com pelo menos duas
#' variaveis numericas e deternina os coeficientes de correlacao de Pearson, Spearman
#' e Kendal.
#'
#' @details E possivel que existam variaveis nao numerias em sua base de dados. Neste caso,
#' elas serao eliminadas. Para mais detalhes veja \code{\link{cor}}.
#'
#' @param base data.frame, tibla, data.table, etc.
#' @param digits numeros de digitos para arrendondar o valor da correlacao.
#' @param ... argumentos passados para a funcao \code{\link{chisq.test}}
#' @author LOPES, J. E.
#' @return Um data.frame com cinco colunas onde as duas primeiras sao as combinacoes
#' aos pares de cada uma das variaveis e as tres ultimas sao as correlacoes de
#' Pearson, Spearman e Kendal respectivamente.
#' @export
rnp_correlacao <- function (base, digits = 4) {
  if(missing(base)) {
    stop("log: Informe uma base com pelo menos duas variaveis numericas.\n")
  }

  cl <- sapply(base, class)
  if(any(!cl %in% c("integer", "numeric"))) {
    cat("log: parece que a base tem variaveis nao numericas, removendo e trabalhando apenas com as numericas")
  }
  if(length(names(cl)[cl %in% c("integer", "numeric")]) < 2) {
    stop("log: Informe uma base com pelo menos duas variaveis numericas.\n")
  }
  base <- subset(base, select = names(cl)[cl %in% c("integer", "numeric")])
  metodos <- c("pearson","spearman", "kendall")
  lc <- lapply(metodos, function(i){
    tab_cor <- round(cor(base, use = "complete.obs", method = i), digits)
    tab_cor[lower.tri(tab_cor, diag = TRUE)] <- NA
    tab_cor <- as.data.frame(as.table(tab_cor))
    tab_cor <- na.omit(tab_cor)
    return(tab_cor)
  })
  names(lc) <- metodos
  pe <- lc$pearson
  sp <- lc$spearman
  ke <- lc$kendall
  cors <- merge(merge(pe, sp,
                     by.x = c("Var1","Var2"), by.y = c("Var1","Var2")),
               ke, by.x = c("Var1","Var2"), by.y = c("Var1","Var2"))
  colnames(cors) <- c("x", "y", metodos)
  cors <- cors[order(cors$x, -abs(cors$pearson)),]
  return(cors)
}

#' Media aritmetica simples e ponderada
#' @details Esta funcao e um atalho para as funcoes \code{\link{mean}} e \code{\link{weighted.mean}}
#' que calculam respectivamente as medias simples e ponderada. Caso seja passado um vetor de pesos e
#' ele contenha missing e o argumento remove.na for TRUE constuimos um data.frame e removemos os
#' NA's na mesma posicao que aparecem nos dois vetores.
#' @param x vetor numerico.
#' @param peso vetor de pesos. Se informado, calcula a media ponderada  .
#' @param remove.na Se TRUE remove NA's do vetor x e/ou dos pesos.
#' @return numerico
#' @author LOPES, J. E.
#' @examples
#' require(rnp)
#' x <- c(2,2,5)
#' p <- c(3,5,NA)
#' media_aritmetica(x)
#' media_aritmetica(x, p)
#' media_aritmetica(x, p, remove.na = TRUE)
#' @export
media_aritmetica <- function(x, peso = NULL, remove.na = TRUE){
  if(missing(x)) stop("Informe um vetor numerico")
  if(remove.na && !is.null(peso)){
    aux <- na.exclude(data.frame(x = x, peso = peso))
    x <- aux$x
    peso <- aux$peso
  } else if(remove.na) {
    x <- na.exclude(x)
  } else {
    x
  }
  ma <- c()
  ma <- if(!is.null(peso)){
    weighted.mean(x = x, w = peso, na.rm = remove.na)
  } else {
    mean(x = x, na.rm = remove.na)
  }
  return(ma)
}

#' Media geometrica simples e ponderada
#' @details Esta funcao calcula respectivamente as medias geometricas simples e ponderada.
#' Caso seja passado um vetor de pesos e ele contenha missing e o argumento remove.na
#' for TRUE constuimos um data.frame e removemos os NA's na mesma posicao que aparecem
#' nos dois vetores.
#' @param x vetor numerico.
#' @param peso vetor de pesos. Se informado, calcula a media ponderada  .
#' @param remove.na Se TRUE remove NA's do vetor x e/ou dos pesos.
#' @return numerico
#' @author LOPES, J. E.
#' @examples
#' require(rnp)
#' x <- c(2,2,5)
#' p <- c(3,5,NA)
#' media_geometrica(x)
#' media_geometrica(x, p)
#' media_geometrica(x, p, remove.na = TRUE)
#' @export
media_geometrica <- function(x, peso = NULL, remove.na = TRUE) {
  if(missing(x)) stop("Informe um vetor numerico positivo")
  if(remove.na && !is.null(peso)){
    aux <- na.exclude(data.frame(x = x, peso = peso))
    x <- aux$x
    peso <- aux$peso
  } else if(remove.na) {
    x <- na.exclude(x)
  } else {
    x
  }

  n <- length(x)
  mg <- c()

  if(!is.null(peso)){
    if(length(peso) != n) stop("O vetor de pesos precisar ser do tamanho do vetor x")
    if(sum(x > 0, na.rm = TRUE) >= 0) {
      mg <- exp((1/(sum(peso, na.rm = remove.na)))*sum(peso*log(x), na.rm = remove.na))
    } else {
      cat("Media geomterica tem sentido apenas para numeros positivos!\n")
    }
  } else {
    if(sum(x > 0, na.rm = TRUE) >= 0) {
      mg <- exp((1/n)*sum(log(x), na.rm = remove.na))
    } else {
      cat("Media geomterica tem sentido apenas para numeros positivos!\n")
    }
  }
  return(mg)
}

#' Media harmonica simples e ponderada
#' @details Esta funcao calcula respectivamente as medias harmonicas simples e ponderada.
#' Caso seja passado um vetor de pesos e ele contenha missing e o argumento remove.na
#' for TRUE constuimos um data.frame e removemos os NA's na mesma posicao que aparecem
#' nos dois vetores.
#' @param x vetor numerico.
#' @param peso vetor de pesos. Se informado, calcula a media ponderada  .
#' @param remove.na Se TRUE remove NA's do vetor x e/ou dos pesos.
#' @return numerico
#' @author LOPES, J. E.
#' @examples
#' require(rnp)
#' x <- c(2,2,5)
#' p <- c(3,5,NA)
#' media_harmonica(x)
#' media_harmonica(x, p)
#' media_harmonica(x, p, remove.na = TRUE)
#' @export
media_harmonica <- function(x, peso = NULL, remove.na = TRUE) {
  if(missing(x)) stop("Informe um vetor numerico positivo")

  if(remove.na && !is.null(peso)){
    aux <- na.exclude(data.frame(x = x, peso = peso))
    x <- aux$x
    peso <- aux$peso
  } else if(remove.na) {
    x <- na.exclude(x)
  } else {
    x
  }
  n <- length(x)
  mh <- c()

  if(!is.null(peso)){
    if(length(peso) != n) {
      stop("O vetor de pesos precisar ser do tamanho do vetor x")
    } else {
      mh <- (sum(peso * x^(-1), na.rm = remove.na) / (sum(peso, na.rm = remove.na)))^(-1)
    }
  } else {
    mh <- (sum(x^(-1)) / length(x))^(-1)
  }
  return(mh)
}


#' Calcula medias de varios tipos
#'
#' @description Esta funcao e uma chamada master paras as funcoes \code{\link{media_aritmetica}},
#' \code{\link{media_geometrica}} e \code{\link{media_harmonica}}. Serve para calcular médias
#' de vetores numéricos e de matrizes/data.frames formados por colunas numericas.
#' @details Atraves desta funcao e possivel calcular medias aritmeticas, geometricas e harmonicas
#' simples e ponderadas. Se for passado uma matriz ela varre tdoas as colunas e determina as medias
#' para cada variavel. Se for passado argumento peso, as medias sao ponderadas pelos seus respectivos
#' pesos. No caso de matrizes, a matriz de peso precisa ter as mesmas dimensoes da matriz de origem
#' e o mesmo tipo de dados.
#' Quando existir peso, o nome das colunas do data.frame serao nomeadas como: media_ap, media_gp e media_hp.
#' Caso contrario, media_a, media_g e media_h.
#' @param x Vetor, matrix ou data.frame de dados numericos
#' @param peso Opcional. Vetor, matrix ou data.frame de dados numericos com pesos com as mesmas
#' caracteristcias de comprimento ou dimensoes do argumento x
#' @param remove.na Se TRUE remove os NA's, caso existam nos dados
#' @return Vetor ou data.frame com as medias de uma ou mais variaveis. No caso de data.frames, a
#' saida e um data.frame com quatro colunas, sendo uma o nome das variaveis e as outras tres as
#' colunas de medias aritmetica, geometrica e harmonica respectivamente. No caso de vetor numerico,
#' a saida e um data.frame simples com tres colunas e uma linha contendo as medias.
#' @author LOPES, J. E.
#' @examples
#' require(rnp)
#' # Exemplos com vetores
#' x <- mtcars$mpg;x[3:5] <- NA
#' p <- mtcars$cyl;p[5:7] <- NA
#' rnp_media(x = x, peso = NULL, remove.na = TRUE)
#' rnp_media(x = x, peso = NULL, remove.na = FALSE)
#' rnp_media(x = x, peso = p, remove.na = TRUE)
#' rnp_media(x = x, peso = p, remove.na = FALSE)
#' # Exemplos com matrizes (de mesmo tipo)
#' X <- mtcars
#' Y <- data.frame(matrix(runif(prod(dim(mtcars)), 5, 10), nrow = nrow(mtcars), ncol = ncol(mtcars)));
#' class(X); class(Y)
#' names(Y) <- names(X)
#' dim(X) == dim(Y)
#' rnp_media(X)
#' rnp_media(X, Y)
#' # Com dados reais da base rnp::dm_ies
#' X <- as.data.frame(rnp::dm_ies)
#' X <- X[,c(sapply(X, class) %in% c("integer", "numeric"))]
#' rnp_media(X[,-c(1:6)])
#' a <- sapply(X[,-c(1:6)], mean)
#' b <- rnp_media(X[,-c(1:6)])$media_a
#' all(a == b)
#' @export
rnp_media <- function(x, peso = NULL, remove.na = TRUE){

  aux <- function(x, peso, remove.na) {
    c(media_aritmetica(x, peso, remove.na),
      media_geometrica(x, peso, remove.na),
      media_harmonica(x, peso, remove.na))
  }
  out <- if(is.null(peso)){
    if(is.null(dim(x))){
      o <- data.frame(rbind(aux(x = x, peso = peso, remove.na = remove.na)))
      names(o) <- c("media_a","media_g","media_h")
      o
    } else {
      o <- sapply(x, function(i){
        rbind(aux(x = i, peso = peso, remove.na = remove.na))
      })
      o <- data.frame(variavel = row.names(t(o)), as.data.frame.matrix(t(o)), stringsAsFactors = FALSE)
      names(o)[-1] <- c("media_a","media_g","media_h")
      rownames(o) <- NULL
      o
    }
  } else {
    if(is.null(dim(x)) && is.null(dim(peso))){
      o <- data.frame(rbind(aux(x = x, peso = peso, remove.na = remove.na)))
      names(o) <- c("media_ap","media_gp","media_hp")
      o
    } else if(all(dim(x) == dim(peso))){
      x <- data.frame(x, stringsAsFactors = FALSE)
      peso <- data.frame(peso, stringsAsFactors = FALSE)
      o <- sapply(seq_along(names(x)), function(i){
        rbind(aux(x = x[,i], peso = peso[,i], remove.na = remove.na))
      })
      o <- data.frame(variavel = row.names(t(x)), as.data.frame.matrix(t(o)), stringsAsFactors = FALSE)
      names(o)[-1] <- c("media_ap","media_gp","media_hp")
      rownames(o) <- NULL
      o
    } else {
      stop(cat("No caso de media ponderada, para matriz ou data.frame, os dois objetos precisam ter os mesmo nomes para cada variavel x e para cada peso. Verifique seus dados!\n"))
    }
  }
  return(out)
}

