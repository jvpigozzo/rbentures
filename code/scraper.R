

request_data <- function(url){
  res <- try(httr::GET(url), silent = T)
  if (!is(res, "try-error")) {
    return(res)
  } else {
    message("Invalid date range")
  }
}

get_request_content <- function(res,
                                encoding="latin1"){
  content <- httr::content(res, as = "text", encoding = encoding)
  return(content)
}

get_numeric_cols <- function(df, num_cols){
  df <- df |>
        dplyr::mutate_at(dplyr::vars(dplyr::matches(num_cols)), ~gsub('[.]', '', .)) |>
        dplyr::mutate_at(dplyr::vars(dplyr::matches(num_cols)), ~gsub('[,]', '.', .)) |>
        dplyr::mutate_at(dplyr::vars(dplyr::matches(num_cols)), as.numeric)
  return(df)
}

get_date_cols <- function(df, date_cols){
  df <- df |>
    dplyr::mutate_at(dplyr::vars(dplyr::matches(date_cols)), ~as.Date(., format = "%d/%m/%Y"))
  return(df)
}



get_storage_by_date <- function(start_date = Sys.Date() - 5, end_date = Sys.Date()){
  url <- "http://www.debentures.com.br/exploreosnd/consultaadados/estoque/estoqueporativo_e1.asp?dt_ini=%s&dt_fim=%s&ativo=&moeda=1&Op_exc=Nada&cab=s"
  start_date <- base::format(base::as.Date(start_date), "%d/%m/%Y")
  end_date <- base::format(base::as.Date(end_date), "%d/%m/%Y")
  url <- base::sprintf(url, start_date, end_date)
  res <- request_data(url=url)
  content <- get_request_content(res=res)
  txt <- base::readLines(base::textConnection(content))
  df <- utils::read.table(base::textConnection(txt, "r"), sep = "\t", header = FALSE, skip = 6)
  return(df)
}

get_storage_by_cetip_code <- function(cetip_code,
                                      start_date = Sys.Date(),
                                      end_date = Sys.Date()){
  url <- "http://www.debentures.com.br/exploreosnd/consultaadados/estoque/estoqueporativo_e.asp?dt_ini=%s&dt_fim=%s&ativo=%s%20%20%20%20&moeda=1&Op_exc=False&cab="
  start_date <- base::format(base::as.Date(start_date), "%d/%m/%Y")
  end_date <- base::format(base::as.Date(end_date), "%d/%m/%Y")
  url <- base::sprintf(url, start_date, end_date, cetip_code)
  res <- request_data(url=url)
  content <- get_request_content(res=res)
  txt <- base::readLines(base::textConnection(content))
  df <- utils::read.table(base::textConnection(txt, "r"), sep = "\t", header = FALSE, skip = 6)
  return(df)
}

get_storage_by_indexer <- function(indexer = c("Prefixado","ANBID","BTN","DI","DOLAR","FDS","IGP-DI","IGP-M","INPC","IPC","IPC-FIPE","IPC-M","IPC-R","IPCA","PÓS","PRÉ","SELIC","SEM ÍNDICE","TBF","TJLP","TR","TR-REAL","UFIR","US$ COMERCIAL"),
                                   start_date = Sys.Date()-5,
                                   end_date = Sys.Date()){
  url <- "http://www.debentures.com.br/exploreosnd/consultaadados/estoque/estoquepor_re.asp?op_rel=Indexadores&Dt_ini=%s&Dt_fim=%s&op_exc=False&op_subInd=&Opcao=%s&Moeda=1"
  start_date <- base::format(base::as.Date(start_date), "%d/%m/%Y")
  end_date <- base::format(base::as.Date(end_date), "%d/%m/%Y")
  url <- base::sprintf(url, start_date, end_date, indexer)
  res <- request_data(url=url)
  content <- get_request_content(res=res)
  txt <- base::readLines(base::textConnection(content))
  txt <- txt[-1][grep("[^\\s]", txt[-1])]
  df <- data.frame()
  for (i in 1:length(txt)) {
    if (base::grepl("Data do Estoque", txt[i])) {
      date_str <- base::gsub(".*Data do Estoque (\\d{2}/\\d{2}/\\d{4}) -.*", "\\1", txt[i])
    }
    if (base::grepl("Indexadores", txt[i])) {
      values <- base::strsplit(txt[i+1], "\t")[[1]]
      row <- data.frame(date = as.Date(date_str, format = "%d/%m/%Y"),
                        indexer = indexer,
                        value_mrkt = values[2],
                        value_treasury = values[3],
                        value = values[4])
      df <- rbind(df, row)
    }
  }
  return(df)
}


get_storage_by_value_adjustment <- function(adjustment = c("inflation","interest"),
                                            start_date = Sys.Date()-5,
                                            end_date = Sys.Date()){

  if(adjustment=="inflation"){
    adj <- "n"
  } else {
    adj <- "s"
  }
  url <- "http://www.debentures.com.br/exploreosnd/consultaadados/estoque/estoquepor_re.asp?op_rel=Tipo&Dt_ini=%s&Dt_fim=%s&op_exc=False&op_subInd=&Opcao=%s&Moeda=1"
  start_date <- base::format(base::as.Date(start_date), "%d/%m/%Y")
  end_date <- base::format(base::as.Date(end_date), "%d/%m/%Y")
  url <- base::sprintf(url, start_date, end_date, adj)
  res <- request_data(url=url)
  content <- get_request_content(res=res)
  txt <- base::readLines(base::textConnection(content))
  txt <- txt[-1][grep("[^\\s]", txt[-1])]
  df <- data.frame()
  for (i in 1:length(txt)) {
    if (base::grepl("Data do Estoque", txt[i])) {
      date_str <- base::gsub(".*Data do Estoque (\\d{2}/\\d{2}/\\d{4}) -.*", "\\1", txt[i])
    }
    if (base::grepl("Tipo", txt[i])) {
      values <- base::strsplit(txt[i+1], "\t")[[1]]
      row <- data.frame(date = as.Date(date_str, format = "%d/%m/%Y"),
                        adjustment = adjustment,
                        value_mrkt = values[2],
                        value_treasury = values[3],
                        value = values[4])
      df <- rbind(df, row)
    }
  }
  return(df)
}


get_storage_by_shape <- function(shape = c("nominativa","escritural"),
                                            start_date = Sys.Date()-5,
                                            end_date = Sys.Date()){

  if(shape=="nominativa"){
    s <- "0"
  } else {
    s <- "1"
  }
  url <- "http://www.debentures.com.br/exploreosnd/consultaadados/estoque/estoquepor_re.asp?op_rel=Forma&Dt_ini=%s&Dt_fim=%s&op_exc=False&op_subInd=&Opcao=%s&Moeda=1"
  start_date <- base::format(base::as.Date(start_date), "%d/%m/%Y")
  end_date <- base::format(base::as.Date(end_date), "%d/%m/%Y")
  url <- base::sprintf(url, start_date, end_date, s)
  res <- request_data(url=url)
  content <- get_request_content(res=res)
  txt <- base::readLines(base::textConnection(content))
  txt <- txt[-1][grep("[^\\s]", txt[-1])]
  df <- data.frame()
  for (i in 1:length(txt)) {
    if (base::grepl("Data do Estoque", txt[i])) {
      date_str <- base::gsub(".*Data do Estoque (\\d{2}/\\d{2}/\\d{4}) -.*", "\\1", txt[i])
    }
    if (base::grepl("Forma", txt[i])) {
      values <- base::strsplit(txt[i+1], "\t")[[1]]
      row <- data.frame(date = as.Date(date_str, format = "%d/%m/%Y"),
                        shape = shape,
                        value_mrkt = values[2],
                        value_treasury = values[3],
                        value = values[4])
      df <- rbind(df, row)
    }
  }
  return(df)
}

get_trades_by_date <- function(start_date = Sys.Date()-5,
                               end_date = Sys.Date()){
  url <- "http://www.debentures.com.br/exploreosnd/consultaadados/mercadosecundario/precosdenegociacao_e.asp?op_exc=False&emissor=&isin=&ativo=&dt_ini=%s&dt_fim=%s"
  start_date <- base::format(base::as.Date(start_date), "%Y%m%d")
  end_date <- base::format(base::as.Date(end_date), "%Y%m%d")
  url <- base::sprintf(url, start_date, end_date)
  res <- request_data(url=url)
  content <- get_request_content(res=res)
  txt <- base::readLines(base::textConnection(content))
  df <- utils::read.table(base::textConnection(txt, "r"), sep = "\t", header = FALSE, skip = 3)
  return(df)
}


get_trades_by_cetip_code <- function(cetip_code,
                                     start_date = Sys.Date()-1,
                                     end_date = Sys.Date()){
  url <- "http://www.debentures.com.br/exploreosnd/consultaadados/mercadosecundario/precosdenegociacao_e.asp?op_exc=False&emissor=&isin=&ativo=%s&dt_ini=%s&dt_fim=%s"
  start_date <- base::format(base::as.Date(start_date), "%Y%m%d")
  end_date <- base::format(base::as.Date(end_date), "%Y%m%d")
  url <- base::sprintf(url, cetip_code, start_date, end_date)
  res <- request_data(url=url)
  content <- get_request_content(res=res)
  txt <- base::readLines(base::textConnection(content))
  df <- utils::read.table(base::textConnection(txt, "r"), sep = "\t", header = FALSE, skip = 3)
  return(df)
}

get_registered_coordinators <- function(){
  url <- "http://www.debentures.com.br/exploreosnd/consultaadados/participantes/coordenadores_e.asp?op_exc=False&coordenador="
  res <- request_data(url=url)
  content <- get_request_content(res=res)
  txt <- base::readLines(base::textConnection(content))
  df <- data.frame()
  for (i in 1:length(txt)) {
    if (!startsWith(txt[i], ";")) {
      coordinator <- txt[i]
      i <- i + 3
      if (txt[i] == ";EMISSORES\t;ATIVOS\t;SITUAÇÃO") {
        i <- i + 1
        while(i+1 <= length(txt) && txt[i]!='; '){
          temp <- data.frame()
          values <- strsplit(txt[i], "\t")[[1]]
          temp[1,"coordinator"] <- coordinator
          temp[1,"issuer"] <- values[1]
          temp[1,"codigo_cetip"] <- values[2]
          temp[1,"status"] <- values[3]
          df <- rbind(temp, df)
          i <- i + 1
        }
      }
    }
  }
  df <- data.frame(lapply(df, function(x) gsub(";", "", x)))
  return(df)
 }



get_registered_bakers <- function(){
  url <- "http://www.debentures.com.br/exploreosnd/consultaadados/participantes/bancosmandatarios_e.asp?op_exc=False&mandatario="
  res <- request_data(url=url)
  content <- get_request_content(res=res)
  txt <- base::readLines(base::textConnection(content))
  df <- data.frame()
  for (i in 1:length(txt)) {
    if (i+1 <= length(txt) && startsWith(txt[i+1], "Contato:")) {
      banker <- txt[i]
      i <- i + 3
      if (txt[i] == "Emissores\tAtivos\tSituação") {
        i <- i + 1
        while(i+1 <= length(txt) && txt[i]!=""){
          temp <- data.frame()
          values <- strsplit(txt[i], "\t")[[1]]
          temp[1,"banker"] <- banker
          temp[1,"issuer"] <- values[1]
          temp[1,"codigo_cetip"] <- values[2]
          temp[1,"status"] <- values[3]
          df <- rbind(temp, df)
          i <- i + 1
        }
      }
    }
  }
  return(df)
}

get_registered_trustees <- function(){
  url <-"http://www.debentures.com.br/exploreosnd/consultaadados/participantes/agentesfiduciarios_e.asp?op_exc=False&fiduciario="
  res <- request_data(url=url)
  content <- get_request_content(res=res)
  txt <- base::readLines(base::textConnection(content))
  df <- data.frame()
  for (i in 1:length(txt)) {
    if (i+1 <= length(txt) && startsWith(txt[i+1], "Contato:")) {
      banker <- txt[i]
      i <- i + 3
      if (txt[i] == "Emissores\tAtivos\tSituação") {
        i <- i + 1
        while(i+1 <= length(txt) && txt[i]!=" "){
          temp <- data.frame()
          values <- strsplit(txt[i], "\t")[[1]]
          temp[1,"banker"] <- banker
          temp[1,"issuer"] <- values[1]
          temp[1,"codigo_cetip"] <- values[2]
          temp[1,"status"] <- values[3]
          df <- rbind(temp, df)
          i <- i + 1
        }
      }
    }
  }
  return(df)
}

get_issuing_prices <- function(start_date = Sys.Date()-5,
                               end_date = Sys.Date()){
  url <- "http://www.debentures.com.br/exploreosnd/consultaadados/emissoesdedebentures/puhistorico_e.asp?op_exc=False&ativo=&dt_ini=%s&dt_fim=%s&Submit.x=&Submit.y="
  start_date <- base::format(base::as.Date(start_date), "%d/%m/%Y")
  end_date <- base::format(base::as.Date(end_date), "%d/%m/%Y")
  url <- base::sprintf(url, start_date, end_date)
  res <- request_data(url=url)
  content <- get_request_content(res=res)
  txt <- base::readLines(base::textConnection(content))
  rows_to_skip <- 3
  rows_to_keep <- length(txt) - (rows_to_skip + 4)
  df <- utils::read.table(base::textConnection(txt[1:rows_to_keep], "r"), sep = "\t", header = FALSE, skip = 3)
  return(df)
}

#ComRepactuacao

get_duration <- function(start_date = Sys.Date()-21,
                         end_date = Sys.Date(),
                         cvm_date = FALSE,
                         cetip_code = NULL){
  url <- "http://www.debentures.com.br/exploreosnd/consultaadados/emissoesdedebentures/prazo-medio_e.asp?Ativo=%s&Emissor=&dataCVM=%s&dt_ini=%s&dt_fim=%s&anoini=&anofim=&ComRepactuacao=&Op_exc=False"
  cvm_date <- ifelse(cvm_date, 'r', 'e')
  start_date <- base::format(base::as.Date(start_date), "%d/%m/%Y")
  end_date <- base::format(base::as.Date(end_date), "%d/%m/%Y")
  url <- base::sprintf(url, ifelse(is.null(cetip_code), "", cetip_code), cvm_date, start_date, end_date)
  res <- request_data(url=url)
  content <- get_request_content(res=res)
  txt <- base::readLines(base::textConnection(content))
  df <- utils::read.table(base::textConnection(txt, "r"), sep = "\t", header = FALSE, skip = 7)
  return(df)
}


#11 Amortização
#04 Atualização
#01 Juros
#18 Opção de Venda
#15 Participação
#03 Prêmio
#21 Prêmio de Permanência
#02 Repactuação
#06 Resgate Total Antecipado
#99 Vencimento

get_events <- function(start_date = Sys.Date(),
                       end_date = Sys.Date()+5,
                       payment_start_date = NULL,
                       payment_end_date = NULL,
                       cetip_code = NULL,
                       event = NULL){
  url <- "http://www.debentures.com.br/exploreosnd/consultaadados/eventosfinanceiros/agenda_e.asp?emissor=&ativo=%s&evento=%s&dt_ini=%s&dt_fim=%s&dt_pgto_ini=%s&dt_pgto_fim=%s&Submit32.x=&Submit32.y="
  start_date <- ifelse(is.null(start_date),'',base::format(base::as.Date(start_date), "%d/%m/%Y"))
  end_date <- ifelse(is.null(end_date),'',base::format(base::as.Date(end_date), "%d/%m/%Y"))
  url <- base::sprintf(url, ifelse(is.null(cetip_code), "", cetip_code),
                            ifelse(is.null(event), "", event),
                            ifelse(is.null(start_date), "", start_date),
                            ifelse(is.null(end_date), "", end_date),
                            start_date, end_date,
                            ifelse(is.null(payment_start_date), "", payment_start_date),
                            ifelse(is.null(payment_end_date), "", payment_end_date))

  res <- request_data(url=url)
  content <- get_request_content(res=res)
  txt <- base::readLines(base::textConnection(content))
  df <- utils::read.table(base::textConnection(txt, "r"), sep = "\t", header = FALSE, skip = 3)
  return(df)
}



get_events_prices <- function(start_date = Sys.Date()-21,
                              end_date = Sys.Date(),
                              cetip_code = NULL,
                              event = NULL){
  url <- "http://www.debentures.com.br/exploreosnd/consultaadados/eventosfinanceiros/pudeeventos_e.asp?op_exc=False&emissor=&ativo=%s&dt_ini=%s&dt_fim=%s&evento=%s&Submit.x=&Submit.y="
  start_date <- base::format(base::as.Date(start_date), "%d/%m/%Y")
  end_date <- base::format(base::as.Date(end_date), "%d/%m/%Y")
  url <- base::sprintf(url, ifelse(is.null(cetip_code), "", cetip_code), start_date, end_date, ifelse(is.null(event), "", event))
  res <- request_data(url=url)
  content <- get_request_content(res=res)
  txt <- base::readLines(base::textConnection(content))
  df <- utils::read.table(base::textConnection(txt, "r"), sep = "\t", header = FALSE, skip = 3)
  return(df)

}

get_issuing_volume_by_date <- function(start_date = Sys.Date()-21,
                                       end_date = Sys.Date()-1,
                                       date_type = c('issuing','registering'),
                                       cvm_instruction = c(400, 476),
                                       as = c("tibble", "xts", "ts", "data.frame", "text")){
  url <- "http://www.debentures.com.br/exploreosnd/consultaadados/volume/volumeporperiodo_e.asp?op_exc=False&emissao=%s&dt_ini=%s&dt_fim=%s&ICVM=%s&moeda=1&Submit3.x=&Submit3.y="
  start_date <- base::format(base::as.Date(start_date), "%d/%m/%Y")
  end_date <- base::format(base::as.Date(end_date), "%d/%m/%Y")
  url <- base::sprintf(url, ifelse(is.null(date_type), "0", ifelse(date_type=='registering', "2", "0")), start_date, end_date, ifelse(is.null(cvm_instruction), "", ifelse(cvm_instruction==400, "1", "2")))
  res <- request_data(url=url)
  content <- get_request_content(res=res)
  txt <- base::readLines(base::textConnection(content))
  rows_to_skip <- 2
  rows_to_keep <- length(txt) - (rows_to_skip)
  df <- utils::read.table(base::textConnection(txt[1:rows_to_keep], "r"), sep = "\t", header = FALSE, skip = 3)
  df <- get_numeric_cols(df=df, num_cols = c(''))
  return(df)
}

get_numeric_cols(df=df, num_cols = 'V7')

get_date_cols(df=df, date_cols = 'V4')




df <- get_events(end_date=NULL, cetip_code = 'OENC11')
tx = 3.9
vna = 836.734
amort = vna*(2.4390/100)
rem = vna-amort
fator = (((tx/100) + 1)^(3/252))^(3/3)
rem-(rem*fator)

