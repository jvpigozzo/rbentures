#' Get Storage Data by Date Range
#'
#' This function retrieves storage data from the Debentures website for a specified date range.
#'
#' @param date Date for retrieving storage data. Defaults to 5 days ago from the current date.
#'
#' @return A data frame containing the storage data for the specified date range.
#'
#' @examples
#' # Retrieve storage data for the last week
#' storage_data <- get_storage_by_date()
#'
#' # Retrieve storage data for a specific date
#' storage_data <- get_storage_by_date(date = "2022-01-01")
#'
#' @importFrom utils read.table
#'
#' @keywords debentures finance storage data
#' @export
get_storage_by_date <- function(date = Sys.Date() - 5){
  url <- "http://www.debentures.com.br/exploreosnd/consultaadados/estoque/estoqueporativo_e1.asp?dt_ini=%s&dt_fim=%s&ativo=&moeda=1&Op_exc=Nada&cab=s"
  date <- base::format(base::as.Date(date), "%d/%m/%Y")
  url <- base::sprintf(url, date, date)
  res <- request_data(url=url)
  content <- get_request_content(res=res)
  txt <- base::readLines(base::textConnection(content))
  txt <- txt[1:(length(txt) - 2)]
  df <- utils::read.table(base::textConnection(txt, "r"), sep = "\t", header = FALSE, skip = 6)
  num_cols <- c("V3", "V4", "V5", "V6", "V7", "V8")
  df <- get_numeric_cols(df, num_cols)
  names(df) <- c("issuer", "ticker", "qty_mrkt", "vol_mrkt", "qty_treasury", "vol_treasury", "qty", "vol", "situation")
  return(df)
}



#' Get Storage Data by CETIP Code and Date Range
#'
#' This function retrieves storage data from the Debentures website for a specified CETIP code and date range.
#'
#' @param cetip_code The CETIP code for the desired storage data.
#' @param start_date The start date for retrieving storage data. Defaults to the current date.
#' @param end_date The end date for retrieving storage data. Defaults to the current date.
#'
#' @return A data frame containing the storage data for the specified CETIP code and date range.
#'
#' @examples
#' # Retrieve storage data for a specific CETIP code and date range
#' storage_data <- get_storage_by_cetip_code(cetip_code = "ABC123", start_date = "2022-01-01", end_date = "2022-12-31")
#'
#' @importFrom utils read.table
#'
#' @keywords debentures finance storage data CETIP code
#' @export
get_storage_by_cetip_code <- function(cetip_code,
                                      start_date = Sys.Date() - 5,
                                      end_date = Sys.Date()){
  url <- "http://www.debentures.com.br/exploreosnd/consultaadados/estoque/estoqueporativo_e.asp?dt_ini=%s&dt_fim=%s&ativo=%s&moeda=1&Op_exc=False&cab="
  start_date <- base::format(base::as.Date(start_date), "%d/%m/%Y")
  end_date <- base::format(base::as.Date(end_date), "%d/%m/%Y")
  url <- base::sprintf(url, start_date, end_date, cetip_code)
  res <- request_data(url=url)
  content <- get_request_content(res=res)
  txt <- base::readLines(base::textConnection(content))
  df <- utils::read.table(base::textConnection(txt, "r"), sep = "\t", header = FALSE, skip = 6)
  df <- df[c("V1", "V2", "V3", "V4", "V5", "V6", "V7")]
  num_cols <- c("V2", "V3", "V4", "V5", "V6", "V7")
  date_cols <- c("V1")
  df <- get_numeric_cols(df, num_cols)
  df <- get_date_cols(df, date_cols)
  names(df) <- c("date", "qty_mrkt", "vol_mrkt", "qty_treasury", "vol_treasury", "qty", "vol")
  return(df)
}



#' Get Storage Data by Indexer and Date Range
#'
#' This function retrieves storage data from the Debentures website for a specified indexer and date range.
#'
#' @param indexer The indexer for the desired storage data. Defaults to various financial indexers.
#'   Options include: Prefixado, ANBID, BTN, DI, DOLAR, FDS, IGP-DI, IGP-M, INPC, IPC,
#'   IPC-FIPE, IPC-M, IPC-R, IPCA, PÓS, PRÉ, SELIC, SEM ÍNDICE, TBF, TJLP, TR, TR-REAL,
#'   UFIR, US$ COMERCIAL.
#' @param start_date The start date for retrieving storage data. Defaults to 5 days ago from the current date.
#' @param end_date The end date for retrieving storage data. Defaults to the current date.
#'
#' @return A data frame containing the storage data for the specified indexer and date range.
#'
#' @examples
#' # Retrieve storage data for a specific indexer and date range
#' storage_data <- get_storage_by_indexer(indexer = "IPCA", start_date = "2022-01-01", end_date = "2022-12-31")
#'
#' @importFrom utils read.table
#'
#' @keywords debentures finance storage data indexer
#' @export
get_storage_by_indexer <- function(indexer = NULL,
                                   start_date = Sys.Date()-5,
                                   end_date = Sys.Date()){

  if (!indexer %in% c("Prefixado","ANBID","BTN","DI","DOLAR","FDS","IGP-DI","IGP-M","INPC","IPC","IPC-FIPE","IPC-M","IPC-R","IPCA","PÓS","PRÉ","SELIC","SEM ÍNDICE","TBF","TJLP","TR","TR-REAL","UFIR","US$ COMERCIAL")) {
    stop("Invalid indexer selected.")
  }
  indexers <- list(
    "Prefixado" = 101,
    "ANBID" = 43,
    "BTN" = 7,
    "DI" = 3,
    "DOLAR" = 5,
    "FDS" = 21,
    "IGP-DI" = 10,
    "IGP-M" = 9,
    "INPC" = 16,
    "IPC" = 6,
    "IPC-FIPE" = 26,
    "IPC-M" = 25,
    "IPC-R" = 22,
    "IPCA" = 18,
    "PÓS" = 46,
    "PRÉ" = 45,
    "SELIC" = 1,
    "SEM ÍNDICE" = 0,
    "TBF" = 24,
    "TJLP" = 23,
    "TR" = 20,
    "TR-REAL" = 11,
    "UFIR" = 2,
    "US$ COMERCIAL" = 15
  )
  indexer_code <- indexers[[indexer]]
  url <- "http://www.debentures.com.br/exploreosnd/consultaadados/estoque/estoquepor_re.asp?op_rel=Indexadores&Dt_ini=%s&Dt_fim=%s&op_exc=False&op_subInd=&Opcao=%s&Moeda=1"
  start_date <- base::format(base::as.Date(start_date), "%d/%m/%Y")
  end_date <- base::format(base::as.Date(end_date), "%d/%m/%Y")
  url <- base::sprintf(url, start_date, end_date, indexer_code)
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
                        vol_mrkt = values[2],
                        vol_treasury = values[3],
                        vol = values[4])
      df <- rbind(df, row)
    }
  }
  num_cols <- c("value_mrkt", "vol_mrkt", "vol_treasury", "vol")
  df <- get_numeric_cols(df, num_cols)
  return(df)
}



#' Get Storage Data by Value Adjustment Type and Date Range
#'
#' This function retrieves storage data from the Debentures website for a specified value adjustment type and date range.
#'
#' @param adjustment The adjustment type for the desired storage data. Either "inflation" or "interest".
#' @param start_date The start date for retrieving storage data. Defaults to 5 days ago from the current date.
#' @param end_date The end date for retrieving storage data. Defaults to the current date.
#'
#' @return A data frame containing the storage data for the specified adjustment type and date range.
#'
#' @examples
#' # Retrieve storage data for a specific adjustment type and date range
#' storage_data <- get_storage_by_value_adjustment(adjustment = "inflation", start_date = "2022-01-01", end_date = "2022-12-31")
#'
#' @importFrom utils read.table
#'
#' @keywords debentures finance storage data value adjustment
#' @export
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



#' Get Storage Data by Shape and Date Range
#'
#' This function retrieves storage data from the Debentures website for a specified shape (nominativa or escritural) and date range.
#'
#' @param shape The shape for the desired storage data. Either "nominativa" or "escritural".
#' @param start_date The start date for retrieving storage data. Defaults to 5 days ago from the current date.
#' @param end_date The end date for retrieving storage data. Defaults to the current date.
#'
#' @return A data frame containing the storage data for the specified shape and date range.
#'
#' @examples
#' # Retrieve storage data for a specific shape and date range
#' storage_data <- get_storage_by_shape(shape = "nominativa", start_date = "2022-01-01", end_date = "2022-12-31")
#'
#' @importFrom utils read.table
#'
#' @keywords debentures finance storage data shape
#' @export
get_storage_by_shape <- function(shape = "nominativa",
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
  num_cols <- c("value_mrkt", "value_treasury", "value")
  df <- get_numeric_cols(df, num_cols)
  return(df)
}



#' Get Trades Data by Date Range
#'
#' This function retrieves trades data from the Debentures website for a specified date range.
#'
#' @param start_date The start date for retrieving trades data. Defaults to 5 days ago from the current date.
#' @param end_date The end date for retrieving trades data. Defaults to the current date.
#'
#' @return A data frame containing the trades data for the specified date range.
#'
#' @examples
#' # Retrieve trades data for the last week
#' trades_data <- get_trades_by_date()
#'
#' # Retrieve trades data for a specific date range
#' trades_data <- get_trades_by_date(start_date = "2022-01-01", end_date = "2022-12-31")
#'
#' @importFrom utils read.table
#'
#' @keywords debentures finance trades data
#' @export
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
  df <- get_date_cols(df, date_cols=c("V1"))
  return(df)
}



#' Get Trades Data by CETIP Code and Date Range
#'
#' This function retrieves trades data from the Debentures website for a specified CETIP code and date range.
#'
#' @param cetip_code The CETIP code for the desired trades data.
#' @param start_date The start date for retrieving trades data. Defaults to 1 day ago from the current date.
#' @param end_date The end date for retrieving trades data. Defaults to the current date.
#'
#' @return A data frame containing the trades data for the specified CETIP code and date range.
#'
#' @examples
#' # Retrieve trades data for a specific CETIP code and date range
#' trades_data <- get_trades_by_cetip_code(cetip_code = "ABC123", start_date = "2022-01-01", end_date = "2022-12-31")
#'
#' @importFrom utils read.table
#'
#' @keywords debentures finance trades data CETIP code
#' @export
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
  df <- get_date_cols(df, date_cols=c("V1"))
  return(df)
}



#' Get Registered Coordinators
#'
#' This function retrieves data about registered coordinators from the Debentures website.
#'
#' @return A data frame containing information about registered coordinators.
#'
#' @examples
#' # Retrieve information about registered coordinators
#' coordinators_data <- get_registered_coordinators()
#'
#' @keywords debentures finance coordinators data
#' @export
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



#' Get Registered Banks (Bakers)
#'
#' This function retrieves data about registered banks (bakers) from the Debentures website.
#'
#' @return A data frame containing information about registered banks (bakers).
#'
#' @examples
#' # Retrieve information about registered banks (bakers)
#' banks_data <- get_registered_bakers()
#'
#' @keywords debentures finance banks bakers data
#' @export
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



#' Get Registered Trustees
#'
#' This function retrieves data about registered trustees from the Debentures website.
#'
#' @return A data frame containing information about registered trustees.
#'
#' @examples
#' # Retrieve information about registered trustees
#' trustees_data <- get_registered_trustees()
#'
#' @keywords debentures finance trustees data
#' @export
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



#' Get Issuing Prices Data by Date Range
#'
#' This function retrieves issuing prices data from the Debentures website for a specified date range.
#'
#' @param start_date The start date for retrieving issuing prices data. Defaults to 5 days ago from the current date.
#' @param end_date The end date for retrieving issuing prices data. Defaults to the current date.
#'
#' @return A data frame containing the issuing prices data for the specified date range.
#'
#' @examples
#' # Retrieve issuing prices data for the last week
#' issuing_prices_data <- get_issuing_prices()
#'
#' # Retrieve issuing prices data for a specific date range
#' issuing_prices_data <- get_issuing_prices(start_date = "2022-01-01", end_date = "2022-12-31")
#'
#' @importFrom utils read.table
#'
#' @keywords debentures finance issuing prices data
#' @export
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
  df <- df[, 1:6]
  df <- get_numeric_cols(df, num_cols=c("V3", "V4", "V5", "V6"))
  df <- get_date_cols(df, date_cols=c("V1"))
  names(df) <- c("issuing_date", "ticker", "par_value", "interest", "premium", "price")
  return(df)
}



#' Get Duration Data by Date Range
#'
#' This function retrieves duration data from the Debentures website for a specified date range.
#'
#' @param start_date The start date for retrieving duration data. Defaults to 21 days ago from the current date.
#' @param end_date The end date for retrieving duration data. Defaults to the current date.
#' @param cvm_date Logical indicating whether to use CVM date format.
#' @param cetip_code The CETIP code for the desired duration data. Defaults to NULL.
#'
#' @return A data frame containing the duration data for the specified date range and optional CETIP code.
#'
#' @examples
#' # Retrieve duration data for the last 21 days
#' duration_data <- get_duration()
#'
#' # Retrieve duration data for a specific date range and CETIP code
#' duration_data <- get_duration(start_date = "2022-01-01", end_date = "2022-12-31", cetip_code = "ABC123")
#'
#' @importFrom utils read.table
#'
#' @keywords debentures finance duration data
#' @export
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
  df <- df[c("V1","V2","V4","V6","V7")]
  df <- get_numeric_cols(df, num_cols=c("V7"))
  df <- get_date_cols(df, date_cols=c("V4","V6"))
  names(df) <- c("issuer", "ticker", "issuing_date", "maturity_date", "duration_years")
  return(df)
}


#' Get Events Data by Date Range and Payment Dates
#'
#' This function retrieves events data from the Debentures website for a specified date range and payment dates.
#'
#' @param start_date The start date for retrieving events data. Defaults to the current date.
#' @param end_date The end date for retrieving events data. Defaults to 5 days after the current date.
#' @param payment_start_date The start date for payment dates. Defaults to NULL.
#' @param payment_end_date The end date for payment dates. Defaults to NULL.
#' @param cetip_code The CETIP code for the desired events data. Defaults to NULL.
#' @param event The type of event to filter. Defaults to NULL.
#'
#' @return A data frame containing the events data for the specified date range and optional parameters.
#'
#' @examples
#' # Retrieve events data for the next 5 days
#' events_data <- get_events()
#'
#' # Retrieve events data for a specific date range, payment dates, and CETIP code
#' events_data <- get_events(start_date = "2022-01-01", end_date = "2022-12-31", payment_start_date = "2022-02-01", payment_end_date = "2022-02-28", cetip_code = "ABC123", event = "Juros")
#'
#' @importFrom utils read.table
#'
#' @keywords debentures finance events data
#' @export
get_events <- function(start_date = Sys.Date(),
                       end_date = Sys.Date()+5,
                       payment_start_date = NULL,
                       payment_end_date = NULL,
                       cetip_code = NULL,
                       event = NULL){
  url <- "http://www.debentures.com.br/exploreosnd/consultaadados/eventosfinanceiros/agenda_e.asp?emissor=&ativo=%s&evento=%s&dt_ini=%s&dt_fim=%s&dt_pgto_ini=%s&dt_pgto_fim=%s&Submit32.x=&Submit32.y="
  start_date <- ifelse(is.null(start_date),'',base::format(base::as.Date(start_date), "%d/%m/%Y"))
  end_date <- ifelse(is.null(end_date),'',base::format(base::as.Date(end_date), "%d/%m/%Y"))
  url <- sprintf(url, ifelse(is.null(cetip_code), "", cetip_code),
                 ifelse(is.null(event), "", event),
                 ifelse(is.null(start_date), "", start_date),
                 ifelse(is.null(end_date), "", end_date),
                 ifelse(is.null(payment_start_date), "", payment_start_date),
                 ifelse(is.null(payment_end_date), "", payment_end_date))
  res <- request_data(url=url)
  content <- get_request_content(res=res)
  txt <- base::readLines(base::textConnection(content))
  df <- utils::read.table(base::textConnection(txt, "r"), sep = "\t", header = FALSE, skip = 3)
  df <- df[c("V1","V2","V3","V4","V5","V6","V7","V8")]
  df <- get_numeric_cols(df, num_cols=c("V7"))
  df <- get_date_cols(df, date_cols=c("V1","V2"))
  names(df) <- c("event_date", "payment_date", "issuer", "ticker", "event", "type", "interest_perc", "liquidation")
  return(df)
}


#' Get Events Prices Data by Date Range
#'
#' This function retrieves events prices data from the Debentures website for a specified date range.
#'
#' @param start_date The start date for retrieving events prices data. Defaults to 21 days ago from the current date.
#' @param end_date The end date for retrieving events prices data. Defaults to the current date.
#' @param cetip_code The CETIP code for the desired events prices data. Defaults to NULL.
#' @param event The type of event to filter. Defaults to NULL.
#'
#' @return A data frame containing the events prices data for the specified date range and optional CETIP code.
#'
#' @examples
#' # Retrieve events prices data for the last 21 days
#' events_prices_data <- get_events_prices()
#'
#' # Retrieve events prices data for a specific date range and CETIP code
#' events_prices_data <- get_events_prices(start_date = "2022-01-01", end_date = "2022-12-31", cetip_code = "ABC123", event = "Juros")
#'
#' @importFrom utils read.table
#'
#' @keywords debentures finance events prices data
#' @export
get_events_prices <- function(start_date = Sys.Date()-21,
                              end_date = Sys.Date(),
                              cetip_code = NULL,
                              event = NULL){
  if (!is.null(event) && !event %in% c("Juros", "Amortização", "Prêmio")) {
    stop("Invalid event selected.")
  }
  event <- list(
    "Juros" = 1,
    "Amortização" = 11,
    "Prêmio" = 3)
  url <- "http://www.debentures.com.br/exploreosnd/consultaadados/eventosfinanceiros/pudeeventos_e.asp?op_exc=False&emissor=&ativo=%s&dt_ini=%s&dt_fim=%s&evento=%s&Submit.x=&Submit.y="
  start_date <- base::format(base::as.Date(start_date), "%d/%m/%Y")
  end_date <- base::format(base::as.Date(end_date), "%d/%m/%Y")
  url <- base::sprintf(url, ifelse(is.null(cetip_code), "", cetip_code), start_date, end_date, ifelse(is.null(event), "", event))
  res <- request_data(url=url)
  content <- get_request_content(res=res)
  txt <- base::readLines(base::textConnection(content))
  df <- utils::read.table(base::textConnection(txt, "r"), sep = "\t", header = FALSE, skip = 3)
  df <- df[c("V1","V2","V3","V4","V5","V6")]
  df <- get_numeric_cols(df, num_cols=c("V4"))
  df <- get_date_cols(df, date_cols=c("V1"))
  names(df) <- c("event_date", "ticker", "event", "price", "situation", "liquidation")
  return(df)

}



#' Get Issuing Volume Data by Date Range
#'
#' This function retrieves issuing volume data from the Debentures website for a specified date range.
#'
#' @param start_date The start date for retrieving issuing volume data. Defaults to 21 days ago from the current date.
#' @param end_date The end date for retrieving issuing volume data. Defaults to 1 day ago from the current date.
#' @param date_type A character vector specifying the date type ('issuing', 'registering'). Defaults to 'issuing'.
#' @param cvm_instruction A numeric vector specifying the CVM instructions (400, 476). Defaults to c(400, 476).
#' @param as A character vector specifying the desired output format ('tibble', 'xts', 'ts', 'data.frame', 'text'). Defaults to 'data.frame'.
#'
#' @return A data frame containing issuing volume data for the specified date range, date type, and CVM instructions.
#'
#' @examples
#' # Retrieve issuing volume data for the last 21 days
#' issuing_volume_data <- get_issuing_volume_by_date()
#'
#' # Retrieve issuing volume data for a specific date range and CVM instructions
#' issuing_volume_data <- get_issuing_volume_by_date(start_date = "2022-01-01", end_date = "2022-12-31", date_type = "registering", cvm_instruction = c(400, 476))
#'
#' @importFrom utils read.table
#'
#' @keywords debentures finance issuing volume data
#' @export
get_issuing_volume_by_date <- function(start_date = Sys.Date()-21,
                                       end_date = Sys.Date()-1,
                                       date_type = 'issuing',
                                       cvm_instruction = c(400, 476),
                                       as = c("tibble", "xts", "ts", "data.frame", "text")){
  if (!date_type %in% c("issuing","registering")) {
    stop("Invalid date type selected.")
  }
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
  df <- get_numeric_cols(df=df, num_cols = c("V7"))
  df <- get_date_cols(df=df, date_cols = c("V4","V5","V6"))
  names(df) <- c("ticker", "issuer", "situation", "issuing_date", "registering_date_snd", "registering_date_cvm", "vol")
  return(df)
}
