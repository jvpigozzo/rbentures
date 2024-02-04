#' Request data from a given URL
#'
#' This function sends an HTTP GET request to the specified URL and returns the response.
#'
#' @param url The URL to request data from
#' @return The HTTP response object
#' @examples
#' \dontrun{
#' # Example usage:
#' url <- "https://example.com/data"
#' response <- request_data(url)
#' }
request_data <- function(url){
  res <- try(httr::GET(url), silent = TRUE)
  if (!is(res, "try-error")) {
    return(res)
  } else {
    message("Invalid date range")
  }
}



#' Get content from an HTTP response object
#'
#' Extracts content from an HTTP response object and returns it.
#'
#' @param res The HTTP response object
#' @param encoding The character encoding to be used (default is "latin1")
#' @return Extracted content
#' @examples
#' \dontrun{
#' # Example usage:
#' url <- "https://example.com/data"
#' response <- request_data(url)
#' content <- get_request_content(response)
#' }
get_request_content <- function(res, encoding = "latin1"){
  content <- httr::content(res, as = "text", encoding = encoding)
  return(content)
}



#' Convert specified columns to numeric format
#'
#' Takes a data frame and converts specified numeric columns by removing periods and commas,
#' and then coerces them into numeric format.
#'
#' @param df The input data frame
#' @param num_cols Character vector of numeric column names
#' @return Data frame with specified columns converted to numeric format
#' @examples
#' \dontrun{
#' # Example usage:
#' data <- data.frame(x = c("1,000.25", "2,500.75"), y = c("3,000", "4,500"))
#' numeric_data <- get_numeric_cols(data, c("x", "y"))
#' }
get_numeric_cols <- function(df, num_cols){
  df <- df |>
    dplyr::mutate_at(dplyr::vars(dplyr::matches(num_cols)), ~gsub('[.]', '', .)) |>
    dplyr::mutate_at(dplyr::vars(dplyr::matches(num_cols)), ~gsub('[-]', '', .)) |>
    dplyr::mutate_at(dplyr::vars(dplyr::matches(num_cols)), ~gsub('[,]', '.', .))|>
    dplyr::mutate_at(dplyr::vars(dplyr::matches(num_cols)), as.numeric)
  return(df)
}



#' Convert specified columns to Date format
#'
#' Takes a data frame and converts specified date columns to Date format using the specified date format.
#'
#' @param df The input data frame
#' @param date_cols Character vector of date column names
#' @return Data frame with specified columns converted to Date format
#' \dontrun{
#' # Example usage:
#' data <- data.frame(date1 = c("01/01/2022", "02/01/2022"), date2 = c("03/01/2022", "04/01/2022"))
#' date_data <- get_date_cols(data, c("date1", "date2"))
#' }
get_date_cols <- function(df, date_cols){
  df <- df |>
    dplyr::mutate_at(dplyr::vars(dplyr::matches("^date_cols$")), ~as.Date(., format = "%d/%m/%Y"))
  return(df)
}
