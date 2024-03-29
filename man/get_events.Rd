% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scraper.R
\name{get_events}
\alias{get_events}
\title{Get Events Data by Date Range and Payment Dates}
\usage{
get_events(
  start_date = Sys.Date(),
  end_date = Sys.Date() + 5,
  payment_start_date = NULL,
  payment_end_date = NULL,
  cetip_code = NULL,
  event = NULL
)
}
\arguments{
\item{start_date}{The start date for retrieving events data. Defaults to the current date.}

\item{end_date}{The end date for retrieving events data. Defaults to 5 days after the current date.}

\item{payment_start_date}{The start date for payment dates. Defaults to NULL.}

\item{payment_end_date}{The end date for payment dates. Defaults to NULL.}

\item{cetip_code}{The CETIP code for the desired events data. Defaults to NULL.}

\item{event}{The type of event to filter. Options include: ('Juros', 'Repactuação', 'Atualização',
'Amortização', 'Prêmio', 'Resgate Total Antecipado', 'Participação', 'Opção de Venda',
'Prêmio de Permanência', 'Vencimento'). Defaults to all events.}
}
\value{
A data frame containing the events data for the specified date range and optional parameters.
}
\description{
This function retrieves events data from the debentures.com.br website for a specified date range and payment dates.
}
\examples{
# Retrieve events data for the next 5 days
events_data <- get_events()

# Retrieve events data for a specific date range, payment dates, and CETIP code
events_data <- get_events(start_date = "2022-01-01", end_date = "2022-12-31", payment_start_date = "2022-02-01", payment_end_date = "2022-02-28", cetip_code = "AALM11", event = "Juros")

}
\keyword{data}
\keyword{debentures}
\keyword{events}
\keyword{finance}
