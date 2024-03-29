% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_annual.R
\encoding{UTF-8}
\name{get_annual}
\alias{get_annual}
\title{Get data on annual market expectations for economic indicators (Focus/BCB)}
\usage{
get_annual(
  indicator = NULL,
  detail = NULL,
  first_date = Sys.Date() - 2 * 365,
  last_date = Sys.Date(),
  reference_date = NULL,
  be_quiet = FALSE,
  use_memoise = TRUE,
  do_parallel = FALSE
)
}
\arguments{
\item{indicator}{A single string or a character vector with economic indicators names, see details for possible values. Defaults to NULL.}

\item{detail}{A single string. See details for possible values. Defaults to NULL.}

\item{first_date}{A single string or Date in "YYYY-mm-dd" format. Default for 2 years prior to the current date.}

\item{last_date}{A single string or Date in "YYYY-mm-dd". format Default is the current date.}

\item{reference_date}{A single string in "YYYY" format, indicating the reference year for which the statistic is expected. Defaults to NULL.}

\item{be_quiet}{Logical. Should messages or warnings not be displayed? Defaults to FALSE.}

\item{use_memoise}{Logical. Sets the use of caching system, creating a folder named "cache_bcb" by default. Default to TRUE.}

\item{do_parallel}{Logical. For using parallel data importation. Defaults to FALSE.}
}
\value{
A tibble with data from BCB-Olinda Data Services.
}
\description{
This function provides the extraction of data and statistics related to the expectations of economic indicators, specifically the annual market expectations, made available by the Central Bank of Brazil's Expectations System through the Olinda API. The data comes from several financial institutions: banks, funds, research houses, etc. Important: arguments are case sensitive.
}
\details{
For periods for which there are no statistics, they will be omitted from the query.

Possible values for indicator argument: 'Balança comercial', 'Conta corrente', 'Investimento direto no país', 'Dívida bruta do governo geral', 'Dívida líquida do setor público', 'Resultado primário', 'Resultado nominal', 'IGP-DI', 'IGP-M', 'INPC', 'IPA-DI', 'IPA-M', 'IPCA', 'IPCA-15', 'IPC-Fipe', 'IPCA Administrados', 'IPCA Alimentação no domicílio', 'IPCA Bens industrializados', 'IPCA Livres', 'IPCA Serviços', 'Produção industrial', 'PIB Agropecuária', 'PIB Indústria', 'PIB Serviços', 'PIB Total', 'PIB Despesa de consumo da administração pública', 'PIB Despesa de consumo das famílias', 'PIB Exportação de bens e serviços', 'PIB Formação Bruta de Capital Fixo', 'PIB Importação de bens e serviços', 'Taxa de desocupação', 'Selic', 'Câmbio'.

Possible values for detail argument: if indicator 'Balança comercial' -> 'Exportações', 'Importações', 'Saldo'.
}
\examples{
df <- get_annual(
  indicator = "IPCA",
  first_date = Sys.Date() - 30,
  reference_date = format(Sys.Date(), "\%Y"),
  use_memoise = FALSE
)
}
\author{
Fernando da Silva <\href{mailto:fernando@fortietwo.com}{fernando@fortietwo.com}>
}
