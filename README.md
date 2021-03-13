
<!-- README.md is generated from README.Rmd. Please edit that file -->

# expectations <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->
<!-- badges: end -->

The goal of **expectations** is to provide quick and easy access to
market expectations data to the main macroeconomic indicators in the
Focus report, made available by the **Central Bank of Brazil** through
the Expectations System data
[API](https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/aplicacao#!/recursos).
This data comes from several financial institutions, such as: banks,
brokers, funds, consultancies, etc.

The **expectations** package offers an R interface to the API and other
advantages:

-   Use of a caching system with package `memoise` to speed up repeated
    requests of data;
-   User can utilize all cores of the machine (parallel computing) when
    fetching a large batch of time series.

## Installation

<!--
You can install the released version of expectations from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("expectations")
```
-->

You can install the development version from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("schoulten/expectations")
```

## Features

-   [get\_monthly()](#get_monthly): Get data on monthly market
    expectations
-   [get\_quarterly()](#get_quarterly): Get data on quarterly market
    expectations
-   [get\_annual()](#get_annual): Get data on annual market expectations
-   [get\_inflation\_12m()](#get_inflation_12m): Get data on market
    expectations for inflation over the next 12 months
-   [get\_monthly\_top5()](#get_monthly_top5): Get data on monthly
    market expectations for the Top 5 indicators
-   [get\_annual\_top5()](#get_annual_top5): Get data on annual market
    expectations for the Top 5 indicators

## Example

These are some basic examples of using the package:

### get\_monthly()

``` r
library(expectations)

# Monthly market expectations for IPCA indicator
expectations::get_monthly(
  indicator      = "IPCA",
  first_date     = Sys.Date()-30,
  reference_date = format(Sys.Date(), "%m/%Y"),
  be_quiet       = TRUE
  )
```

### get\_quarterly()

``` r
# Quarterly market expectations for GDP indicator
expectations::get_quarterly(
  indicator      = "PIB Total",
  first_date     = "2021-01-01",
  reference_date = paste0(lubridate::quarter(Sys.Date()), "/", lubridate::year(Sys.Date())),
  be_quiet       = TRUE
  )
```

### get\_annual()

``` r
# Annual market expectations for SELIC and exchange rate (BRL) indicator
expectations::get_annual(
  indicator      = c("Meta para taxa over-selic", "Taxa de câmbio"),
  reference_date = format(Sys.Date(), "%Y"),
  be_quiet       = TRUE
  )
```

### get\_inflation\_12m()

``` r
# Inflation over the next 12 months
# First, and a suggestion, run this for using parallel computing:
future::plan(future::multisession, workers = floor(future::availableCores()/2))
expectations::get_inflation_12m(
  indicator   = c("IGP-DI", "IGP-M", "INPC", "IPA-DI", "IPA-M", "IPCA", "IPCA-15", "IPC-FIPE"),
  smoothed    = "yes",
  be_quiet    = FALSE, # display messages
  do_parallel = TRUE # turn on parallel computing
  )
```

### get\_monthly\_top5()

``` r
# Monthly market expectations for IGP-M indicator (Top 5 Focus)
expectations::get_monthly_top5(
  indicator  = "IGP-M",
  first_date = NULL, # get all data to current date
  calc_type  = "long",
  be_quiet   = TRUE
  )
```

### get\_annual\_top5()

``` r
# Annual market expectations for SELIC indicator (Top 5 Focus)
expectations::get_annual_top5(
  indicator   = "Meta para taxa over-selic",
  detail      = "Fim do ano",
  be_quiet    = TRUE,
  use_memoise = FALSE # disable caching system
  )
```
