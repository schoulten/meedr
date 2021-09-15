
<!-- README.md is generated from README.Rmd. Please edit that file -->

# meedr <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/meedr)](https://CRAN.R-project.org/package=meedr)
[![R-CMD-check](https://github.com/schoulten/meedr/workflows/R-CMD-check/badge.svg)](https://github.com/schoulten/meedr/actions)
[![Travis build
status](https://travis-ci.com/schoulten/meedr.svg?branch=main)](https://travis-ci.com/schoulten/meedr)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/schoulten/meedr?branch=main&svg=true)](https://ci.appveyor.com/project/schoulten/meedr)
[![CRAN
downloads](http://cranlogs.r-pkg.org/badges/grand-total/meedr?color=green)](https://cran.r-project.org/package=meedr)
<!-- badges: end -->

The goal of **meedr** is to provide quick and easy access to market
expectations data to the main macroeconomic indicators in the Focus
report, made available by the **Central Bank of Brazil** through the
Expectations System data [API](https://dadosabertos.bcb.gov.br/). This
data comes from several financial institutions, such as: banks, brokers,
funds, consultancies, etc.

**Warning**: This package was definitively archived by CRAN on July 22,
2021 and is only available in this repository. Despite this, maintenance
and bug fixes will continue to be done.

The **meedr** package offers an R interface to the API and other
advantages:

-   Use of a caching system with package `memoise` to speed up repeated
    requests of data;
-   User can utilize all cores of the machine (parallel computing) when
    fetching a large batch of time series.

Check the [meedr pkgdown page](https://fortietwo.com/meedr/) for more
general information ;)

## Installation

You can install the development version from
[GitHub](https://github.com/schoulten/meedr) with:

``` r
# install.packages("devtools")
devtools::install_github("schoulten/meedr")
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
library(meedr)

# Monthly market expectations for IPCA indicator
ipca <- meedr::get_monthly(
  indicator      = "IPCA",
  first_date     = Sys.Date()-30,
  reference_date = format(Sys.Date(), "%m/%Y"),
  be_quiet       = TRUE
  )

head(ipca, 5)
#> # A tibble: 5 x 10
#>   indicator date       reference_date  mean median    sd coef_var   min   max
#>   <chr>     <date>     <chr>          <dbl>  <dbl> <dbl>    <dbl> <dbl> <int>
#> 1 IPCA      2021-09-10 09/2021        0.875  0.915 0.179    0.42   1.13    74
#> 2 IPCA      2021-09-10 09/2021        0.763  0.77  0.228    0.277  1.13   121
#> 3 IPCA      2021-09-09 09/2021        0.800  0.81  0.179    0.42   1.13    69
#> 4 IPCA      2021-09-09 09/2021        0.698  0.72  0.207    0.277  1.13   121
#> 5 IPCA      2021-09-08 09/2021        0.754  0.75  0.148    0.33   1.01    61
#> # ... with 1 more variable: n_respondents <int>
```

### get\_quarterly()

``` r
# Quarterly market expectations for GDP indicator
meedr::get_quarterly(
  indicator      = "PIB Total",
  first_date     = "2021-01-01",
  reference_date = paste0(lubridate::quarter(Sys.Date()), "/", lubridate::year(Sys.Date())),
  be_quiet       = TRUE
  )
```

### get\_annual()

``` r
# Annual market expectations for SELIC and exchange rate (BRL) indicator
meedr::get_annual(
  indicator      = c("Selic", "Câmbio"),
  reference_date = format(Sys.Date(), "%Y"),
  be_quiet       = TRUE
  )
```

### get\_inflation\_12m()

``` r
# Inflation over the next 12 months
# First, and a suggestion, run this for using parallel computing:
future::plan(future::multisession, workers = floor(future::availableCores()/2))
meedr::get_inflation_12m(
  indicator   = c("IGP-DI", "IGP-M", "INPC", "IPA-DI", "IPA-M", "IPCA", "IPCA-15", "IPC-Fipe"),
  smoothed    = "yes",
  be_quiet    = FALSE, # display messages
  do_parallel = TRUE # turn on parallel computing
  )
```

### get\_monthly\_top5()

``` r
# Monthly market expectations for IGP-M indicator (Top 5 Focus)
meedr::get_monthly_top5(
  indicator  = "IGP-M",
  first_date = NULL, # get all data to current date
  calc_type  = "long",
  be_quiet   = TRUE
  )
```

### get\_annual\_top5()

``` r
# Annual market expectations for SELIC indicator (Top 5 Focus)
meedr::get_annual_top5(
  indicator   = "Selic",
  detail      = "Fim do ano", # argument deprecated by API BCB-Olinda
  be_quiet    = TRUE,
  use_memoise = FALSE # disable caching system
  )
```

## Related works

Check out some similar works:

-   [rbcb](https://github.com/wilsonfreitas/rbcb): R interface to
    Brazilian Central Bank web services by @wilsonfreitas
