
<!-- README.md is generated from README.Rmd. Please edit that file -->

# meedr <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/meedr)](https://CRAN.R-project.org/package=meedr)
[![R-CMD-check](https://github.com/schoulten/meedr/workflows/R-CMD-check/badge.svg)](https://github.com/schoulten/meedr/actions)
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
# install.packages("remotes")
remotes::install_github("schoulten/meedr")
```

## Features

-   [get_monthly()](#get_monthly): Get data on monthly market
    expectations
-   [get_quarterly()](#get_quarterly): Get data on quarterly market
    expectations
-   [get_annual()](#get_annual): Get data on annual market expectations
-   [get_inflation_12m()](#get_inflation_12m): Get data on market
    expectations for inflation over the next 12 months
-   [get_monthly_top5()](#get_monthly_top5): Get data on monthly market
    expectations for the Top 5 indicators
-   [get_annual_top5()](#get_annual_top5): Get data on annual market
    expectations for the Top 5 indicators
-   [get_selic()](#get_selic): Get Selic market expectations data

## Example

These are some basic examples of using the package:

### get_monthly()

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
#> # A tibble: 5 × 10
#>   indicator date       reference_date   mean median    sd    min   max
#>   <chr>     <date>     <chr>           <dbl>  <dbl> <dbl>  <dbl> <dbl>
#> 1 IPCA      2022-08-05 08/2022        -0.149 -0.15  0.180 -0.580  0.35
#> 2 IPCA      2022-08-05 08/2022        -0.125 -0.15  0.197 -0.580  0.55
#> 3 IPCA      2022-08-04 08/2022        -0.157 -0.17  0.181 -0.840  0.35
#> 4 IPCA      2022-08-04 08/2022        -0.117 -0.135 0.207 -0.840  0.55
#> 5 IPCA      2022-08-03 08/2022        -0.154 -0.165 0.182 -0.840  0.35
#> # … with 2 more variables: n_respondents <int>, basis <int>
```

### get_quarterly()

``` r
# Quarterly market expectations for GDP indicator
meedr::get_quarterly(
  indicator      = "PIB Total",
  first_date     = "2021-01-01",
  reference_date = paste0(lubridate::quarter(Sys.Date()), "/", lubridate::year(Sys.Date())),
  be_quiet       = TRUE
  )
```

### get_annual()

``` r
# Annual market expectations for SELIC and exchange rate (BRL) indicator
meedr::get_annual(
  indicator      = c("Selic", "Câmbio"),
  reference_date = format(Sys.Date(), "%Y"),
  be_quiet       = TRUE
  )
```

### get_inflation_12m()

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

### get_monthly_top5()

``` r
# Monthly market expectations for IGP-M indicator (Top 5 Focus)
meedr::get_monthly_top5(
  indicator  = "IGP-M",
  first_date = NULL, # get all data to current date
  calc_type  = "long",
  be_quiet   = TRUE
  )
```

### get_annual_top5()

``` r
# Annual market expectations for SELIC indicator (Top 5 Focus)
meedr::get_annual_top5(
  indicator   = "Selic",
  detail      = "Fim do ano", # argument deprecated by API BCB-Olinda
  be_quiet    = TRUE,
  use_memoise = FALSE # disable caching system
  )
```

### get_selic()

``` r
df1 <- get_selic(indicator = "Selic", first_date = Sys.Date() - 30)
#> 
#> Fetching [Selic] data from BCB-Olinda...
#> 
#> Found 576 observations!
df1
#> # A tibble: 576 × 10
#>    indicator date       meeting  mean median    sd   min   max n_respondents
#>    <chr>     <date>     <chr>   <dbl>  <dbl> <dbl> <dbl> <dbl>         <int>
#>  1 Selic     2022-08-05 R5/2024  8.91    9   0.854   7    10              14
#>  2 Selic     2022-08-05 R5/2024  8.91    9   0.854   7    10              14
#>  3 Selic     2022-08-05 R4/2024  8.87    9   0.894   7    10.5            30
#>  4 Selic     2022-08-05 R4/2024  9.12    9   0.987   7    12.2           107
#>  5 Selic     2022-08-05 R3/2024  9.34    9.5 0.949   7.5  11              33
#>  6 Selic     2022-08-05 R3/2024  9.52    9.5 0.991   7.5  12.5           112
#>  7 Selic     2022-08-05 R2/2024  9.81   10   0.894   8    11.5            33
#>  8 Selic     2022-08-05 R2/2024  9.97   10   0.980   7.5  12.8           112
#>  9 Selic     2022-08-05 R1/2024 10.4    10.4 0.934   8.5  13              34
#> 10 Selic     2022-08-05 R1/2024 10.4    10.5 0.968   8.5  13             113
#> # … with 566 more rows, and 1 more variable: basis <int>

df2 <- get_selic(indicator = "Selic", first_date = NULL, meeting = "R1/2024")
#> 
#> Fetching [Selic] data from BCB-Olinda...
#> 
#> Found 254 observations!
df2
#> # A tibble: 254 × 10
#>    indicator date       meeting  mean median    sd   min   max n_respondents
#>    <chr>     <date>     <chr>   <dbl>  <dbl> <dbl> <dbl> <dbl>         <int>
#>  1 Selic     2022-08-05 R1/2024  10.4   10.4 0.934  8.5   13              34
#>  2 Selic     2022-08-05 R1/2024  10.4   10.5 0.968  8.5   13             113
#>  3 Selic     2022-08-04 R1/2024  10.5   10.5 0.933  8.5   12.5            63
#>  4 Selic     2022-08-04 R1/2024  10.4   10.5 0.954  8.5   13             113
#>  5 Selic     2022-08-03 R1/2024  10.5   10.5 1.04   7.25  13              64
#>  6 Selic     2022-08-03 R1/2024  10.4   10.5 0.990  7.25  13             113
#>  7 Selic     2022-08-02 R1/2024  10.5   10.5 1.06   7.25  13              67
#>  8 Selic     2022-08-02 R1/2024  10.4   10.5 0.990  7.25  13             113
#>  9 Selic     2022-08-01 R1/2024  10.5   10.5 1.07   7.25  13              70
#> 10 Selic     2022-08-01 R1/2024  10.4   10.5 0.989  7.25  13             113
#> # … with 244 more rows, and 1 more variable: basis <int>
```

## Related works

Check out some similar works:

-   [rbcb](https://github.com/wilsonfreitas/rbcb): R interface to
    Brazilian Central Bank web services by @wilsonfreitas
