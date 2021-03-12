#' Get data on annual market expectations for economic indicators (Focus/BCB)
#'
#' @param indicator A single string or a character vector with economic indicators names, see details for possible values. Defaults to NULL.
#' @param detail A single string. See details for possible values. Defaults to NULL.
#' @param first_date A single string or Date in "YYYY-mm-dd" format. Default for 2 years prior to the current date.
#' @param last_date A single string or Date in "YYYY-mm-dd". format Default is the current date.
#' @param reference_date A single string in "YYYY" format, indicating the reference year for which the statistic is expected. Defaults to NULL.
#' @param be_quiet Logical. Should messages or warnings not be displayed? Defaults to FALSE.
#' @param use_memoise Logical. Sets the use of caching system, creating a folder named "cache_bcb" by default. Default to TRUE.
#' @param do_parallel Logical. For using parallel data importation. Defaults to FALSE.
#'
#' @return A tibble with data from BCB-Olinda Data Services.
#' @description This function provides the extraction of data and statistics related to the expectations of economic indicators, specifically the annual market expectations, made available by the Central Bank of Brazil's Expectations System through the Olinda API. The data comes from several financial institutions: banks, funds, research houses, etc. Important: arguments are case sensitive.
#' @details For periods for which there are no statistics, they will be omitted from the query.
#'
#' Possible values for indicator argument: "Balança Comercial", "Balanço de Pagamentos", "Fiscal", "IGP-DI", "IGP-M", "INPC", "IPA-DI", "IPA-M", "IPCA", "IPCA-15", "IPC-FIPE", "Preços administrados por contrato e monitorados", "Produção industrial", "PIB Agropecuária", "PIB Industrial", "PIB Serviços", "PIB Total", "Meta para taxa over-selic", "Taxa de câmbio".
#'
#' Possible values for detail argument: if indicator "Balança Comercial" -> "Exportações", "Importações", "Saldo"; if indicator "Balanço de Pagamentos" -> "Conta corrente", "Investimento direto no país"; if indicator "Fiscal" -> "Resultado Primário", "Resultado Nominal", "Dívida líquida do setor público"; if indicator "Meta para taxa over-selic" -> "Fim do ano", "Média do ano".
#' @author Fernando da Silva <<fernando@fortietwo.com>>
#' @encoding UTF-8
#' @export
#'
#' @examples
#' df <- get_annual(
#'   indicator = "IPCA",
#'   reference_date = format(Sys.Date(), "%Y"),
#'   use_memoise = FALSE
#' )
get_annual <- function (
  indicator      = NULL,
  detail         = NULL,
  first_date     = Sys.Date() - 2*365,
  last_date      = Sys.Date(),
  reference_date = NULL,
  be_quiet       = FALSE,
  use_memoise    = TRUE,
  do_parallel    = FALSE
  ){
  valid_indicator <- c(
    "Balan\u00e7a Comercial", "Balan\u00e7o de Pagamentos", "Fiscal", "IGP-DI",
    "IGP-M", "INPC", "IPA-DI", "IPA-M", "IPCA", "IPCA-15", "IPC-FIPE",
    "Pre\u00e7os administrados por contrato e monitorados", "Produ\u00e7\u00e3o industrial",
    "PIB Agropecu\u00e1ria", "PIB Industrial", "PIB Servi\u00e7os", "PIB Total",
    "Meta para taxa over-selic", "Taxa de c\u00e2mbio"
  )

  if (missing(indicator) | !all(indicator %in% valid_indicator) | is.null(indicator)) {
    stop("\nArgument 'indicator' is not valid or missing. Check your inputs.", call. = FALSE)
  } else indicator

  valid_detail <- c(
    "Balan\u00e7a Comercial / Exporta\u00e7\u00f5es", "Balan\u00e7a Comercial / Importa\u00e7\u00f5es",
    "Balan\u00e7a Comercial / Saldo", "Balan\u00e7o de Pagamentos / Conta corrente",
    "Balan\u00e7o de Pagamentos / Investimento direto no pa\u00eds",
    "Fiscal / Resultado Prim\u00e1rio", "Fiscal / Resultado Nominal",
    "Fiscal / D\u00edvida l\u00edquida do setor p\u00fablico", "Meta para taxa over-selic / Fim do ano",
    "Meta para taxa over-selic / M\u00e9dia do ano"
  )

  if (!is.null(detail) && !is.na(detail)) {
    if ((class(detail) != "character")) {
      stop("\nArgument 'detail' is not valid. Check your inputs.", call. = FALSE)
    } else if
    (!all(paste0(indicator, " / ", detail) %in% valid_detail)) {
      stop("\nArgument 'detail' is not valid. Check your inputs.", call. = FALSE)
    } else if
    (length(detail) > 1) {
      stop("\nArgument 'detail' is not valid. Check your inputs.", call. = FALSE)
    } else
      detail
  } else if
  ((length(detail) > 0) && is.na(detail)) {
    detail <- NULL
  } else detail

  first_date <- try(as.Date(first_date), silent = TRUE)
  if (length(first_date) <= 0 || is.na(first_date)) {first_date = NULL}
  if (class(first_date) %in% "try-error") {
    stop("\nArgument 'first_date' is not a valid date.", call. = FALSE)
  }
  if (missing(first_date)) {first_date = Sys.Date() - 10 * 365} else
    first_date

  last_date <- try(as.Date(last_date), silent = TRUE)
  if (length(last_date) <= 0 || is.na(last_date)) {last_date = NULL}
  if (class(last_date) %in% "try-error") {
    stop("\nArgument 'last_date' is not a valid date.", call. = FALSE)
  }
  if (missing(last_date)) {last_date = Sys.Date() - 10 * 365} else
    last_date

  if ((length(first_date) > 0) && first_date > Sys.Date()) {
    stop("\nIt seems that 'first_date' > current date. Check your inputs.", call. = FALSE)
  }

  if (!is.null(last_date)) {
    if ((length(first_date) > 0) && last_date < first_date) {
      stop("\nIt seems that 'last_date' < first_date. Check your inputs.", call. = FALSE)
    }
  }

  if (!is.null(reference_date) && !is.na(reference_date)) {
    if ((class(reference_date) != "character")) {
      stop("\nArgument 'reference_date' is not valid. Check your inputs.", call. = FALSE)
    } else if
    (nchar(reference_date) == 4L & grepl("[[:digit:]]+$", reference_date)) {
      reference_date <- as.character(reference_date)
    } else
      stop("\nArgument 'reference_date' is not valid. Check yout inputs.", call. = FALSE)
  } else if
  (is.na(reference_date) && (length(reference_date) > 0)) {reference_date <- NULL} else
    reference_date

  if ((class(do_parallel) != "logical") || (is.na(do_parallel))) {
    stop("\nArgument 'do_parallel' must be logical. Check your inputs.", call. = FALSE)
  } else if

  ((class(be_quiet) != "logical") || (is.na(be_quiet))) {
    stop("\nArgument 'be_quiet' must be logical. Check your inputs.", call. = FALSE)
  }

  foo_args <- paste0(
    paste0("(", paste(sprintf("Indicador eq '%s'", indicator), collapse = " or ", sep = ""), ")"),
    sprintf(" and IndicadorDetalhe eq '%s'", detail),
    sprintf(" and Data ge '%s'", first_date),
    sprintf(" and Data le '%s'", last_date),
    sprintf(" and DataReferencia eq '%s'", reference_date)
  )

  odata_url <- list(
    httr::modify_url(
      "https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativasMercadoAnuais",
      query = list(
        `$filter`  = foo_args,
        `$format`  = "json",
        `$orderby` = "Data desc"
      )
    )
  )

  if ((class(use_memoise) != "logical") || (is.na(use_memoise))) {
    stop("\nArgument 'use_memoise' must be logical. Check your inputs.", call. = FALSE)
  } else
    memoising <- function(use_memoise, cache_dir) {
      if (use_memoise) {
        foo_memoise <- memoise::memoise(f = jsonlite::fromJSON, cache = cache_dir)
      } else
        foo_memoise <- jsonlite::fromJSON
    }

  from_bcb <- memoising(
    use_memoise = use_memoise,
    cache_dir   = memoise::cache_filesystem("./cache_bcb")
  )

  if (!do_parallel) {

    if (be_quiet) {message("", appendLF = FALSE)} else {
      message(
        paste0("\nFetching [", paste(indicator, collapse = ", "), "] data ", "from BCB-Olinda... \n"),
        appendLF = FALSE
      )
    }

    df <- try(
      suppressWarnings(purrr::pmap(.l = odata_url, .f = from_bcb)[[1]][["value"]]),
      silent = TRUE
    )
  } else {
    formals_parallel <- formals(future::plan())
    used_workers <- formals_parallel$workers
    available_cores <- future::availableCores()
    msg <- utils::capture.output(future::plan())
    flag <- grepl("sequential", msg)[1]
    if (flag) {
      stop(paste0(
        "When using do_parallel = TRUE, you need to call future::plan() to configure your parallel settings.\n",
        "A suggestion, write the following lines just before:\n\n",
        "future::plan(future::multisession, workers = floor(future::availableCores()/2))", "\n\n",
        "Notice it will use half of your available cores so that your OS has some room to breathe."),
        call. = FALSE
      )
    } else if
    (be_quiet) {message("", appendLF = FALSE)} else
      message(
        paste0("\nRunning parallel with ", used_workers, " cores (", available_cores, " available)\n",
               "\nFetching [", paste(indicator, collapse = ", "), "] data ", "from BCB-Olinda... \n"),
        appendLF = TRUE
      )

    df <- try(
      suppressWarnings(furrr::future_pmap(.l = odata_url, .f = from_bcb)[[1]][["value"]]),
      silent = TRUE
    )
  }

  if (class(df) == "try-error") {
    stop("\nError in fetching data: ", conditionMessage(attr(df, "condition")),
         call. = FALSE
    )
  } else if
  (purrr::is_empty(df)) {
    stop(
      paste0(
        "\nIt seems that there is no data available. Possibly, the last available data is earlier than that defined in one of these arguments:
      \n1. 'first_date'", "\n2. 'reference_date'"
      ),
      call. = FALSE
    )
  } else if
  (be_quiet) {message("", appendLF = FALSE)}
  else
    message(paste0("\nFound ", nrow(df), " observations!\n"), appendLF = FALSE)

  df <- dplyr::rename_with(
    dplyr::as_tibble(df),
    ~c("indicator", "detail", "date", "reference_date", "mean",
       "median", "sd", "coef_var", "min", "max", "n_respondents", "basis")
  )
  df <- dplyr::mutate(df, date = as.Date(date, format = "%Y-%m-%d"))

  return(df)
}
