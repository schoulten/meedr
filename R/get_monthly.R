#' Get data on monthly market expectations for economic indicators (Focus/BCB)
#'
#' @param indicator A single string or a character vector with economic indicators names, see details for possible value. Defaults to NULL.
#' @param first_date A single string or Date in "YYYY-mm-dd" format. Default for 2 years prior to the current date.
#' @param last_date A single string or Date in "YYYY-mm-dd" format. Default is the current date.
#' @param reference_date A single string in "mm/YYYY" format, indicating the reference month and year for which the statistic is expected. Defaults to NULL.
#' @param be_quiet Logical. Should messages or warnings not be displayed? Defaults to FALSE.
#' @param use_memoise Logical. Sets the use of caching system, creating a folder named "cache_bcb" by default. Default to TRUE.
#' @param do_parallel Logical. For using parallel data importation. Defaults to FALSE.
#'
#' @return A tibble with data from BCB-Olinda Data Services.
#' @description This function provides the extraction of data and statistics related to expectations of economic indicators, specifically monthly market expectations, made available by the Central Bank of Brazil's Expectations System through the Olinda API. The data comes from several financial institutions: banks, funds, research houses, etc. Important: arguments are case sensitive.
#' @details For periods for which there are no statistics, they will be omitted from the query.
#'
#' Possible values for indicator argument: 'IGP-DI', 'IGP-M', 'INPC', 'IPA-DI', 'IPA-M', 'IPCA', 'IPCA-15', 'IPC-Fipe', 'IPCA Administrados', 'IPCA Alimentação no domicílio', 'IPCA Bens industrializados', 'IPCA Livres', 'IPCA Serviços', 'Selic' (discontinued by source, data may be partial), 'Câmbio', 'Taxa de desocupação', 'Produção industrial'.
#'
#' @author Fernando da Silva <<fernando@fortietwo.com>>
#' @encoding UTF-8
#' @export
#'
#' @examples
#' df <- get_monthly(
#'   indicator = "IPCA",
#'   first_date = Sys.Date() - 30,
#'   reference_date = format(Sys.Date(), "%m/%Y"),
#'   use_memoise = FALSE
#' )
get_monthly <- function(
  indicator      = NULL,
  first_date     = Sys.Date() - 2*365,
  last_date      = Sys.Date(),
  reference_date = NULL,
  be_quiet       = FALSE,
  use_memoise    = TRUE,
  do_parallel    = FALSE
){
  valid_indicator <- c(
    # Índices de preços
    "IGP-DI",
    "IGP-M",
    "INPC",
    "IPA-DI",
    "IPA-M",
    "IPCA",
    "IPCA-15",
    "IPC-Fipe",
    "IPCA Administrados",
    "IPCA Alimenta\u00e7\u00e3o no domic\u00edlio",
    "IPCA Bens industrializados",
    "IPCA Livres",
    "IPCA Servi\u00e7os",
    # Taxas
    "Selic",
    "C\u00e2mbio",
    # Atividade
    "Taxa de desocupa\u00e7\u00e3o",
    "Produ\u00e7\u00e3o industrial"
    )

  if (missing(indicator) | !all(indicator %in% valid_indicator) | is.null(indicator)) {
    stop("\nArgument 'indicator' is not valid or missing. Check your inputs.", call. = FALSE)
  }
  if ("Selic" %in% indicator) {
    warning(
      "The Selic indicator has been discontinued by the source, data returned may be partial.\nSuggestion: use the get_selic() function to access the new API data.",
      call. = FALSE,
      immediate. = TRUE
    )
  }

  first_date <- try(as.Date(first_date), silent = TRUE)
  if (length(first_date) <= 0 || is.na(first_date)) {first_date = NULL}
  if (inherits(first_date, "try-error")) {
    stop("\nArgument 'first_date' is not a valid date.", call. = FALSE)
  }
  if (missing(first_date)) {first_date = Sys.Date() - 10 * 365}

  last_date <- try(as.Date(last_date), silent = TRUE)
  if (length(last_date) <= 0 || is.na(last_date)) {last_date = NULL}
  if (inherits(last_date, "try-error")) {
    stop("\nArgument 'last_date' is not a valid date.", call. = FALSE)
  }
  if (missing(last_date)) {last_date = Sys.Date() - 10 * 365}

  if ((length(first_date) > 0) && first_date > Sys.Date()) {
    stop("\nIt seems that 'first_date' > current date. Check your inputs.", call. = FALSE)
  }

  if (!is.null(last_date)) {
    if ((length(first_date) > 0) && last_date < first_date) {
      stop("\nIt seems that 'last_date' < first_date. Check your inputs.", call. = FALSE)
    }
  }

  if (!is.null(reference_date) && !is.na(reference_date)) {
    if (!inherits(reference_date, "character")) {
      stop("\nArgument 'reference_date' is not valid. Check your inputs.", call. = FALSE)
    } else if
    (nchar(reference_date) == 7L & (grepl("(\\d{2})(\\/{1})(\\d{4}$)", reference_date))) {
      reference_date <- as.character(reference_date)
    } else
      stop("\nArgument 'reference_date' is not valid. Check yout inputs.", call. = FALSE)
  } else if
  (is.na(reference_date) && (length(reference_date) > 0)) {reference_date <- NULL}

  if (!inherits(do_parallel, "logical") || (is.na(do_parallel))) {
    stop("\nArgument 'do_parallel' must be logical. Check your inputs.", call. = FALSE)
  } else if

  (!inherits(be_quiet, "logical") || (is.na(be_quiet))) {
    stop("\nArgument 'be_quiet' must be logical. Check your inputs.", call. = FALSE)
  }

  foo_args <- paste0(
    paste0("(", paste(sprintf("Indicador eq '%s'", indicator), collapse = " or ", sep = ""), ")"),
    sprintf(" and Data ge '%s'", first_date),
    sprintf(" and Data le '%s'", last_date),
    sprintf(" and DataReferencia eq '%s'", reference_date)
  )

  odata_url <- list(
    httr::modify_url(
      "https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativaMercadoMensais",
      query = list(
        `$filter`  = foo_args,
        `$format`  = "json",
        `$orderby` = "Data desc"
      )
    )
  )

  if (!inherits(use_memoise, "logical") || (is.na(use_memoise))) {
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

  resp <- httr::GET(odata_url[[1]])
  if (httr::http_type(resp) != "application/json") {
    stop("BCB-Olinda API did not return json.", call. = FALSE)
  }

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
    used_workers <- future::nbrOfWorkers()
    available_cores <- future::availableCores()
    flag <- inherits(future::plan(), "sequential")
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

  if (inherits(df, "try-error")) {
    stop("\nProblem in fetching data, error message: ", conditionMessage(attr(df, "condition")),
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

  new_names <- c(
    "indicator"      = "Indicador",
    "detail"         = "IndicadorDetalhe",
    "date"           = "Data",
    "reference_date" = "DataReferencia",
    "mean"           = "Media",
    "median"         = "Mediana",
    "sd"             = "DesvioPadrao",
    "coef_var"       = "CoeficienteVariacao",
    "min"            = "Minimo",
    "max"            = "Maximo",
    "n_respondents"  = "numeroRespondentes",
    "basis"          = "baseCalculo"
  )

  df <- dplyr::rename(
    dplyr::as_tibble(df),
    dplyr::any_of(new_names)
  )

  df <- dplyr::mutate(df, date = as.Date(date, format = "%Y-%m-%d"))

  return(df)
}
