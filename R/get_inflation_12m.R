#' Get data on market expectations for inflation over the next 12 months (Focus/BCB)
#'
#' @param indicator A single string or a character vector with inflation indicators names, see details for possible values. Defaults to NULL.
#' @param first_date A single string or Date in "YYYY-mm-dd" format. Default for 2 years prior to the current date.
#' @param last_date A single string or Date in "YYYY-mm-dd" format. Default is the current date.
#' @param smoothed A single string indicating if the statistics should be smoothed ("yes" or "no"). Defaults to NULL.
#' @param be_quiet Logical. Should messages or warnings not be displayed? Defaults to FALSE.
#' @param use_memoise Logical. Sets the use of caching system, creating a folder named "cache_bcb" by default. Default to TRUE.
#' @param do_parallel Logical. For using parallel data importation. Defaults to FALSE.
#'
#' @return A tibble with data from BCB-Olinda Data Services.
#' @description This function provides the extraction of data and statistics related to the expectations of economic indicators, specifically the market expectations for inflation in the next 12 months, made available by the Central Bank of Brazil's Expectations System through the Olinda API. The data comes from several financial institutions: banks, funds, research houses, etc. Important: arguments are case sensitive.
#' @details For periods for which there are no statistics, they will be omitted from the query.
#'
#' Possible values for indicator argument: "IGP-DI", "IGP-M", "INPC", "IPA-DI", "IPA-M", "IPCA", "IPCA-15", "IPC-FIPE".
#' @author Fernando da Silva <<fernando@fortietwo.com>>
#' @encoding UTF-8
#' @export
#'
#' @examples
#' df <- get_inflation_12m(
#'   indicator = "IPCA",
#'   first_date = Sys.Date() - 30,
#'   smoothed = "yes",
#'   use_memoise = FALSE
#' )
get_inflation_12m <- function (
  indicator   = NULL,
  first_date  = Sys.Date() - 2*365,
  last_date   = Sys.Date(),
  smoothed    = NULL,
  be_quiet    = FALSE,
  use_memoise = TRUE,
  do_parallel = FALSE
){
  valid_indicator <- c("IGP-DI", "IGP-M", "INPC", "IPA-DI", "IPA-M", "IPCA", "IPCA-15", "IPC-FIPE")

  if (missing(indicator) | !all(indicator %in% valid_indicator) | is.null(indicator)) {
    stop("\nArgument 'indicator' is not valid or missing. Check your inputs.", call. = FALSE)
  } else indicator

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

  if (!is.null(smoothed) && !is.na(smoothed)) {
    if ((class(smoothed) != "character")) {
      stop("\nArgument 'smoothed' is not valid. Check your inputs.", call. = FALSE)
    } else if
    (!all(smoothed %in% c("yes", "no"))){
      stop("\nArgument 'smoothed' is not valid. Check your inputs.", call. = FALSE)
    } else if
    (length(smoothed) > 1) {
      stop("\nArgument 'smoothed' is not valid. Check your inputs.", call. = FALSE)
    } else
      if (smoothed == "yes") {smoothed <- "S"} else
        if (smoothed == "no") {smoothed <- "N"}
  } else if
  (is.na(smoothed) && (length(smoothed) > 0)) {smoothed <- NULL} else
    smoothed <- as.character(smoothed)

  if ((class(do_parallel) != "logical") || (is.na(do_parallel))) {
    stop("\nArgument 'do_parallel' must be logical. Check your inputs.", call. = FALSE)
  } else if

  ((class(be_quiet) != "logical") || (is.na(be_quiet))) {
    stop("\nArgument 'be_quiet' must be logical. Check your inputs.", call. = FALSE)
  }

  foo_args <- paste0(
    paste0("(", paste(sprintf("Indicador eq '%s'", indicator), collapse = " or ", sep = ""), ")"),
    sprintf(" and Data ge '%s'", first_date),
    sprintf(" and Data le '%s'", last_date),
    sprintf(" and Suavizada eq '%s'", smoothed)
  )

  odata_url <- list(
    httr::modify_url(
      "https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativasMercadoInflacao12Meses",
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

  if (class(df) == "try-error") {
    stop("\nError in fetching data: ", conditionMessage(attr(df, "condition")),
         call. = FALSE
    )
  } else if
  (purrr::is_empty(df)) {
    stop(
      paste0(
        "\nIt seems that there is no data available. Possibly, the last available data is earlier than that defined in one of these arguments:
      \n1. 'first_date'"
      ),
      call. = FALSE
    )
  } else if
  (be_quiet) {message("", appendLF = FALSE)}
  else
    message(paste0("\nFound ", nrow(df), " observations!\n"), appendLF = FALSE)

  df <- dplyr::rename_with(
    dplyr::as_tibble(df),
    ~c("indicator", "date", "smoothed", "mean", "median",
       "sd","coef_var", "min", "max", "n_respondents", "basis")
  )
  df <- dplyr::mutate(
    df,
    date = as.Date(date, format = "%Y-%m-%d"),
    smoothed = dplyr::recode(smoothed, "N" = "no", "S" = "yes")
  )

  return(df)
}
