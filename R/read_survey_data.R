#' Read survey microdata from multiple file formats
#'
#' Reads microdata commonly used in official statistics and survey analysis
#' from a local file path. The function returns a tibble and attaches source
#' metadata as attributes, facilitating traceability and reproducibility.
#'
#' @param path Character. File path to the dataset.
#' @param col_types Optional. Passed to \code{readr::read_csv()} /
#'   \code{read_tsv()}.
#' @param guess_max Integer. Maximum rows used for type guessing (CSV/TSV only).
#' @param encoding Character. File encoding (CSV/TSV only).
#'   Default \code{"UTF-8"}.
#' @param sheet Integer or character. Sheet to read for Excel files.
#'   Default \code{1}.
#'
#' @return A tibble with attributes:
#' \itemize{
#'   \item \code{source_path}: normalized file path
#'   \item \code{source_format}: file extension
#'   \item \code{n_rows}: number of rows
#'   \item \code{n_cols}: number of columns
#' }
#'
#' @details
#' Supported formats:
#' \itemize{
#'   \item \code{.csv} — comma-separated values (\code{readr})
#'   \item \code{.tsv}, \code{.txt} — tab-separated values (\code{readr})
#'   \item \code{.xlsx} — Excel 2007+ (\code{readxl})
#'   \item \code{.xls} — Excel 97-2003 (\code{readxl})
#'   \item \code{.sav} — SPSS (\code{haven})
#'   \item \code{.por} — SPSS portable (\code{haven})
#'   \item \code{.dta} — Stata (\code{haven})
#'   \item \code{.sas7bdat} — SAS data (\code{haven})
#'   \item \code{.xpt} — SAS transport (\code{haven})
#'   \item \code{.rds} — R serialized object (base R)
#' }
#'
#' @examples
#' \dontrun{
#' df <- read_survey_data("data/sample.csv")
#' df <- read_survey_data("data/sample.sav")
#' df <- read_survey_data("data/sample.dta")
#' df <- read_survey_data("data/sample.sas7bdat")
#' attr(df, "source_format")
#' }
#'
#' @export
read_survey_data <- function(
    path,
    col_types = NULL,
    guess_max = 10000,
    encoding  = "UTF-8",
    sheet     = 1
) {

  if (!is.character(path) || length(path) != 1) {
    rlang::abort("`path` must be a single character string.")
  }

  if (!file.exists(path)) {
    rlang::abort(
      message = paste("File does not exist:", path),
      class   = "shinycomplexsurvey_file_not_found"
    )
  }

  ext <- tolower(tools::file_ext(path))

  data <- switch(
    ext,

    "csv" = readr::read_csv(
      path,
      col_types     = col_types,
      guess_max     = guess_max,
      locale        = readr::locale(encoding = encoding),
      show_col_types = FALSE,
      progress      = FALSE
    ),

    "tsv" = ,
    "txt" = readr::read_tsv(
      path,
      col_types     = col_types,
      guess_max     = guess_max,
      locale        = readr::locale(encoding = encoding),
      show_col_types = FALSE,
      progress      = FALSE
    ),

    "xlsx" = readxl::read_xlsx(path, sheet = sheet),

    "xls"  = readxl::read_xls(path, sheet = sheet),

    "sav"  = haven::read_sav(path),

    "por"  = haven::read_por(path),

    "dta"  = haven::read_dta(path),

    "sas7bdat" = haven::read_sas(path),

    "xpt"  = haven::read_xpt(path),

    "rds"  = readRDS(path),

    rlang::abort(
      message = paste0(
        "Unsupported file format: .", ext,
        ". Supported: csv, tsv, txt, xlsx, xls,",
        " sav, por, dta, sas7bdat, xpt, rds."
      ),
      class = "shinycomplexsurvey_unsupported_format"
    )
  )

  if (!tibble::is_tibble(data)) {
    data <- tibble::as_tibble(data)
  }

  attr(data, "source_path")   <- normalizePath(
    path, winslash = "/", mustWork = FALSE
  )
  attr(data, "source_format") <- ext
  attr(data, "n_rows")        <- nrow(data)
  attr(data, "n_cols")        <- ncol(data)

  data
}