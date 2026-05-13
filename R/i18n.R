#' Load a language dictionary from JSON
#'
#' Reads a JSON translation file from the package's \code{inst/} directory and
#' returns it as a nested list suitable for use with \code{\link{i18n_t}}.
#'
#' @param lang Character. Language code, e.g. \code{"en"} (English) or
#'   \code{"es"} (Spanish).
#' @param path Character. Path inside \code{inst/} to the directory that
#'   contains the JSON translation files.
#'   Defaults to \code{"app/www/i18n"}.
#'
#' @return A named list with the translation dictionary.
#'
#' @seealso \code{\link{i18n_t}}
#'
#' @examples
#' dict <- i18n_load("en")
#' i18n_t(dict, "mod_datos.title")
#'
#' @export
i18n_load <- function(lang, path = "app/www/i18n") {
  
  file <- system.file(path, paste0(lang, ".json"), package = "complexr")
  
  if (file == "") {
    stop("Language file not found: ", lang)
  }
  
  jsonlite::fromJSON(file, simplifyVector = FALSE)
}

#' Translate a key from a language dictionary
#'
#' Resolves a dot-separated key path in a nested translation list returned by
#' \code{\link{i18n_load}}. Returns the key itself when the path is not found,
#' so the interface degrades gracefully on missing translations.
#'
#' @param dict List. A translation dictionary returned by \code{i18n_load()}.
#' @param key Character. A dot-separated path to the translation string, e.g.
#'   \code{"mod_datos.title"}.
#'
#' @return A character string with the translated text, or \code{key} when the
#'   path does not exist.
#'
#' @seealso \code{\link{i18n_load}}
#'
#' @examples
#' dict <- i18n_load("en")
#' i18n_t(dict, "mod_datos.title")
#' i18n_t(dict, "nonexistent.key")  # returns "nonexistent.key"
#'
#' @export
i18n_t <- function(dict, key) {
  
  keys <- strsplit(key, "\\.")[[1]]
  val <- dict
  
  for (k in keys) {
    val <- val[[k]]
    if (is.null(val)) return(key)
  }
  
  val
}