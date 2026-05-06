#' Load language dictionary from JSON
#' @param lang character ("es", "en", ...)
#' @param path relative path inside inst/
#' @return list
#' @export
#' 
i18n_load <- function(lang, path = "app/www/i18n") {
  
  file <- system.file(path, paste0(lang, ".json"), package = "complexr")
  
  if (file == "") {
    stop("Language file not found: ", lang)
  }
  
  jsonlite::fromJSON(file, simplifyVector = FALSE)
}

#' Translate key from dictionary
#' @param dict list
#' @param key character (e.g. "mod_datos.title")
#' @return character
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