#' Mutate survey data using declarative definitions
#'
#' Adds or transforms variables in survey microdata using a named list
#' of formulas. Each definition must be a one-sided formula of the form
#' \code{~ expression}, evaluated within the data context.
#'
#' @param data A tibble with survey microdata.
#' @param definitions Named list of formulas defining new variables.
#' @param overwrite Logical. If FALSE, prevents overwriting existing variables.
#'
#' @return A tibble with new variables added or modified.
#' @export
#'
#' @examples
#' data <- tibble::tibble(
#'   ingreso = c(100, 200),
#'   miembros = c(2, 4)
#' )
#'
#' mutate_survey_data(
#'   data,
#'   list(
#'     ingreso_pc = ~ ingreso / miembros
#'   )
#' )
mutate_survey_data <- function(data, definitions, overwrite = TRUE) {
  
  # ---------------------------------------------------------
  # 1. Input validation
  # ---------------------------------------------------------
  if (!tibble::is_tibble(data)) {
    rlang::abort("`data` must be a tibble.")
  }
  
  if (!is.list(definitions) || is.null(names(definitions))) {
    rlang::abort("`definitions` must be a named list.")
  }
  
  if (any(names(definitions) == "")) {
    rlang::abort("All variable definitions must be named.")
  }
  
  # ---------------------------------------------------------
  # 2. Overwrite protection
  # ---------------------------------------------------------
  existing_vars <- names(data)
  new_vars <- names(definitions)
  
  if (!overwrite) {
    conflict <- intersect(existing_vars, new_vars)
    if (length(conflict) > 0) {
      rlang::abort(
        paste("Variables already exist:", paste(conflict, collapse = ", "))
      )
    }
  }
  
  # ---------------------------------------------------------
  # 3. Build quosures
  # ---------------------------------------------------------
  quos <- purrr::imap(
    definitions,
    function(def, name) {
      
      if (!inherits(def, "formula")) {
        rlang::abort(
          paste("Definition for", name, "must be a formula (~ expr).")
        )
      }
      
      expr <- rlang::f_rhs(def)
      
      # Validación de variables referenciadas
      vars_used <- all.vars(expr)
      missing_vars <- setdiff(vars_used, names(data))
      
      if (length(missing_vars) > 0) {
        rlang::abort(
          paste(
            "Variables not found in data for", name, ":",
            paste(missing_vars, collapse = ", ")
          )
        )
      }
      
      rlang::new_quosure(expr, env = rlang::caller_env())
    }
  )
  
  # ---------------------------------------------------------
  # 4. Mutate
  # ---------------------------------------------------------
  dplyr::mutate(data, !!!quos)
}