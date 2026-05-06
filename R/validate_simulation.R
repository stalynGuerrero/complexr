#' Validate simulated survey data
#'
#' Performs a set of structural and consistency checks on a dataset generated
#' by `generate_example_data()`. The function verifies household-level
#' invariants (area, weights, per capita income), demographic rules,
#' and PSU size constraints.
#'
#' @param data A tibble or data.frame at individual level.
#' @param strict Logical. If TRUE, stops execution when any validation fails.
#'
#' @return A tibble with validation results (one row per check).
#' @export
#'
#' @examples
#' data <- generate_example_data(n_upm = 20, seed = 123)
#' validate_simulation(data)
validate_simulation <- function(data, strict = FALSE){
  
  # ---------------------------
  # Basic input validation
  # ---------------------------
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame or tibble")
  }
  
  required_vars <- c(
    "hogar_id","upm","area","weight",
    "ingreso_pc","edad","educacion"
  )
  
  missing_vars <- setdiff(required_vars, names(data))
  
  if (length(missing_vars) > 0) {
    stop(
      sprintf("Missing required columns: %s",
              paste(missing_vars, collapse = ", "))
    )
  }
  
  # ---------------------------
  # 1. Area constant within household
  # ---------------------------
  area_check <- data |>
    dplyr::distinct(hogar_id, area) |>
    dplyr::count(hogar_id)
  
  ok_area <- all(area_check$n == 1)
  
  # ---------------------------
  # 2. Weight constant within household
  # ---------------------------
  weight_check <- data |>
    dplyr::group_by(hogar_id) |>
    dplyr::summarise(
      n_w = dplyr::n_distinct(weight),
      .groups = "drop"
    )
  
  ok_weight <- all(weight_check$n_w == 1)
  
  # ---------------------------
  # 3. Per capita income consistency
  # ---------------------------
  income_check <- data |>
    dplyr::group_by(hogar_id) |>
    dplyr::summarise(
      n_inc = dplyr::n_distinct(ingreso_pc),
      .groups = "drop"
    )
  
  ok_income <- all(income_check$n_inc == 1)
  
  # ---------------------------
  # 4. Education rule
  # edad < 5 → educacion NA
  # ---------------------------
  edu_check <- data |>
    dplyr::filter(edad < 5)
  
  ok_edu <- all(is.na(edu_check$educacion))
  
  # ---------------------------
  # 5. PSU size constraint
  # ---------------------------
  upm_check <- data |>
    dplyr::distinct(upm, hogar_id) |>
    dplyr::count(upm)
  
  ok_upm <- all(upm_check$n <= 15)
  
  # ---------------------------
  # Consolidated output
  # ---------------------------
  results <- tibble::tibble(
    check = c(
      "area_within_household",
      "weight_within_household",
      "income_pc_within_household",
      "education_rule",
      "upm_size"
    ),
    passed = c(
      ok_area,
      ok_weight,
      ok_income,
      ok_edu,
      ok_upm
    )
  )
  
  # ---------------------------
  # Fail-fast option
  # ---------------------------
  if (strict && any(!results$passed)) {
    failed <- results$check[!results$passed]
    stop(
      sprintf(
        "Validation failed for: %s",
        paste(failed, collapse = ", ")
      )
    )
  }
  
  return(results)
}