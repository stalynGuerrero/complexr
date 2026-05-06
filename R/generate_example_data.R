#' Generate simulated complex survey data with hierarchical structure
#'
#' Generates a synthetic dataset representing a complex survey design with a
#' three-level hierarchical structure: primary sampling units (PSUs), households,
#' and individuals. The function ensures internal consistency of weights,
#' cluster sizes, and derived variables, making it suitable for testing survey
#' estimators, calibration methods, and small area estimation workflows.
#'
#' @param n_upm Integer. Number of primary sampling units (PSUs).
#'
#' @param seed Integer. Random seed for reproducibility.
#'
#' @details
#'
#' \strong{Sampling structure}
#'
#' The simulated data follows a hierarchical design:
#'
#' \enumerate{
#'   \item \strong{PSU (UPM)}: each PSU belongs to a stratum and contains a
#'         random number of households (between 5 and 15).
#'   \item \strong{Households}: each household has a sampling weight and a
#'         fixed area (urban/rural).
#'   \item \strong{Individuals}: observations are generated at the individual level.
#' }
#'
#' \strong{Weights}
#'
#' Sampling weights are defined at the household level and are constant for all
#' individuals within the same household:
#'
#' \deqn{
#' w_{hi} = w_h
#' }
#'
#'
#' \strong{Income generation}
#'
#' Household income is generated with hierarchical random effects:
#'
#' \deqn{
#' Y_h \sim \text{Gamma}(\alpha, \beta \cdot \exp(u_j + v_h))
#' }
#'
#' where:
#' \itemize{
#'   \item \eqn{u_j} is the PSU-level effect
#'   \item \eqn{v_h} is the household-level effect
#' }
#'
#' Per capita income is then defined as:
#'
#' \deqn{
#' y_{hi} = \frac{Y_h}{N_h}
#' }
#'
#' where \eqn{N_h} is the household size.
#'
#'
#' \strong{Education and income correlation}
#'
#' Education is assigned at the individual level conditional on age. A household-level
#' education effect is derived from the maximum education level within the household
#' and used to adjust household income:
#'
#' \deqn{
#' Y_h^{*} = Y_h \cdot f(E_h)
#' }
#'
#' where \eqn{f(E_h)} is a multiplicative factor depending on household education.
#'
#'
#' \strong{Consistency constraints}
#'
#' The simulation enforces:
#'
#' \itemize{
#'   \item All individuals in a household share the same \code{area}
#'   \item Individuals younger than 5 years have no education (\code{NA})
#'   \item Per capita income is constant within households
#'   \item Each PSU contains at most 15 households
#' }
#'
#'
#' \strong{Variables}
#'
#' The dataset includes:
#'
#' \itemize{
#'   \item Design variables: \code{strata}, \code{upm}, \code{hogar_id}, \code{persona_id}, \code{weight}
#'   \item Domains: \code{region}, \code{sexo}, \code{area}
#'   \item Demographics: \code{edad}, \code{educacion}, \code{empleo}
#'   \item Welfare indicators: \code{ingreso_pc}, \code{gasto_pc}, \code{pobre}
#'   \item Auxiliary variable with missingness: \code{ingreso2}
#' }
#'
#' @return
#' A tibble at the individual level with hierarchical identifiers and survey variables.
#'
#' @examples
#' data <- generate_example_data(n_upm = 50)
#' validate_simulation(data)
#' # ---------------------------------------------------------
#' # Example: build survey design
#' # ---------------------------------------------------------
#' # design <- as_survey_design_tbl(
#' #   data,
#' #   weight = "weight",
#' #   strata = "strata",
#' #   cluster = "upm"
#' # )
#' 
#' @importFrom dplyr mutate group_by ungroup case_when select n
#' @importFrom tidyr uncount
#' @importFrom magrittr %>%
#' @importFrom stats plogis rnorm runif rbinom rgamma
#' @export
generate_example_data <- function(n_upm = 200, seed = 123){
  
  set.seed(seed)
  
  # =========================================================
  # 1. PSU (Primary Sampling Units)
  # =========================================================
  upm_df <- tibble::tibble(
    upm = paste0("UPM", seq_len(n_upm)),
    strata = sample(paste0("S",1:5), n_upm, TRUE),
    upm_effect = rnorm(n_upm, 0, 0.3),
    n_hogares = sample(5:15, n_upm, TRUE)
  )
  
  # =========================================================
  # 2. Households
  # =========================================================
  hogares <- upm_df %>%
    tidyr::uncount(n_hogares, .id = "hogar_id_upm") %>%
    mutate(
      hogar_id = paste0(upm, "_H", hogar_id_upm),
      n_personas = sample(1:6, n(), TRUE),
      weight_hogar = runif(n(), 200, 1500),
      hogar_effect = rnorm(n(), 0, 0.2),
      
      # consistent area within household
      area = sample(c("Urban","Rural"), n(), TRUE),
      
      # baseline household income (latent)
      ingreso_hogar = rgamma(
        n(),
        shape = 5,
        scale = 800 * exp(upm_effect + hogar_effect)
      )
    )
  
  # =========================================================
  # 3. Individuals
  # =========================================================
  data <- hogares %>%
    tidyr::uncount(n_personas, .id = "persona_id_hogar") %>%
    mutate(
      persona_id = paste0(hogar_id, "_P", persona_id_hogar),
      
      # domains
      region = sample(c("North","Center","South"), n(), TRUE),
      sexo   = sample(c("Male","Female"), n(), TRUE),
      
      # age
      edad = pmax(0, round(rnorm(n(), 35, 18)))
    )
  
  # =========================================================
  # 4. Education (age-dependent)
  # =========================================================
  data <- data %>%
    mutate(
      educacion = case_when(
        edad < 5  ~ NA_character_,
        edad < 12 ~ "Primary",
        edad < 18 ~ "Secondary",
        TRUE ~ sample(
          c("Secondary","Higher"),
          n(),
          TRUE,
          prob = c(0.6, 0.4)
        )
      ),
      educacion = factor(
        educacion,
        levels = c("Primary","Secondary","Higher")
      )
    )
  
  # =========================================================
  # 5. Education → income correlation (household level)
  # =========================================================
  data <- data %>%
    mutate(
      educ_num = case_when(
        educacion == "Primary" ~ 1,
        educacion == "Secondary" ~ 2,
        educacion == "Higher" ~ 3,
        TRUE ~ NA_real_
      )
    ) %>%
    group_by(hogar_id) %>%
    mutate(
      educ_hogar = ifelse(
        all(is.na(educ_num)),
        NA_real_,
        max(educ_num, na.rm = TRUE)
      )
    ) %>%
    ungroup() %>%
    mutate(
      educ_effect = case_when(
        is.na(educ_hogar) ~ 1,
        educ_hogar == 3 ~ 1.3,
        educ_hogar == 2 ~ 1.1,
        educ_hogar == 1 ~ 0.9
      ),
      ingreso_hogar = ingreso_hogar * educ_effect
    )
  
  # =========================================================
  # 6. Additional individual variables
  # =========================================================
  data <- data %>%
    mutate(
      empleo = factor(sample(
        c("Formal","Informal","Unemployed"),
        n(),
        TRUE
      )),
      
      pobre = rbinom(
        n(),
        1,
        plogis(-1 + upm_effect + hogar_effect)
      )
    )
  
  # =========================================================
  # 7. Per capita income
  # =========================================================
  data <- data %>%
    group_by(hogar_id) %>%
    mutate(
      ingreso_pc = ingreso_hogar / n(),
      gasto_pc = ingreso_pc * runif(1, 0.6, 0.9)
    ) %>%
    ungroup()
  
  # =========================================================
  # 8. Missing values
  # =========================================================
  data <- data %>%
    mutate(
      ingreso2 = ifelse(
        runif(n()) < 0.1,
        NA,
        ingreso_pc * runif(n(), 0.8, 1.2)
      )
    )
  
  # =========================================================
  # 9. Final weight (household level)
  # =========================================================
  data <- data %>%
    mutate(
      weight = weight_hogar
    )
  
  # =========================================================
  # 10. Final selection
  # =========================================================
  data %>%
    select(
      strata,
      upm,
      hogar_id,
      persona_id,
      weight,
      region,
      sexo,
      area,
      edad,
      educacion,
      empleo,
      ingreso_pc,
      gasto_pc,
      pobre,
      ingreso2
    )
}