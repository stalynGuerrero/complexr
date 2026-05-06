# Global variable bindings for NSE (Non-Standard Evaluation)
# Required to avoid R CMD check notes related to dplyr/srvyr/ggplot2 usage

utils::globalVariables(c(
  
  # pronouns
  ".data",
  ".ind",
  
  # diseño
  "estrato",
  "upm",
  "strata",
  "w",
  
  # IDs jerárquicos
  "hogar_id",
  "hogar_id_upm",
  "persona_id",
  "persona_id_hogar",
  
  # tamaños
  "n_hogares",
  "n_personas",
  
  # pesos
  "weight_hogar",
  "weight",
  
  # efectos
  "upm_effect",
  "hogar_effect",
  "estimate_deff",
  
  # dominios
  "sexo",
  "region",
  "etnia",
  "dam",
  "area",
  
  # demografía
  "edad",
  "educacion",
  "empleo",
  
  # educación derivada
  "educ_num",
  "educ_effect",
  
  # ingresos
  "ingreso",
  "ingreso_hogar",
  "ingreso_pc",
  "gasto_pc",
  "ingreso2",
  
  # pobreza
  "pobre",
  "miembros",
  
  # outputs
  "estimate",
  "se",
  "cv",
  "lci",
  "uci",
  "estimate_se",
  "estimate_cv",
  "estimator",
  "variable",
  "ic",
  
  # ratio
  "ratio_num_level",
  "ratio_den_level"
))

# Explicit imports required by R CMD check

#' @importFrom stats coef vcov update weights rlnorm runif rbinom rgamma rnorm
#' @importFrom utils head
#' @importFrom magrittr %>%
#' @importFrom dplyr n_distinct
#' @importFrom rlang :=
#' @importFrom shiny req observeEvent updateSelectInput
#' @importFrom tidyselect where
NULL