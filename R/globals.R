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
  "educ_hogar",
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
  "ratio_den_level",

  # inferencia
  "gini",
  "correlation",
  "t_stat",
  "df",
  "p_value",
  "mean_diff",
  "mean_1",
  "mean_2",
  "group_1",
  "group_2",
  "group_var",
  "x_var",
  "y_var",
  "row_var",
  "col_var",
  "domain_var",
  "domain_val",
  "statistic",
  "value",
  "df_num",
  "df_den",
  ".section",

  # contrastes
  "group_i",
  "group_j",
  "mean_i",
  "mean_j",
  "diff",
  "sig",
  "contraste",
  "coeficientes",
  "estimado",
  "label",

  # hiptest por grupos
  "se_1",
  "se_2",

  # gráficos
  "p",
  "L",
  "cat",
  "prop",
  "comparison",
  "sig_cat"
))

# Explicit imports required by R CMD check

#' @importFrom stats coef vcov update weights rlnorm runif rbinom rgamma rnorm pt qnorm
#' @importFrom utils head
#' @importFrom magrittr %>%
#' @importFrom dplyr n_distinct
#' @importFrom rlang :=
#' @importFrom shiny req observeEvent updateSelectInput
#' @importFrom tidyselect where
NULL