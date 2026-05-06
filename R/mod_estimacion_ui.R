mod_estimacion_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(

    # -- Encabezado ----------------------------------------------------
    shiny::tags$div(
      class = "page-header page-header--estimacion",
      shiny::tags$div(
        class = "page-header-icon",
        shiny::tags$i(class = "fa fa-bar-chart")
      ),
      shiny::tags$div(
        shiny::h3(shiny::textOutput(ns("title")), class = "page-title"),
        shiny::tags$p(
          shiny::textOutput(ns("subtitle"), inline = TRUE),
          class = "page-subtitle"
        )
      )
    ),

    shiny::fluidRow(

      # -- Columna izquierda: parametros ----------------------------
      shiny::column(
        3,
        shiny::wellPanel(
          class = "sidebar-card sidebar-card--estimacion",

          shiny::tags$div(
            class = "card-header-block card-header-block--estimacion",
            shiny::tags$i(class = "fa fa-sliders"),
            shiny::textOutput(ns("params_card"), inline = TRUE)
          ),

          shiny::tags$div(
            class = "card-body-block",

            shiny::selectInput(
              inputId = ns("estimator"),
              label   = "Tipo de estimaci\u00f3n",
              choices = c(
                "Media"        = "mean",
                "Total"        = "total",
                "Proporci\u00f3n" = "prop",
                "Raz\u00f3n"      = "ratio",
                "Cuantiles"    = "quantile"
              )
            ),

            # Razon
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'ratio'", ns("estimator")),
              shiny::selectInput(
                ns("numerator"), "Numerador", choices = NULL
              ),
              shiny::selectInput(
                ns("denominator"), "Denominador", choices = NULL
              ),
              shiny::uiOutput(ns("ratio_levels_ui"))
            ),

            # Cuantiles
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'quantile'", ns("estimator")),
              shiny::textInput(
                ns("probs"),
                label = "Cuantiles (ej: 0.25, 0.5, 0.75)",
                value = "0.25, 0.5, 0.75"
              )
            ),

            # Variable de interes
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] != 'ratio'", ns("estimator")),
              shiny::selectInput(
                ns("y_var"),
                label   = "Variable de inter\u00e9s",
                choices = NULL
              )
            ),

            # Dominios
            shiny::selectInput(
              inputId  = ns("domain_vars"),
              label    = "Dominio(s) (opcional)",
              choices  = c("Ninguno" = ""),
              multiple = TRUE
            ),

            shiny::hr(),

            shiny::actionButton(
              inputId = ns("run"),
              label   = shiny::tagList(
                shiny::tags$i(class = "fa fa-play"),
                " ",
                shiny::textOutput(ns("run_btn"), inline = TRUE)
              ),
              class = "btn-primary btn-action"
            )
          )
        )
      ),

      # -- Columna derecha: resultados ----------------------------
      shiny::column(
        9,
        shiny::wellPanel(

          # Marco teorico
          shiny::tags$div(
            class = "card-header-block card-header-block--estimacion",
            shiny::tags$i(class = "fa fa-book"),
            shiny::textOutput(ns("theory_title"), inline = TRUE)
          ),
          shiny::tags$div(
            class = "card-body-block",
            shiny::uiOutput(ns("theory_box"))
          ),

          shiny::hr(),

          # Estado
          shiny::tags$div(
            class = "card-header-block card-header-block--estimacion",
            shiny::tags$i(class = "fa fa-info-circle"),
            shiny::textOutput(ns("status_title"), inline = TRUE)
          ),
          shiny::tags$div(
            class = "card-body-block",
            shiny::verbatimTextOutput(ns("log"))
          ),

          shiny::hr(),

          # Resultado
          shiny::tags$div(
            class = "card-header-block card-header-block--estimacion",
            shiny::tags$i(class = "fa fa-table"),
            shiny::textOutput(ns("result_title"), inline = TRUE)
          ),
          shiny::tags$div(
            class = "card-body-block",
            DT::DTOutput(ns("preview"))
          ),

          shiny::hr(),

          # Indicadores de calidad
          shiny::tags$div(
            class = "card-header-block card-header-block--estimacion",
            shiny::tags$i(class = "fa fa-check-circle"),
            shiny::textOutput(ns("quality_title"), inline = TRUE)
          ),
          shiny::tags$div(
            class = "card-body-block",
            shiny::uiOutput(ns("quality_theory"))
          )
        )
      )
    )
  )
}
