mod_diseno_ui <- function(id) {

  ns <- shiny::NS(id)

  shiny::fluidPage(

    # Encabezado del modulo
    shiny::div(
      style = "margin-bottom:16px;",
      shiny::h3(shiny::textOutput(ns("title"), inline = TRUE)),
      shiny::tags$p(
        shiny::textOutput(ns("subtitle"), inline = TRUE),
        style = "color:#4A6572; font-size:13px; margin-top:4px;"
      )
    ),
    shiny::tags$style(shiny::HTML("
/* Altura del dropdown */
.selectize-dropdown {
  max-height: 300px !important;
  overflow-y: auto;
}

/* Espaciado y legibilidad de opciones */
.selectize-dropdown-content .option {
  padding: 10px 12px;
  line-height: 1.4;
  white-space: normal;   /* permite salto de linea */
}

/* Opcional: ancho completo del dropdown */
.selectize-control {
  width: 100%;
}
")),

    shiny::tabsetPanel(
      id   = ns("diseno_tabs"),
      type = "tabs",

      # ============================================================
      # Sub-pestana 1: Configurar diseno
      # ============================================================
      shiny::tabPanel(
        title = shiny::tagList(
          shiny::tags$i(class = "fa fa-cogs"),
          " ",
          shiny::textOutput(ns("lbl_tab_config"), inline = TRUE)
        ),
        value = "config",

        shiny::br(),

        shiny::fluidRow(

          shiny::column(
            3,
            shiny::wellPanel(
              class = "sidebar-card sidebar-card--diseno",

              shiny::selectInput(
                ns("design_type"),
                "Tipo de dise\u00f1o",
                choices = c(
                  "Simple (SRS)"               = "srs",
                  "Estratificado"              = "stratified",
                  "Conglomerados / Multiet\u00e1pico" = "cluster"
                )
              ),

              shiny::uiOutput(ns("design_theory")),

              shiny::hr(),

              shiny::uiOutput(ns("design_arguments")),
              
              shiny::br(),
              shiny::br(),
              shiny::br(),
              shiny::br(),
              
              shiny::actionButton(
                ns("build"),
                shiny::textOutput(ns("build_btn"), inline = TRUE),
                class = "btn-primary"
              )
            )
          ),

          shiny::column(
            9,
            shiny::wellPanel(

              shiny::h4(shiny::textOutput(ns("design_summary"), inline = TRUE)),
              shiny::verbatimTextOutput(ns("log")),

              shiny::h4(shiny::textOutput(ns("r_code"), inline = TRUE)),
              shiny::verbatimTextOutput(ns("design_code")),

              shiny::hr(),

              shiny::h4(shiny::textOutput(ns("diagnostic"), inline = TRUE)),
              shiny::tableOutput(ns("summary"))
            )
          )
        )
      ),

      # ============================================================
      # Sub-pestana 2: Crear variables
      # ============================================================
      shiny::tabPanel(
        title = shiny::tagList(
          shiny::tags$i(class = "fa fa-magic"),
          " ",
          shiny::textOutput(ns("lbl_tab_variables"), inline = TRUE)
        ),
        value = "variables",

        shiny::br(),

        mod_variables_ui(ns("variables"))
      )
    )
  )
}
