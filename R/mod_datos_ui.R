mod_datos_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(

    shiny::tags$div(
      class = "page-header page-header--datos",
      shiny::tags$div(
        class = "page-header-icon",
        shiny::tags$i(class = "fa fa-database")
      ),
      shiny::tags$div(
        shiny::h3(shiny::textOutput(ns("title")), class = "page-title"),
        shiny::tags$p(class = "page-subtitle",
          shiny::textOutput(ns("input_panel"), inline = TRUE)
        )
      )
    ),

    shiny::fluidRow(

      shiny::column(
        4,
        shiny::wellPanel(
          class = "sidebar-card sidebar-card--datos",

          shiny::tags$div(
            class = "card-header-block card-header-block--datos",
            shiny::tags$i(class = "fa fa-upload"),
            shiny::textOutput(ns("source_card"), inline = TRUE)
          ),

          shiny::tags$div(
            class = "card-body-block",
            shiny::uiOutput(ns("file_ui")),
            shiny::hr(),
            shiny::actionButton(
              ns("load_example"),
              label = NULL,
              class = "btn-primary btn-action"
            )
          )
        )
      ),

      shiny::column(
        8,
        shiny::wellPanel(

          shiny::tags$div(
            class = "card-header-block card-header-block--datos",
            shiny::tags$i(class = "fa fa-table"),
            shiny::textOutput(ns("preview_card"), inline = TRUE)
          ),

          shiny::tags$div(
            class = "card-body-block",
            DT::DTOutput(ns("preview")),
            shiny::hr(),
            shiny::h4(
              shiny::tags$i(class = "fa fa-info-circle"),
              shiny::textOutput(ns("status"), inline = TRUE)
            ),
            shiny::verbatimTextOutput(ns("log"))
          )
        )
      )
    )
  )
}
