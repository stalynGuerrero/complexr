mod_tablas_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(

    # ---- Encabezado ----
    shiny::div(
      style = paste0(
        "display:flex; align-items:flex-start;",
        " justify-content:space-between;",
        " flex-wrap:wrap; gap:12px; margin-bottom:20px;"
      ),

      shiny::tags$div(
        class = "page-header page-header--tablas",
        style = "border-bottom:none; margin-bottom:0; padding-bottom:0;",
        shiny::tags$div(
          class = "page-header-icon",
          shiny::tags$i(class = "fa fa-folder-open")
        ),
        shiny::tags$div(
          shiny::h3(
            shiny::textOutput(ns("title"), inline = TRUE),
            class = "page-title"
          ),
          shiny::tags$p(
            shiny::textOutput(ns("subtitle"), inline = TRUE),
            class = "page-subtitle"
          )
        )
      ),

      shiny::div(
        style = "display:flex; gap:8px; align-items:center; flex-wrap:wrap;",
        shiny::uiOutput(ns("count_badge")),
        shiny::selectInput(
          inputId  = ns("export_format_all"),
          label    = NULL,
          choices  = c("CSV" = "csv", "Excel (.xlsx)" = "xlsx",
                       "R Data (.rds)" = "rds", "JSON (.json)" = "json",
                       "TXT (tab)" = "txt"),
          selected = "csv",
          width    = "140px"
        ),
        shiny::downloadButton(
          outputId = ns("download_all"),
          label    = shiny::tagList(
            shiny::tags$i(class = "fa fa-download"),
            shiny::textOutput(ns("lbl_download_all"), inline = TRUE)
          ),
          class = "btn-sm"
        ),
        shiny::actionButton(
          inputId = ns("clear_all"),
          label   = shiny::tagList(
            shiny::tags$i(class = "fa fa-trash"),
            shiny::textOutput(ns("lbl_clear"), inline = TRUE)
          ),
          class = "btn btn-danger btn-sm"
        )
      )
    ),

    shiny::hr(),

    # ---- Contenido dinámico de tablas ----
    shiny::uiOutput(ns("tabs_ui"))
  )
}
