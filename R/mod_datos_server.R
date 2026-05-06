mod_datos_server <- function(id, dict) {
  shiny::moduleServer(id, function(input, output, session) {

    output$title <- shiny::renderText({
      i18n_t(dict(), "mod_datos.title")
    })

    output$input_panel <- shiny::renderText({
      i18n_t(dict(), "mod_datos.input_panel")
    })

    output$source_card <- shiny::renderText({
      i18n_t(dict(), "mod_datos.source_card")
    })

    output$preview_card <- shiny::renderText({
      i18n_t(dict(), "mod_datos.preview_card")
    })

    output$status <- shiny::renderText({
      i18n_t(dict(), "mod_datos.status")
    })

    data_r <- shiny::reactiveVal(NULL)

    output$file_ui <- shiny::renderUI({
      shiny::tagList(
        shiny::fileInput(
          session$ns("file"),
          label  = i18n_t(dict(), "mod_datos.file_label"),
          accept = c(
            ".csv", ".tsv", ".txt",
            ".xlsx", ".xls",
            ".sav", ".por",
            ".dta",
            ".sas7bdat", ".xpt",
            ".rds"
          )
        ),
        shiny::tags$small(
          shiny::textOutput(session$ns("file_hint"), inline = TRUE),
          style = "color:#4A6572; display:block; margin-top:-10px; margin-bottom:8px;"
        )
      )
    })

    output$file_hint <- shiny::renderText({
      i18n_t(dict(), "mod_datos.file_hint")
    })

    shiny::observe({
      shiny::updateActionButton(
        session, "load_example",
        label = i18n_t(dict(), "mod_datos.example_btn")
      )
    })

    shiny::observeEvent(input$file, {

      req(input$file)

      ext <- tolower(tools::file_ext(input$file$name))
      tmp <- paste0(input$file$datapath, ".", ext)
      file.copy(input$file$datapath, tmp)
      on.exit(unlink(tmp), add = TRUE)

      tryCatch({
        df      <- read_survey_data(tmp)
        data_r(df)
      }, error = function(e) {
        shiny::showNotification(
          paste(i18n_t(dict(), "mod_datos.load_error"), conditionMessage(e)),
          type     = "error",
          duration = 10
        )
      })

    }, ignoreInit = TRUE)

    shiny::observeEvent(input$load_example, {

      df <- complexr::generate_example_data()
      data_r(df)

    }, ignoreInit = TRUE)

    output$preview <- DT::renderDT({
      req(data_r())
      head(data_r(), 100)
    })

    output$log <- shiny::renderPrint({

      if (is.null(data_r())) {
        return(i18n_t(dict(), "mod_datos.no_data"))
      }

      d <- dict()
      stats::setNames(
        list(nrow(data_r()), ncol(data_r()), names(data_r())),
        c(i18n_t(d, "mod_datos.log_rows"),
          i18n_t(d, "mod_datos.log_cols"),
          i18n_t(d, "mod_datos.log_vars"))
      )
    })

    return(list(
      data = shiny::reactive({ data_r() })
    ))
  })
}
