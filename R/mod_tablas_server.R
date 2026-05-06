mod_tablas_server <- function(id, est_results, est_meta, dict) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # ---- Etiquetas estáticas ----
    output$title           <- shiny::renderText({ i18n_t(dict(), "mod_tablas.title") })
    output$subtitle        <- shiny::renderText({ i18n_t(dict(), "mod_tablas.subtitle") })
    output$lbl_download_all <- shiny::renderText({ i18n_t(dict(), "mod_tablas.download_all") })
    output$lbl_clear       <- shiny::renderText({ i18n_t(dict(), "mod_tablas.clear_all") })

    # ---- Almacenamiento reactivo ----
    tables_store <- shiny::reactiveValues(items = list())
    counter      <- shiny::reactiveVal(0L)

    # IDs de outputs ya registrados (se vive en el closure del moduleServer)
    registered_outputs <- character(0)

    # ---- Capturar nuevo resultado de estimación ----
    shiny::observeEvent(est_results(), {
      res  <- est_results()
      meta <- est_meta()
      shiny::req(res)

      n       <- counter() + 1L
      counter(n)
      item_id <- paste0("est_", n)

      tables_store$items[[item_id]] <- list(
        n    = n,
        meta = if (is.null(meta)) {
          list(
            estimator = "?",
            variable  = "",
            domains   = character(0),
            timestamp = Sys.time()
          )
        } else {
          meta
        },
        data = res
      )
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    # ---- Limpiar todo ----
    shiny::observeEvent(input$clear_all, {
      tables_store$items <- list()
      counter(0L)
      registered_outputs <<- character(0)
    })

    # ---- Badge de conteo ----
    output$count_badge <- shiny::renderUI({
      n <- length(tables_store$items)
      d <- dict()
      shiny::tags$span(
        style = paste0(
          "background:", if (n > 0) "#009FDB" else "#4A6572", ";",
          "color:white; padding:4px 10px; border-radius:12px;",
          "font-size:12px; font-weight:600; white-space:nowrap;"
        ),
        paste(n, i18n_t(d, "mod_tablas.count_badge"))
      )
    })

    # ---- Descarga consolidada ----
    output$download_all <- shiny::downloadHandler(
      filename = function() {
        fmt <- input$export_format_all
        if (is.null(fmt)) fmt <- "csv"
        ext <- switch(fmt,
          "xlsx" = ".xlsx", "rds" = ".rds",
          "json" = ".json", "txt" = ".txt", ".csv"
        )
        paste0("estimaciones_", format(Sys.time(), "%Y%m%d_%H%M%S"), ext)
      },
      content = function(file) {
        items <- tables_store$items
        shiny::req(length(items) > 0)
        fmt <- input$export_format_all
        if (is.null(fmt)) fmt <- "csv"

        all_data <- lapply(names(items), function(iid) {
          item <- items[[iid]]
          df   <- item$data
          meta <- item$meta
          df$tabla_id  <- item$n
          df$estimador <- meta$estimator
          df$variable  <- if (nchar(meta$variable) > 0) meta$variable else ""
          df$dominios  <- if (length(meta$domains) > 0 && !all(meta$domains == "")) {
            paste(meta$domains, collapse = ", ")
          } else {
            "global"
          }
          df
        })

        combined <- do.call(dplyr::bind_rows, all_data)

        switch(fmt,
          "xlsx" = writexl::write_xlsx(combined, file),
          "rds"  = saveRDS(combined, file),
          "json" = jsonlite::write_json(combined, file, pretty = TRUE),
          "txt"  = utils::write.table(combined, file, sep = "\t", row.names = FALSE),
          utils::write.csv(combined, file, row.names = FALSE)
        )
      }
    )

    # ---- Registrar output DT + descarga por tabla (sólo IDs nuevos) ----
    shiny::observe({
      items   <- tables_store$items
      new_ids <- setdiff(names(items), registered_outputs)

      for (item_id in new_ids) {
        local({
          lid <- item_id

          output[[paste0("dt_", lid)]] <- DT::renderDT({
            item <- tables_store$items[[lid]]
            shiny::req(item)
            item$data %>%
              dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4))) %>%
              DT::datatable(
                rownames = FALSE,
                options  = list(pageLength = 10, scrollX = TRUE)
              )
          })

          output[[paste0("dl_", lid)]] <- shiny::downloadHandler(
            filename = function() {
              fmt <- input[[paste0("fmt_", lid)]]
              if (is.null(fmt)) fmt <- "csv"
              ext <- switch(fmt,
                "xlsx" = ".xlsx", "rds" = ".rds",
                "json" = ".json", "txt" = ".txt", ".csv"
              )
              paste0("estimacion_", lid, "_",
                     format(Sys.time(), "%Y%m%d_%H%M%S"), ext)
            },
            content = function(file) {
              item <- tables_store$items[[lid]]
              shiny::req(item)
              fmt <- input[[paste0("fmt_", lid)]]
              if (is.null(fmt)) fmt <- "csv"

              switch(fmt,
                "xlsx" = writexl::write_xlsx(item$data, file),
                "rds"  = saveRDS(item$data, file),
                "json" = jsonlite::write_json(item$data, file, pretty = TRUE),
                "txt"  = utils::write.table(item$data, file, sep = "\t", row.names = FALSE),
                utils::write.csv(item$data, file, row.names = FALSE)
              )
            }
          )
        })
        registered_outputs <<- c(registered_outputs, item_id)
      }
    })

    # ---- UI principal: pestañas dinámicas ----
    output$tabs_ui <- shiny::renderUI({
      items <- tables_store$items
      d     <- dict()

      if (length(items) == 0) {
        return(
          shiny::div(
            style = "text-align:center; padding:80px 20px; color:#4A6572;",
            shiny::tags$i(
              class = "fa fa-table",
              style = "font-size:48px; margin-bottom:16px; display:block; color:#009FDB;"
            ),
            shiny::tags$h4(
              i18n_t(d, "mod_tablas.no_tables"),
              style = "color:#4A6572; margin-bottom:8px;"
            ),
            shiny::tags$p(
              i18n_t(d, "mod_tablas.no_tables_hint"),
              style = "max-width:400px; margin:0 auto;"
            )
          )
        )
      }

      # Etiquetas i18n de estimadores (reutiliza claves del módulo de estimación)
      est_labels <- stats::setNames(
        c(i18n_t(d, "mod_estimacion.mean"),
          i18n_t(d, "mod_estimacion.total"),
          i18n_t(d, "mod_estimacion.prop"),
          i18n_t(d, "mod_estimacion.ratio"),
          i18n_t(d, "mod_estimacion.quantile")),
        c("mean", "total", "prop", "ratio", "quantile")
      )

      # Más reciente primero (reverse)
      tabs <- lapply(rev(names(items)), function(item_id) {
        item <- items[[item_id]]
        meta <- item$meta

        est_lbl   <- est_labels[meta$estimator]
        if (is.na(est_lbl)) est_lbl <- meta$estimator
        var_part  <- if (nchar(meta$variable) > 0) paste0(" \u00b7 ", meta$variable) else ""
        tab_title <- paste0("#", item$n, " ", est_lbl, var_part)

        dom_text <- if (length(meta$domains) > 0 && !all(meta$domains == "")) {
          paste(meta$domains, collapse = ", ")
        } else {
          i18n_t(d, "mod_tablas.global")
        }

        shiny::tabPanel(
          title = tab_title,
          value = item_id,

          shiny::br(),

          # Fila de metadatos
          shiny::div(
            class = "well",
            style = "padding:12px 16px; margin-bottom:16px; display:flex;
                     gap:32px; flex-wrap:wrap; font-size:13px;",

            shiny::div(
              shiny::tags$small(
                i18n_t(d, "mod_tablas.estimator_lbl"),
                style = "color:#4A6572; display:block; text-transform:uppercase;
                         font-size:10px; letter-spacing:.06em;"
              ),
              shiny::tags$strong(est_lbl)
            ),

            shiny::div(
              shiny::tags$small(
                i18n_t(d, "mod_tablas.variable_lbl"),
                style = "color:#4A6572; display:block; text-transform:uppercase;
                         font-size:10px; letter-spacing:.06em;"
              ),
              shiny::tags$strong(
                if (nchar(meta$variable) > 0) meta$variable else "\u2014"
              )
            ),

            shiny::div(
              shiny::tags$small(
                i18n_t(d, "mod_tablas.domains_lbl"),
                style = "color:#4A6572; display:block; text-transform:uppercase;
                         font-size:10px; letter-spacing:.06em;"
              ),
              shiny::tags$strong(dom_text)
            ),

            shiny::div(
              shiny::tags$small(
                i18n_t(d, "mod_tablas.timestamp_lbl"),
                style = "color:#4A6572; display:block; text-transform:uppercase;
                         font-size:10px; letter-spacing:.06em;"
              ),
              shiny::tags$strong(format(meta$timestamp, "%H:%M:%S"))
            ),

            shiny::div(
              shiny::tags$small(
                i18n_t(d, "mod_tablas.rows_lbl"),
                style = "color:#4A6572; display:block; text-transform:uppercase;
                         font-size:10px; letter-spacing:.06em;"
              ),
              shiny::tags$strong(nrow(item$data))
            )
          ),

          # Selector de formato + botón de descarga individual
          shiny::div(
            style = "display:flex; gap:8px; align-items:center; flex-wrap:wrap;",
            shiny::selectInput(
              inputId  = ns(paste0("fmt_", item_id)),
              label    = NULL,
              choices  = c("CSV" = "csv", "Excel (.xlsx)" = "xlsx",
                           "R Data (.rds)" = "rds", "JSON (.json)" = "json",
                           "TXT (tab)" = "txt"),
              selected = "csv",
              width    = "140px"
            ),
            shiny::downloadButton(
              outputId = ns(paste0("dl_", item_id)),
              label    = shiny::tagList(
                shiny::tags$i(class = "fa fa-download"),
                paste0(" ", i18n_t(d, "mod_tablas.download_table"))
              ),
              class = "btn-sm"
            )
          ),

          shiny::br(), shiny::br(),

          # Tabla de datos
          DT::DTOutput(ns(paste0("dt_", item_id)))
        )
      })

      do.call(
        shiny::tabsetPanel,
        c(tabs, list(type = "tabs", id = ns("active_tab")))
      )
    })
  })
}
