mod_variables_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(

    # ── Alerta si no hay diseño construido ──────────────────────────
    shiny::uiOutput(ns("no_design_alert")),

    shiny::fluidRow(

      # ── Columna izquierda: formulario de creación ───────────────
      shiny::column(
        width = 5,
        shiny::wellPanel(

          # Selector de modo
          shiny::div(
            style = "margin-bottom:14px;",
            shiny::radioButtons(
              ns("mode"),
              label        = NULL,
              choiceNames  = list(
                shiny::tagList(
                  shiny::tags$i(class = "fa fa-mouse-pointer"), " ",
                  shiny::textOutput(ns("lbl_mode_basic"), inline = TRUE)
                ),
                shiny::tagList(
                  shiny::tags$i(class = "fa fa-code"), " ",
                  shiny::textOutput(ns("lbl_mode_expert"), inline = TRUE)
                )
              ),
              choiceValues = list("basic", "expert"),
              selected     = "basic",
              inline       = TRUE
            )
          ),

          shiny::hr(style = "margin:4px 0 12px;"),

          # ── Modo básico ─────────────────────────────────────────
          shiny::conditionalPanel(
            condition = paste0("input['", ns("mode"), "'] == 'basic'"),

            shiny::textInput(
              ns("var_name"),
              label       = shiny::textOutput(ns("lbl_var_name"), inline = TRUE),
              placeholder = "ej: ingreso_pc"
            ),

            shiny::selectInput(
              ns("op_type"),
              label   = shiny::textOutput(ns("lbl_op_type"), inline = TRUE),
              choices = c("arithmetic", "mathfun", "indicator", "recode", "cut")
            ),

            shiny::uiOutput(ns("basic_form"))
          ),

          # ── Modo experto ────────────────────────────────────────
          shiny::conditionalPanel(
            condition = paste0("input['", ns("mode"), "'] == 'expert'"),

            shiny::tags$small(
              shiny::textOutput(ns("lbl_expert_hint"), inline = TRUE),
              style = "color:#4A6572; display:block; margin-bottom:4px;"
            ),
            shiny::tags$code(
              "nueva_var <- expresion",
              style = paste(
                "font-size:11px; display:block; margin-bottom:10px;",
                "color:#002856; background:#E0F4FD; padding:4px 8px; border-radius:4px;"
              )
            ),
            shiny::textAreaInput(
              ns("expert_code"),
              label       = shiny::textOutput(ns("lbl_expert_label"), inline = TRUE),
              rows        = 6,
              placeholder = paste0(
                "ingreso_pc <- ingreso / miembros\n",
                "pobre      <- ifelse(ingreso_pc < 200, 1L, 0L)\n",
                "log_ing    <- log(ingreso)"
              )
            )
          ),

          shiny::hr(style = "margin:12px 0 8px;"),

          # Botones de acción
          shiny::div(
            style = "display:flex; gap:8px;",
            shiny::actionButton(
              ns("add_var"),
              shiny::tagList(
                shiny::tags$i(class = "fa fa-plus"), " ",
                shiny::textOutput(ns("lbl_btn_add"), inline = TRUE)
              ),
              class = "btn-primary btn-sm"
            ),
            shiny::actionButton(
              ns("preview_var"),
              shiny::tagList(
                shiny::tags$i(class = "fa fa-eye"), " ",
                shiny::textOutput(ns("lbl_btn_preview"), inline = TRUE)
              ),
              class = "btn-default btn-sm"
            )
          ),

          # Panel de previsualización del código R generado
          shiny::uiOutput(ns("preview_panel"))
        )
      ),

      # ── Columna derecha: lista de variables del diseño ──────────
      shiny::column(
        width = 7,
        shiny::wellPanel(
          shiny::div(
            style = paste(
              "display:flex; align-items:center;",
              "justify-content:space-between; margin-bottom:14px;"
            ),
            shiny::div(
              shiny::h5(
                shiny::textOutput(ns("lbl_vars_title"), inline = TRUE),
                style = "margin:0 0 2px;"
              ),
              shiny::tags$small(
                shiny::textOutput(ns("vars_count_text"), inline = TRUE),
                style = "color:#8AA5B0;"
              )
            ),
            shiny::actionButton(
              ns("clear_vars"),
              shiny::tagList(shiny::tags$i(class = "fa fa-trash")),
              class = "btn-danger btn-sm",
              title = "Eliminar todas las variables derivadas"
            )
          ),
          shiny::uiOutput(ns("vars_table"))
        )
      )
    )
  )
}
