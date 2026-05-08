mod_inferencia_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(

    # -- Encabezado --------------------------------------------------------
    shiny::tags$div(
      class = "page-header page-header--inferencia",
      shiny::tags$div(
        class = "page-header-icon",
        shiny::tags$i(class = "fa fa-flask")
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

      # -- Columna izquierda: par\u00e1metros -----------------------------------
      shiny::column(
        3,
        shiny::wellPanel(
          class = "sidebar-card sidebar-card--inferencia",

          shiny::tags$div(
            class = "card-header-block card-header-block--inferencia",
            shiny::tags$i(class = "fa fa-sliders"),
            shiny::textOutput(ns("params_card"), inline = TRUE)
          ),

          shiny::tags$div(
            class = "card-body-block",

            shiny::selectInput(
              inputId = ns("analysis_type"),
              label   = "Tipo de an\u00e1lisis",
              choices = c(
                "Coeficiente de Gini"  = "gini",
                "Correlaci\u00f3n"     = "correlation",
                "Prueba de hip\u00f3tesis" = "hyptest",
                "Tablas cruzadas"      = "crosstable",
                "Contrastes"           = "contrasts"
              )
            ),

            # -- Gini ----
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'gini'", ns("analysis_type")),
              shiny::selectInput(ns("gini_var"),    label = "Variable",           choices = NULL),
              shiny::selectInput(ns("gini_domain"), label = "Dominio (opcional)", choices = NULL)
            ),

            # -- Correlaci\u00f3n ----
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'correlation'", ns("analysis_type")),
              shiny::selectInput(ns("cor_x"), label = "Variable X", choices = NULL),
              shiny::selectInput(ns("cor_y"), label = "Variable Y", choices = NULL)
            ),

            # -- Prueba de hip\u00f3tesis ----
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'hyptest'", ns("analysis_type")),
              shiny::selectInput(ns("hyp_y"),     label = "Variable de respuesta (num\u00e9rica)", choices = NULL),
              shiny::selectInput(ns("hyp_group"), label = "Variable de grupo (2 categor\u00edas)", choices = NULL)
            ),

            # -- Tablas cruzadas ----
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'crosstable'", ns("analysis_type")),
              shiny::selectInput(ns("ct_row"), label = "Variable fila", choices = NULL),
              shiny::selectInput(ns("ct_col"), label = "Variable columna", choices = NULL),
              shiny::selectInput(
                ns("ct_statistic"),
                label   = "Estad\u00edstico de prueba",
                choices = c(
                  "F (Rao-Scott)"    = "F",
                  "Chi\u00b2 (Rao-Scott)" = "Chisq",
                  "Wald"             = "Wald"
                )
              )
            ),

            # -- Contrastes ----
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'contrasts'", ns("analysis_type")),

              shiny::selectInput(
                ns("ctr_y"),
                label   = "Variable de respuesta (num\u00e9rica)",
                choices = NULL
              ),
              shiny::selectInput(
                ns("ctr_group"),
                label   = "Variable de grupo",
                choices = NULL
              ),

              shiny::radioButtons(
                inputId  = ns("ctr_mode"),
                label    = "Modo",
                choices  = c("Todos los pares" = "all_pairs",
                             "Personalizado"    = "custom"),
                selected = "all_pairs",
                inline   = TRUE
              ),

              # Opciones modo autom\u00e1tico
              shiny::conditionalPanel(
                condition = sprintf("input['%s'] == 'all_pairs'", ns("ctr_mode")),
                shiny::checkboxInput(
                  ns("ctr_bonferroni"),
                  label = "Correcci\u00f3n de Bonferroni",
                  value = FALSE
                )
              ),

              # Botones modo personalizado
              shiny::conditionalPanel(
                condition = sprintf("input['%s'] == 'custom'", ns("ctr_mode")),
                shiny::tags$p(
                  shiny::textOutput(ns("ctr_matrix_hint"), inline = TRUE),
                  style = "font-size:12px; color:#666; margin-bottom:6px;"
                ),
                shiny::fluidRow(
                  shiny::column(
                    6,
                    shiny::actionButton(
                      ns("ctr_add"),
                      label = shiny::tagList(
                        shiny::tags$i(class = "fa fa-plus"), " ",
                        shiny::textOutput(ns("ctr_add_lbl"), inline = TRUE)
                      ),
                      class = "btn-sm btn-default btn-block"
                    )
                  ),
                  shiny::column(
                    6,
                    shiny::actionButton(
                      ns("ctr_del"),
                      label = shiny::tagList(
                        shiny::tags$i(class = "fa fa-trash"), " ",
                        shiny::textOutput(ns("ctr_del_lbl"), inline = TRUE)
                      ),
                      class = "btn-sm btn-danger btn-block"
                    )
                  )
                )
              )
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

      # -- Columna derecha: teor\u00eda + editor + resultados -------------------
      shiny::column(
        9,
        shiny::wellPanel(

          # Marco te\u00f3rico
          shiny::tags$div(
            class = "card-header-block card-header-block--inferencia",
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
            class = "card-header-block card-header-block--inferencia",
            shiny::tags$i(class = "fa fa-info-circle"),
            shiny::textOutput(ns("status_title"), inline = TRUE)
          ),
          shiny::tags$div(
            class = "card-body-block",
            shiny::verbatimTextOutput(ns("log"))
          ),

          shiny::hr(),

          # \u2500\u2500 Editor de matriz de contrastes (solo modo personalizado) \u2500\u2500\u2500\u2500\u2500
          shiny::conditionalPanel(
            condition = sprintf(
              "input['%s'] == 'contrasts' && input['%s'] == 'custom'",
              ns("analysis_type"), ns("ctr_mode")
            ),
            shiny::tags$div(
              class = "card-header-block card-header-block--inferencia",
              shiny::tags$i(class = "fa fa-th"),
              shiny::textOutput(ns("ctr_matrix_title"), inline = TRUE)
            ),
            shiny::tags$div(
              class = "card-body-block",
              shiny::tags$p(
                shiny::HTML(
                  "<small style='color:#555;'>
                   Edita los coeficientes directamente en la tabla. Selecciona filas
                   para eliminarlas con el bot\u00f3n <b>Eliminar</b> del panel izquierdo.
                   Cada fila define un contraste lineal: \\(\\hat{\\delta} =
                   \\mathbf{c}^\\top\\hat{\\boldsymbol{\\mu}}\\).
                   </small>"
                )
              ),
              DT::DTOutput(ns("contrast_matrix")),
              shiny::tags$p(
                style = "margin-top:6px; font-size:12px; color:#888;",
                shiny::textOutput(ns("ctr_sum_hint"), inline = TRUE)
              )
            ),
            shiny::hr()
          ),

          # Resultados
          shiny::tags$div(
            class = "card-header-block card-header-block--inferencia",
            shiny::tags$i(class = "fa fa-table"),
            shiny::textOutput(ns("result_title"), inline = TRUE)
          ),
          shiny::tags$div(
            class = "card-body-block",
            shiny::uiOutput(ns("results_ui"))
          ),

          shiny::hr(),

          # Gr\u00e1fico
          shiny::tags$div(
            class = "card-header-block card-header-block--inferencia",
            shiny::tags$i(class = "fa fa-chart-bar"),
            shiny::textOutput(ns("plot_section"), inline = TRUE)
          ),
          shiny::tags$div(
            class = "card-body-block",
            shiny::plotOutput(ns("analysis_plot"), height = "420px")
          ),

          shiny::hr(),

          # Descarga
          shiny::tags$div(
            class = "card-header-block card-header-block--inferencia",
            shiny::tags$i(class = "fa fa-download"),
            shiny::textOutput(ns("download_title"), inline = TRUE)
          ),
          shiny::tags$div(
            class = "card-body-block",
            shiny::fluidRow(
              shiny::column(3, shiny::downloadButton(ns("dl_csv"),  "CSV",   class = "btn-sm btn-default")),
              shiny::column(3, shiny::downloadButton(ns("dl_xlsx"), "Excel", class = "btn-sm btn-default")),
              shiny::column(3, shiny::downloadButton(ns("dl_rds"),  "RDS",   class = "btn-sm btn-default"))
            )
          )
        )
      )
    )
  )
}
