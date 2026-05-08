#' Shiny server for complexr
#'
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session

app_server <- function(input, output, session) {

  # =========================
  # Idioma reactivo global
  # =========================
  lang <- shiny::reactiveVal("es")

  shiny::observeEvent(input$lang, {
    lang(input$lang)
  }, ignoreInit = TRUE)

  # =========================
  # Diccionario reactivo
  # =========================
  dict <- shiny::reactive({
    i18n_load(lang())
  })

  # =========================
  # Tabs traducidos
  # =========================
  output$tab_portada <- shiny::renderText({
    i18n_t(dict(), "app.tabs.portada")
  })
  output$tab_datos <- shiny::renderText({
    i18n_t(dict(), "app.tabs.datos")
  })
  output$tab_diseno <- shiny::renderText({
    i18n_t(dict(), "app.tabs.diseno")
  })
  output$tab_estimacion <- shiny::renderText({
    i18n_t(dict(), "app.tabs.estimacion")
  })
  output$tab_inferencia <- shiny::renderText({
    i18n_t(dict(), "app.tabs.inferencia")
  })
  output$tab_tablas <- shiny::renderText({
    i18n_t(dict(), "app.tabs.tablas")
  })

  # =========================
  # Portada: textos reactivos
  # =========================
  output$cover_app_active <- shiny::renderText({
    i18n_t(dict(), "cover.app_active")
  })
  output$cover_desc <- shiny::renderText({
    i18n_t(dict(), "cover.desc")
  })
  output$cover_btn <- shiny::renderText({
    i18n_t(dict(), "cover.btn")
  })
  output$cover_feat1_title <- shiny::renderText({
    i18n_t(dict(), "cover.feat1.title")
  })
  output$cover_feat1_desc <- shiny::renderText({
    i18n_t(dict(), "cover.feat1.desc")
  })
  output$cover_feat2_title <- shiny::renderText({
    i18n_t(dict(), "cover.feat2.title")
  })
  output$cover_feat2_desc <- shiny::renderText({
    i18n_t(dict(), "cover.feat2.desc")
  })
  output$cover_feat3_title <- shiny::renderText({
    i18n_t(dict(), "cover.feat3.title")
  })
  output$cover_feat3_desc <- shiny::renderText({
    i18n_t(dict(), "cover.feat3.desc")
  })

  output$app_footer <- shiny::renderText({

    i18n_t(dict(), "app.footer")

  })
  output$btn_exit <- shiny::renderText({
    i18n_t(dict(), "app.exit_btn")
  })

  shiny::observeEvent(input$exit_app, {
    shiny::stopApp()
  })

  # Botón CTA de portada → navega a la pestaña Datos
  shiny::observeEvent(input$cover_btn_start, {
    shiny::updateNavbarPage(session, "navbar", selected = "datos")
  })

  # =========================
  # Módulo: Datos
  # =========================
  datos_res <- mod_datos_server("datos", dict)

  # =========================
  # Módulo: Diseño muestral
  # =========================
  diseno_res <- mod_diseno_server("diseno", datos_res$data, dict)

  # =========================
  # Módulo: Estimación
  # =========================
  estimacion_res <- mod_estimacion_server("estimacion", diseno_res$design, dict)

  # =========================
  # Módulo: Inferencia estadística
  # =========================
  inferencia_res <- mod_inferencia_server("inferencia", diseno_res$design, dict)

  # =========================
  # Módulo: Tablas de estimaciones
  # =========================
  mod_tablas_server(
    "tablas",
    estimacion_res$results,
    estimacion_res$meta,
    inferencia_res$results,
    inferencia_res$meta,
    dict
  )
}
