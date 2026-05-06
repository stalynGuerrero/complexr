mod_diseno_server <- function(id, data, dict) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # -------------------------------------------------
    # 0. Static labels
    # -------------------------------------------------
    output$title             <- shiny::renderText({ i18n_t(dict(), "mod_diseno.title") })
    output$subtitle          <- shiny::renderText({ i18n_t(dict(), "mod_diseno.subtitle") })
    output$params_card       <- shiny::renderText({ i18n_t(dict(), "mod_diseno.params_card") })
    output$build_btn         <- shiny::renderText({ i18n_t(dict(), "mod_diseno.build_btn") })
    output$results_card      <- shiny::renderText({ i18n_t(dict(), "mod_diseno.results_card") })
    output$design_summary    <- shiny::renderText({ i18n_t(dict(), "mod_diseno.design_summary") })
    output$r_code            <- shiny::renderText({ i18n_t(dict(), "mod_diseno.r_code") })
    output$diagnostic        <- shiny::renderText({ i18n_t(dict(), "mod_diseno.diagnostic") })
    output$lbl_tab_config    <- shiny::renderText({ i18n_t(dict(), "mod_diseno.tab_config") })
    output$lbl_tab_variables <- shiny::renderText({ i18n_t(dict(), "mod_diseno.tab_variables") })

    # -------------------------------------------------
    # 0b. Update selectInput choices when language changes
    # -------------------------------------------------
    shiny::observe({
      d <- dict()
      shiny::updateSelectInput(session, "design_type",
        label   = i18n_t(d, "mod_diseno.design_type"),
        choices = stats::setNames(
          c("srs", "stratified", "cluster"),
          c(i18n_t(d, "mod_diseno.srs"),
            i18n_t(d, "mod_diseno.stratified"),
            i18n_t(d, "mod_diseno.cluster"))
        )
      )
    })

    # -------------------------------------------------
    # 1. Theory panel (MathJax)
    # -------------------------------------------------
    output$design_theory <- shiny::renderUI({

      shiny::req(input$design_type)
      d <- dict()

      switch(
        input$design_type,

        "srs" = shiny::tagList(
          shiny::withMathJax(),
          shiny::h5(i18n_t(d, "mod_diseno.theory.srs.title")),
          shiny::p(i18n_t(d, "mod_diseno.theory.srs.desc")),
          shiny::h6(i18n_t(d, "mod_diseno.theory.srs.estimator_h")),
          shiny::HTML("$$ \\hat{\\bar{Y}} = \\frac{1}{n} \\sum_{i=1}^{n} y_i $$"),
          shiny::h6(i18n_t(d, "mod_diseno.theory.srs.variance_h")),
          shiny::HTML("$$ Var(\\hat{\\bar{Y}}) = \\left(1-\\frac{n}{N}\\right)\\frac{S^2}{n} $$"),
          shiny::tags$ul(
            shiny::tags$li(shiny::HTML(i18n_t(d, "mod_diseno.theory.srs.yi_desc"))),
            shiny::tags$li(shiny::HTML(i18n_t(d, "mod_diseno.theory.srs.n_desc"))),
            shiny::tags$li(shiny::HTML(i18n_t(d, "mod_diseno.theory.srs.N_desc"))),
            shiny::tags$li(shiny::HTML(i18n_t(d, "mod_diseno.theory.srs.S2_desc")))
          )
        ),

        "stratified" = shiny::tagList(
          shiny::withMathJax(),
          shiny::h5(i18n_t(d, "mod_diseno.theory.stratified.title")),
          shiny::p(i18n_t(d, "mod_diseno.theory.stratified.desc")),
          shiny::h6(i18n_t(d, "mod_diseno.theory.stratified.estimator_h")),
          shiny::HTML("$$ \\hat{\\bar{Y}}_{st} = \\sum_{h=1}^{H} W_h \\bar{y}_h $$"),
          shiny::h6(i18n_t(d, "mod_diseno.theory.stratified.variance_h")),
          shiny::HTML("$$ Var(\\hat{\\bar{Y}}_{st}) = \\sum_{h=1}^{H} W_h^2 \\left(1-\\frac{n_h}{N_h}\\right) \\frac{S_h^2}{n_h} $$"),
          shiny::tags$ul(
            shiny::tags$li(shiny::HTML(i18n_t(d, "mod_diseno.theory.stratified.H_desc"))),
            shiny::tags$li(shiny::HTML(i18n_t(d, "mod_diseno.theory.stratified.Wh_desc"))),
            shiny::tags$li(shiny::HTML(i18n_t(d, "mod_diseno.theory.stratified.nh_desc"))),
            shiny::tags$li(shiny::HTML(i18n_t(d, "mod_diseno.theory.stratified.S2h_desc")))
          )
        ),

        "cluster" = shiny::tagList(
          shiny::withMathJax(),
          shiny::h5(i18n_t(d, "mod_diseno.theory.cluster.title")),
          shiny::p(i18n_t(d, "mod_diseno.theory.cluster.desc")),
          shiny::h6(i18n_t(d, "mod_diseno.theory.cluster.estimator_h")),
          shiny::HTML("$$ \\hat{Y} = \\sum_{i \\in s} \\frac{y_i}{\\pi_i} $$"),
          shiny::h6(i18n_t(d, "mod_diseno.theory.cluster.variance_h")),
          shiny::HTML("$$ Var(\\hat{Y}) = \\sum_i \\sum_j \\left(\\frac{\\pi_{ij}-\\pi_i\\pi_j}{\\pi_{ij}}\\right) \\frac{y_i}{\\pi_i} \\frac{y_j}{\\pi_j} $$"),
          shiny::tags$ul(
            shiny::tags$li(shiny::HTML(i18n_t(d, "mod_diseno.theory.cluster.pi_desc"))),
            shiny::tags$li(shiny::HTML(i18n_t(d, "mod_diseno.theory.cluster.piij_desc"))),
            shiny::tags$li(shiny::HTML(i18n_t(d, "mod_diseno.theory.cluster.wi_desc")))
          )
        )
      )
    })

    # -------------------------------------------------
    # 2. Dynamic design arguments
    # -------------------------------------------------
    output$design_arguments <- shiny::renderUI({

      shiny::req(data())
      vars <- names(data())
      d    <- dict()

      switch(
        input$design_type,

        "srs" = shiny::tagList(
          shiny::selectInput(ns("weight_var"),
            i18n_t(d, "mod_diseno.weight_var"), choices = vars)
        ),

        "stratified" = shiny::tagList(
          shiny::selectInput(ns("strata_var"),
            i18n_t(d, "mod_diseno.strata_var"), choices = vars),
          shiny::selectInput(ns("weight_var"),
            i18n_t(d, "mod_diseno.weight_var"), choices = vars)
        ),

        "cluster" = shiny::tagList(
          shiny::numericInput(ns("n_stages"),
            i18n_t(d, "mod_diseno.n_stages"), value = 2, min = 1, max = 5),
          shiny::uiOutput(ns("stage_clusters")),
          shiny::selectInput(ns("strata_var"),
            i18n_t(d, "mod_diseno.strata"),
            choices = c(stats::setNames("", i18n_t(d, "mod_diseno.none")), vars)),
          shiny::selectInput(ns("weight_var"),
            i18n_t(d, "mod_diseno.weight_var"), choices = vars),
          shiny::selectInput(
            ns("lonely_psu"),
            i18n_t(d, "mod_diseno.lonely_psu"),
            choices = stats::setNames(
              c("adjust", "average", "certainty"),
              c(i18n_t(d, "mod_diseno.lonely_adjust"),
                i18n_t(d, "mod_diseno.lonely_average"),
                i18n_t(d, "mod_diseno.lonely_certainty"))
            ),
            selected = "adjust"
          )
        )
      )
    })

    # -------------------------------------------------
    # 3. Dynamic stage cluster inputs
    # -------------------------------------------------
    output$stage_clusters <- shiny::renderUI({

      shiny::req(input$n_stages, data())
      vars <- names(data())
      d    <- dict()

      lapply(seq_len(input$n_stages), function(i) {
        shiny::selectInput(
          ns(paste0("cluster_stage_", i)),
          paste(i18n_t(d, "mod_diseno.cluster_stage"), i),
          choices = vars
        )
      })
    })

    # -------------------------------------------------
    # 4. Helper
    # -------------------------------------------------
    clean_input <- function(x) {
      if (is.null(x) || length(x) == 0 || nchar(trimws(x)) == 0) return(NULL)
      x
    }

    # -------------------------------------------------
    # 5. Reactive log
    # -------------------------------------------------
    log_design <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$design_type, {
      log_design(NULL)
    }, ignoreInit = TRUE)

    # -------------------------------------------------
    # 6. Build design
    # -------------------------------------------------
    design_r <- shiny::eventReactive(input$build, {

      df <- data()
      shiny::req(df, input$design_type, input$weight_var)

      weight <- input$weight_var
      strata <- clean_input(input$strata_var)

      tryCatch({

        des <- if (input$design_type == "srs") {

          formula_design <- paste0("svydesign(id=~1, weights=~", weight, ", data=data)")
          as_survey_design_tbl(data = df, weight = weight)

        } else if (input$design_type == "stratified") {

          shiny::req(strata)
          formula_design <- paste0(
            "svydesign(id=~1, strata=~", strata, ", weights=~", weight, ", data=data)"
          )
          as_survey_design_tbl(data = df, weight = weight, strata = strata)

        } else {

          clusters <- sapply(
            seq_len(input$n_stages),
            function(i) input[[paste0("cluster_stage_", i)]]
          )
          lonely <- input$lonely_psu
          if (!is.null(lonely) && nchar(lonely) > 0) {
            options(survey.lonely.psu = lonely)
          }

          formula_design <- paste0(
            "svydesign(id=~", paste(clusters, collapse = "+"),
            if (!is.null(strata)) paste0(", strata=~", strata) else "",
            ", weights=~", weight, ", nest=TRUE, data=data)"
          )

          as_survey_design_tbl(
            data    = df,
            weight  = weight,
            strata  = strata,
            cluster = clusters[1],
            nest    = TRUE
          )
        }

        log_design(list(
          tipo       = input$design_type,
          formula_R  = formula_design,
          lonely_psu = input$lonely_psu
        ))

        des

      }, error = function(e) {
        shiny::showNotification(
          paste(i18n_t(dict(), "mod_diseno.error_build"), conditionMessage(e)),
          type = "error", duration = 8
        )
        NULL
      })

    }, ignoreInit = TRUE)

    # -------------------------------------------------
    # 7. R code output
    # -------------------------------------------------
    output$design_code <- shiny::renderText({
      shiny::req(log_design())
      log_design()$formula_R
    })

    # -------------------------------------------------
    # 8. Design log
    # -------------------------------------------------
    output$log <- shiny::renderPrint({
      log_design()
    })

    # -------------------------------------------------
    # 9. Diagnostic table
    # -------------------------------------------------
    output$summary <- shiny::renderTable({
      shiny::req(design_r())
      tryCatch(
        describe_survey_design(design_r()),
        error = function(e) NULL
      )
    }, digits = 2)

    # -------------------------------------------------
    # 10. Variables sub-module (wired after design is built)
    # -------------------------------------------------
    vars_res <- mod_variables_server("variables", design_r, dict)

    # -------------------------------------------------
    # 11. Module output: design with all derived variables
    # -------------------------------------------------
    return(list(
      design = vars_res$design
    ))

  })
}
