# ---------------------------------------------------------------------------
# Internal: apply a block of expert R code (assignments) to a tbl_svy
# ---------------------------------------------------------------------------
.apply_expert_code <- function(des, code_str) {
  exprs <- tryCatch(rlang::parse_exprs(code_str), error = function(e) list())
  for (e in exprs) {
    call_name <- tryCatch(as.character(e[[1L]]), error = function(e) "")
    if (!(call_name %in% c("<-", "="))) next
    var_name <- tryCatch(as.character(e[[2L]]), error = function(e) "")
    if (!grepl("^[a-zA-Z.][a-zA-Z0-9._]*$", var_name)) next
    rhs <- e[[3L]]
    des <- dplyr::mutate(des, !!rlang::sym(var_name) := !!rhs)
  }
  des
}

# Internal: extract variable names from expert code block
.expert_var_names <- function(code_str) {
  exprs <- tryCatch(rlang::parse_exprs(code_str), error = function(e) list())
  nms <- character(0L)
  for (e in exprs) {
    call_name <- tryCatch(as.character(e[[1L]]), error = function(e) "")
    if (call_name %in% c("<-", "=")) {
      vn <- tryCatch(as.character(e[[2L]]), error = function(e) "")
      if (nchar(vn) > 0L) nms <- c(nms, vn)
    }
  }
  nms
}

# ---------------------------------------------------------------------------
# Module server
# ---------------------------------------------------------------------------
mod_variables_server <- function(id, design, dict) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # ── Storage ────────────────────────────────────────────────────
    vars_store  <- shiny::reactiveValues(items = list())
    var_counter <- shiny::reactiveVal(0L)
    recode_df   <- shiny::reactiveVal(
      data.frame(
        original = character(), nuevo = character(),
        stringsAsFactors = FALSE
      )
    )
    registered_delete <- character(0L)

    # ── Design with all derived variables applied ──────────────────
    design_final <- shiny::reactive({
      des <- design()
      if (is.null(des)) return(NULL)
      items <- vars_store$items
      if (length(items) == 0L) return(des)

      for (item in items) {
        des <- tryCatch({
          if (item$mode == "basic") {
            expr <- rlang::parse_expr(item$expr_str)
            dplyr::mutate(des, !!rlang::sym(item$var_name) := !!expr)
          } else {
            .apply_expert_code(des, item$expr_str)
          }
        }, error = function(e) des)  # skip silently on failure
      }
      des
    })

    # Convenience: all variable names currently in the design
    # (including derived)
    design_vars_all <- shiny::reactive({
      des <- design_final()
      if (is.null(des)) return(character(0L))
      names(des$variables)
    })

    design_vars_numeric <- shiny::reactive({
      des <- design_final()
      if (is.null(des)) return(character(0L))
      df <- des$variables
      names(df)[vapply(df, is.numeric, logical(1L))]
    })

    # ── Static labels ──────────────────────────────────────────────
    output$lbl_mode_basic   <- shiny::renderText({
      i18n_t(dict(), "mod_variables.mode_basic")
    })
    output$lbl_mode_expert  <- shiny::renderText({
      i18n_t(dict(), "mod_variables.mode_expert")
    })
    output$lbl_var_name     <- shiny::renderText({
      i18n_t(dict(), "mod_variables.var_name")
    })
    output$lbl_op_type      <- shiny::renderText({
      i18n_t(dict(), "mod_variables.op_type")
    })
    output$lbl_expert_hint  <- shiny::renderText({
      i18n_t(dict(), "mod_variables.expert_hint")
    })
    output$lbl_expert_label <- shiny::renderText({
      i18n_t(dict(), "mod_variables.expert_label")
    })
    output$lbl_btn_add      <- shiny::renderText({
      i18n_t(dict(), "mod_variables.btn_add")
    })
    output$lbl_btn_preview  <- shiny::renderText({
      i18n_t(dict(), "mod_variables.btn_preview")
    })
    output$lbl_vars_title   <- shiny::renderText({
      i18n_t(dict(), "mod_variables.vars_title")
    })

    # ── Language-aware selectInput choices ─────────────────────────
    shiny::observe({
      d <- dict()
      shiny::updateSelectInput(session, "op_type",
        choices = stats::setNames(
          c("arithmetic", "mathfun", "indicator", "recode", "cut"),
          c(i18n_t(d, "mod_variables.op_arithmetic"),
            i18n_t(d, "mod_variables.op_mathfun"),
            i18n_t(d, "mod_variables.op_indicator"),
            i18n_t(d, "mod_variables.op_recode"),
            i18n_t(d, "mod_variables.op_cut"))
        )
      )
    })

    # ── Alert when no design is built ─────────────────────────────
    output$no_design_alert <- shiny::renderUI({
      if (!is.null(design())) return(NULL)
      shiny::div(
        class = "alert alert-warning",
        role  = "alert",
        style = "margin-bottom:16px;",
        shiny::tags$i(class = "fa fa-exclamation-triangle"), " ",
        i18n_t(dict(), "mod_variables.no_design")
      )
    })

    # ── Variables count badge ──────────────────────────────────────
    output$vars_count_text <- shiny::renderText({
      des   <- design_final()
      d     <- dict()
      n_all <- if (is.null(des)) 0L else ncol(des$variables)
      n_new <- length(vars_store$items)
      n_ori <- n_all - n_new
      sprintf("%d %s \u00b7 %d %s",
        n_ori, i18n_t(d, "mod_variables.vars_original"),
        n_new, i18n_t(d, "mod_variables.vars_new"))
    })

    # -- Dynamic basic form
    output$basic_form <- shiny::renderUI({
      shiny::req(design())
      vars_all <- design_vars_all()
      vars_num <- design_vars_numeric()
      d        <- dict()
      switch(
        input$op_type,
        "arithmetic" = .arith_form_ui(ns, vars_all, d),
        "mathfun"    = .mathfun_form_ui(ns, vars_num, d),
        "indicator"  = .indicator_form_ui(ns, vars_all, d),
        "recode"     = .recode_form_ui(ns, vars_all, d),
        "cut"        = .cut_form_ui(ns, vars_num, d)
      )
    })

    # ── Recode DT: populate when source variable changes ──────────
    shiny::observeEvent(input$recode_src, {
      shiny::req(input$recode_src, design_final())
      vals <- tryCatch(
        sort(unique(
          as.character(
            design_final()$variables[[input$recode_src]]
          )
        )),
        error = function(e) character(0L)
      )
      vals <- head(vals, 50L)
      recode_df(data.frame(
        original = vals,
        nuevo    = vals,
        stringsAsFactors = FALSE
      ))
    }, ignoreInit = TRUE)

    output$recode_dt <- DT::renderDT({
      shiny::req(nrow(recode_df()) > 0L)
      d <- dict()
      DT::datatable(
        recode_df(),
        colnames = c(i18n_t(d, "mod_variables.recode_col_orig"),
                     i18n_t(d, "mod_variables.recode_col_new")),
        rownames = FALSE,
        editable = list(target = "cell", disable = list(columns = 0L)),
        options  = list(
          pageLength   = 15L,
          dom          = "t",
          scrollY      = "180px",
          scrollCollapse = TRUE
        )
      )
    })

    shiny::observeEvent(input$recode_dt_cell_edit, {
      recode_df(DT::editData(
        recode_df(), input$recode_dt_cell_edit, rownames = FALSE
      ))
    })

    # -- Build expression string from basic form
    .build_basic_expr <- function() {
      switch(
        input$op_type,
        "arithmetic" = .build_arith_expr(input),
        "mathfun"    = .build_mathfun_expr(input),
        "indicator"  = .build_indicator_expr(input),
        "recode"     = .build_recode_expr(
          input, recode_df, design_final
        ),
        "cut"        = .build_cut_expr(input)
      )
    }

    # ── Preview panel ──────────────────────────────────────────────
    shiny::observeEvent(input$preview_var, {
      shiny::req(design())
      d <- dict()

      if (input$mode == "basic") {
        var_name <- trimws(input$var_name)
        expr_str <- tryCatch(.build_basic_expr(), error = function(e) NULL)
        if (is.null(expr_str) || nchar(var_name) == 0L) return()
        code_display <- paste0(var_name, " <- ", expr_str)
      } else {
        shiny::req(input$expert_code, nchar(trimws(input$expert_code)) > 0L)
        code_display <- trimws(input$expert_code)
      }

      output$preview_panel <- shiny::renderUI({
        shiny::tagList(
          shiny::hr(style = "margin:12px 0 8px;"),
          shiny::tags$small(
            i18n_t(dict(), "mod_variables.preview_title"),
            style = paste0(
              "color:#4A6572; font-weight:600;",
              " text-transform:uppercase; font-size:10px;"
            )
          ),
          shiny::tags$pre(
            code_display,
            style = paste(
              "background:#0f172a; color:#38bdf8; padding:10px 12px;",
              "border-radius:6px; font-size:11px;",
              " margin:6px 0 0; white-space:pre-wrap;"
            )
          )
        )
      })
    })

    # ── Add variable ───────────────────────────────────────────────
    shiny::observeEvent(input$add_var, {
      shiny::req(design())
      d <- dict()

      # ---- Build var_name and expr_str ----------------------------
      if (input$mode == "basic") {
        var_name <- trimws(input$var_name)

        if (nchar(var_name) == 0L) {
          shiny::showNotification(i18n_t(d, "mod_variables.err_no_name"),
                                  type = "error", duration = 5L)
          return()
        }
        if (!grepl("^[a-zA-Z.][a-zA-Z0-9._]*$", var_name)) {
          shiny::showNotification(i18n_t(d, "mod_variables.err_invalid_name"),
                                  type = "error", duration = 5L)
          return()
        }

        expr_str <- tryCatch(.build_basic_expr(), error = function(e) {
          shiny::showNotification(paste(i18n_t(d, "mod_variables.err_apply"),
                                        conditionMessage(e)),
                                  type = "error", duration = 8L)
          NULL
        })
        if (is.null(expr_str)) return()

        var_names_vec <- var_name
        display_str   <- paste0(var_name, " <- ", expr_str)

      } else {
        shiny::req(input$expert_code, nchar(trimws(input$expert_code)) > 0L)
        code_str      <- trimws(input$expert_code)
        var_names_vec <- .expert_var_names(code_str)

        if (length(var_names_vec) == 0L) {
          shiny::showNotification(i18n_t(d, "mod_variables.err_no_assignments"),
                                  type = "error", duration = 5L)
          return()
        }

        var_name <- paste(var_names_vec, collapse = ", ")
        expr_str <- code_str
        display_str <- code_str
      }

      # ---- Validate by trial mutation -----------------------------
      trial_ok <- tryCatch({
        des <- design_final()
        if (input$mode == "basic") {
          expr <- rlang::parse_expr(expr_str)
          dplyr::mutate(des, !!rlang::sym(var_names_vec[1L]) := !!expr)
        } else {
          .apply_expert_code(des, expr_str)
        }
        TRUE
      }, error = function(e) {
        shiny::showNotification(
          paste(i18n_t(d, "mod_variables.err_apply"), conditionMessage(e)),
          type = "error", duration = 8L
        )
        FALSE
      })
      if (!trial_ok) return()

      # ---- Store item --------------------------------------------
      n       <- var_counter() + 1L
      var_counter(n)
      item_id <- paste0("v_", n)

      vars_store$items[[item_id]] <- list(
        id        = item_id,
        var_name  = var_name,
        var_names = var_names_vec,
        mode      = input$mode,
        op_type   = if (input$mode == "basic") input$op_type else "expert",
        expr_str  = expr_str,
        display   = display_str
      )

      shiny::showNotification(
        i18n_t(d, "mod_variables.success_add"),
        type = "message", duration = 3L
      )

      # Reset name field for next variable
      if (input$mode == "basic") {
        shiny::updateTextInput(session, "var_name", value = "")
      }
    })

    # ── Register delete observers for new items ────────────────────
    shiny::observe({
      items   <- vars_store$items
      new_ids <- setdiff(names(items), registered_delete)

      for (item_id in new_ids) {
        local({
          lid <- item_id
          shiny::observeEvent(input[[paste0("del_", lid)]], {
            vars_store$items[[lid]] <- NULL
          }, ignoreInit = TRUE)
        })
        registered_delete <<- c(registered_delete, item_id)
      }
    })

    # ── Clear all derived variables ────────────────────────────────
    shiny::observeEvent(input$clear_vars, {
      vars_store$items    <- list()
      var_counter(0L)
      registered_delete <<- character(0L)
    })

    # -- Render variable list
    output$vars_table <- shiny::renderUI({
      items  <- vars_store$items
      d      <- dict()
      active <- design_vars_all()

      if (length(items) == 0L) {
        return(shiny::div(
          style = paste(
            "text-align:center; padding:40px 20px;",
            "color:#4A6572;"
          ),
          shiny::tags$i(
            class = "fa fa-magic",
            style = paste(
              "font-size:32px; color:#009FDB;",
              "display:block; margin-bottom:12px;"
            )
          ),
          shiny::tags$p(
            i18n_t(d, "mod_variables.no_vars"),
            style = "margin:0; font-size:13px;"
          )
        ))
      }

      cards <- lapply(rev(names(items)), function(item_id) {
        .var_card_ui(items[[item_id]], ns, d, active)
      })
      shiny::tagList(cards)
    })

    # ── Module return ──────────────────────────────────────────────
    return(list(
      design = design_final
    ))
  })
}
