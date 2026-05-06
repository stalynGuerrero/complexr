# ---------------------------------------------------------------------------
# Expression builders (package-private, called from mod_variables_server)
# ---------------------------------------------------------------------------

.build_arith_expr <- function(input) {
  shiny::req(input$arith_left, input$arith_op, input$arith_rtype)
  right <- if (input$arith_rtype == "var") {
    shiny::req(input$arith_rvar)
    input$arith_rvar
  } else {
    shiny::req(input$arith_rconst)
    as.character(input$arith_rconst)
  }
  op_r <- switch(
    input$arith_op,
    "\u2212" = "-", "\u00d7" = "*", "\u00f7" = "/",
    input$arith_op
  )
  paste0(input$arith_left, " ", op_r, " ", right)
}

.build_mathfun_expr <- function(input) {
  shiny::req(input$mfn_fn, input$mfn_var)
  if (input$mfn_fn == "round") {
    shiny::req(input$mfn_digits)
    return(sprintf(
      "round(%s, %d)", input$mfn_var, as.integer(input$mfn_digits)
    ))
  }
  sprintf("%s(%s)", input$mfn_fn, input$mfn_var)
}

.build_indicator_expr <- function(input) {
  shiny::req(input$ind_var, input$ind_op, input$ind_val)
  val <- trimws(input$ind_val)
  shiny::req(nchar(val) > 0L)
  val_r <- if (!is.na(suppressWarnings(as.numeric(val)))) val
           else sprintf('"%s"', val)
  sprintf(
    "ifelse(%s %s %s, 1L, 0L)", input$ind_var, input$ind_op, val_r
  )
}

.build_recode_expr <- function(input, recode_df_fn, design_final_fn) {
  shiny::req(input$recode_src)
  df <- recode_df_fn()
  shiny::req(nrow(df) > 0L)
  src_col <- design_final_fn()$variables[[input$recode_src]]
  is_num  <- is.numeric(src_col)
  pairs   <- vapply(seq_len(nrow(df)), function(i) {
    orig <- df$original[i]
    nuev <- df$nuevo[i]
    if (is_num) {
      sprintf('%s == %s ~ "%s"', input$recode_src, orig, nuev)
    } else {
      sprintf('%s == "%s" ~ "%s"', input$recode_src, orig, nuev)
    }
  }, character(1L))
  sprintf(
    'dplyr::case_when(%s, TRUE ~ NA_character_)',
    paste(pairs, collapse = ", ")
  )
}

.build_cut_expr <- function(input) {
  shiny::req(input$cut_var, input$cut_type)
  vv <- input$cut_var
  if (input$cut_type == "custom") {
    shiny::req(input$cut_breaks)
    brks <- trimws(strsplit(input$cut_breaks, ",")[[1L]])
    shiny::req(all(!is.na(suppressWarnings(as.numeric(brks)))))
    return(sprintf(
      "cut(%s, breaks = c(%s), include.lowest = TRUE)",
      vv, paste(brks, collapse = ", ")
    ))
  }
  n <- switch(
    input$cut_type,
    "terciles"  = 3L,
    "quartiles" = 4L,
    "quintiles" = 5L,
    "deciles"   = 10L,
    4L
  )
  sprintf(
    paste0(
      "cut(%s, breaks = quantile(%s,",
      " probs = seq(0, 1, by = 1/%d), na.rm = TRUE),",
      " include.lowest = TRUE)"
    ),
    vv, vv, n
  )
}

# ---------------------------------------------------------------------------
# Form UI builders (package-private)
# ---------------------------------------------------------------------------

.arith_form_ui <- function(ns, vars_all, d) {
  shiny::tagList(
    shiny::selectInput(
      ns("arith_left"),
      i18n_t(d, "mod_variables.arith_left"),
      choices = vars_all
    ),
    shiny::selectInput(
      ns("arith_op"),
      i18n_t(d, "mod_variables.arith_op"),
      choices = c(
        "+" = "+", "\u2212" = "-",
        "\u00d7" = "*", "\u00f7" = "/", "^" = "^"
      )
    ),
    shiny::radioButtons(
      ns("arith_rtype"),
      i18n_t(d, "mod_variables.arith_rtype"),
      choiceNames  = list(
        i18n_t(d, "mod_variables.arith_rvar"),
        i18n_t(d, "mod_variables.arith_rconst")
      ),
      choiceValues = list("var", "const"),
      selected     = "var",
      inline       = TRUE
    ),
    shiny::conditionalPanel(
      paste0("input['", ns("arith_rtype"), "'] == 'var'"),
      shiny::selectInput(ns("arith_rvar"), NULL, choices = vars_all)
    ),
    shiny::conditionalPanel(
      paste0("input['", ns("arith_rtype"), "'] == 'const'"),
      shiny::numericInput(ns("arith_rconst"), NULL, value = 1)
    )
  )
}

.mathfun_form_ui <- function(ns, vars_num, d) {
  shiny::tagList(
    shiny::selectInput(
      ns("mfn_fn"),
      i18n_t(d, "mod_variables.mathfun_fn"),
      choices = c(
        "log", "log10", "sqrt", "abs", "exp",
        "round", "ceiling", "floor"
      )
    ),
    shiny::selectInput(
      ns("mfn_var"),
      i18n_t(d, "mod_variables.mathfun_var"),
      choices = vars_num
    ),
    shiny::conditionalPanel(
      paste0("input['", ns("mfn_fn"), "'] == 'round'"),
      shiny::numericInput(
        ns("mfn_digits"),
        i18n_t(d, "mod_variables.mathfun_digits"),
        value = 2L, min = 0L, max = 15L
      )
    )
  )
}

.indicator_form_ui <- function(ns, vars_all, d) {
  shiny::tagList(
    shiny::selectInput(
      ns("ind_var"),
      i18n_t(d, "mod_variables.ind_var"),
      choices = vars_all
    ),
    shiny::selectInput(
      ns("ind_op"),
      i18n_t(d, "mod_variables.ind_op"),
      choices = c(
        "==" = "==", "!=" = "!=",
        ">"  = ">",  ">=" = ">=",
        "<"  = "<",  "<=" = "<="
      )
    ),
    shiny::textInput(
      ns("ind_val"),
      i18n_t(d, "mod_variables.ind_val"),
      placeholder = i18n_t(d, "mod_variables.ind_val_ph")
    )
  )
}

.recode_form_ui <- function(ns, vars_all, d) {
  shiny::tagList(
    shiny::selectInput(
      ns("recode_src"),
      i18n_t(d, "mod_variables.recode_src"),
      choices = vars_all
    ),
    shiny::tags$small(
      i18n_t(d, "mod_variables.recode_hint"),
      style = "color:#4A6572; display:block; margin:6px 0 4px;"
    ),
    DT::DTOutput(ns("recode_dt"), height = "220px")
  )
}

.cut_form_ui <- function(ns, vars_num, d) {
  shiny::tagList(
    shiny::selectInput(
      ns("cut_var"),
      i18n_t(d, "mod_variables.cut_var"),
      choices = vars_num
    ),
    shiny::selectInput(
      ns("cut_type"),
      i18n_t(d, "mod_variables.cut_type"),
      choices = stats::setNames(
        c("terciles", "quartiles", "quintiles", "deciles", "custom"),
        c(
          i18n_t(d, "mod_variables.cut_terciles"),
          i18n_t(d, "mod_variables.cut_quartiles"),
          i18n_t(d, "mod_variables.cut_quintiles"),
          i18n_t(d, "mod_variables.cut_deciles"),
          i18n_t(d, "mod_variables.cut_custom")
        )
      )
    ),
    shiny::conditionalPanel(
      paste0("input['", ns("cut_type"), "'] == 'custom'"),
      shiny::textInput(
        ns("cut_breaks"),
        i18n_t(d, "mod_variables.cut_breaks"),
        placeholder = "0, 100, 500, 1000"
      )
    )
  )
}

# ---------------------------------------------------------------------------
# Card UI builder
# ---------------------------------------------------------------------------

.var_card_ui <- function(item, ns, d, active) {
  ok <- all(item$var_names %in% active)

  status_icon <- if (ok) {
    shiny::tags$span(
      shiny::tags$i(class = "fa fa-check-circle"),
      style = "color:#22c55e; font-size:13px;"
    )
  } else {
    shiny::tags$span(
      shiny::tags$i(class = "fa fa-exclamation-circle"),
      style = "color:#f59e0b; font-size:13px;"
    )
  }

  badge_bg    <- if (item$mode == "basic") "#3b82f6" else "#8b5cf6"
  badge_label <- if (item$mode == "basic") {
    i18n_t(d, "mod_variables.mode_basic")
  } else {
    "R"
  }
  mode_badge <- shiny::tags$span(
    badge_label,
    style = paste0(
      "background:", badge_bg, ";",
      "color:white; padding:1px 7px; border-radius:10px;",
      "font-size:10px; font-weight:600; white-space:nowrap;"
    )
  )

  display_trunc <- item$display
  if (nchar(display_trunc) > 80L) {
    display_trunc <- paste0(substr(display_trunc, 1L, 77L), "...")
  }

  shiny::div(
    style = paste(
      "border:1px solid #e2e8f0; border-radius:8px;",
      "padding:10px 12px; margin-bottom:8px; background:#fafafa;"
    ),
    shiny::div(
      style = paste(
        "display:flex; align-items:center;",
        "justify-content:space-between;"
      ),
      shiny::div(
        style = paste(
          "display:flex; align-items:center;",
          "gap:8px; flex:1; min-width:0;"
        ),
        status_icon,
        shiny::tags$strong(item$var_name, style = "font-size:13px;"),
        mode_badge
      ),
      shiny::actionButton(
        ns(paste0("del_", item$id)),
        shiny::tags$i(class = "fa fa-times"),
        class = "btn-default btn-xs",
        style = "padding:2px 6px; line-height:1.2;"
      )
    ),
    shiny::tags$code(
      display_trunc,
      style = paste(
        "display:block; margin-top:6px; font-size:10px;",
        "color:#475569; word-break:break-all;"
      )
    )
  )
}
