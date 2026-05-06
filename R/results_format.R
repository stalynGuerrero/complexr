#' Format survey estimation results
#'
#' Formats survey estimation outputs by ensuring required metrics
#' are present (CV, confidence intervals) and applying rounding.
#'
#' @param res A data.frame or tibble with estimation results.
#' @param digits Integer. Number of decimal places.
#'
#' @return A tibble with formatted results.
#'
#' @examples
#' res <- data.frame(estimate = c(0.45, 0.30), se = c(0.02, 0.015))
#' format_results_table(res)
#'
#' @export
format_results_table <- function(res, digits = 2) {
  
  if (!inherits(res, "data.frame")) {
    rlang::abort("`res` must be a data.frame or tibble.")
  }
  
  # ---------------------------------------------------------
  # 1. Required columns
  # ---------------------------------------------------------
  required_cols <- c("estimate", "se")
  
  missing_cols <- setdiff(required_cols, names(res))
  if (length(missing_cols) > 0) {
    rlang::abort(
      paste("Missing required columns:", paste(missing_cols, collapse = ", "))
    )
  }
  
  # ---------------------------------------------------------
  # 2. CV (robusto)
  # ---------------------------------------------------------
  if (!"cv" %in% names(res)) {
    res$cv <- ifelse(
      abs(res$estimate) < .Machine$double.eps,
      NA_real_,
      res$se / res$estimate
    )
  }
  
  res$cv[!is.finite(res$cv)] <- NA_real_
  
  # ---------------------------------------------------------
  # 3. Confidence intervals
  # ---------------------------------------------------------
  if (!all(c("lci", "uci") %in% names(res))) {
    
    conf_level <- if ("conf_level" %in% names(res)) {
      stats::na.omit(res$conf_level)[1]
    } else {
      0.95
    }
    
    z <- stats::qnorm(1 - (1 - conf_level)/2)
    
    res$lci <- res$estimate - z * res$se
    res$uci <- res$estimate + z * res$se
  }
  
  # ---------------------------------------------------------
  # 4. Rounding
  # ---------------------------------------------------------
  num_cols <- intersect(
    c("estimate", "se", "cv", "lci", "uci"),
    names(res)
  )
  
  res[num_cols] <- lapply(res[num_cols], function(x) {
    round(x, digits = digits)
  })
  
  # ---------------------------------------------------------
  # 5. Format confidence level
  # ---------------------------------------------------------
  if ("conf_level" %in% names(res)) {
    res$conf_level <- paste0(round(res$conf_level * 100), "%")
  }
  
  tibble::as_tibble(res)
}

#' Plot survey estimates as bar chart
#'
#' @param results Tibble returned by estimate_survey().
#'
#' @return A ggplot object.
#'
#' @examples
#' res <- data.frame(
#'   region   = c("North", "South", "East"),
#'   estimate = c(0.45, 0.30, 0.55),
#'   lci      = c(0.40, 0.25, 0.50),
#'   uci      = c(0.50, 0.35, 0.60),
#'   estimator = "prop"
#' )
#' plot_results_bar(res)
#'
#' @export
plot_results_bar <- function(results) {
  
  if (!inherits(results, "data.frame")) {
    rlang::abort("`results` must be a data.frame or tibble.")
  }
  
  required <- c("estimate", "lci", "uci")
  if (!all(required %in% names(results))) {
    rlang::abort("Results must include estimate, lci and uci.")
  }
  
  # ---------------------------------------------------------
  # Detect domains
  # ---------------------------------------------------------
  meta_cols <- c("variable", "estimator", "estimate", "se", "cv",
                 "deff", "lci", "uci", "quality", "conf_level", "quantile")
  domain_cols <- setdiff(names(results), meta_cols)
  
  if (length(domain_cols) == 0) {
    rlang::abort("No domain variables found to plot.")
  }
  
  x_var <- domain_cols[1]
  fill_var <- if (length(domain_cols) >= 2) domain_cols[2] else NULL
  
  # ---------------------------------------------------------
  # Base plot
  # ---------------------------------------------------------
  p <- ggplot2::ggplot(
    results,
    ggplot2::aes(
      x = .data[[x_var]],
      y = estimate,
      fill = if (!is.null(fill_var)) .data[[fill_var]] else NULL
    )
  ) +
    ggplot2::geom_col(
      position = ggplot2::position_dodge(width = 0.9),
      na.rm = TRUE
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = lci, ymax = uci),
      position = ggplot2::position_dodge(width = 0.9),
      width = 0.2,
      na.rm = TRUE
    ) +
    ggplot2::labs(
      x = x_var,
      y = "Estimate",
      fill = fill_var
    ) +
    ggplot2::theme_minimal()
  
  # ---------------------------------------------------------
  # Proportions scaling
  # ---------------------------------------------------------
  if ("estimator" %in% names(results)) {
    est_type <- unique(stats::na.omit(results$estimator))
    
    if (length(est_type) == 1 && est_type == "prop") {
      p <- p + ggplot2::scale_y_continuous(limits = c(0, 1))
    }
  }
  
  p
}
