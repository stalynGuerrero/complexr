#' Estimate survey statistics under complex sampling designs
#'
#' Computes design-based point estimates and their associated uncertainty
#' measures for complex survey data. The function operates on objects of class
#' \code{tbl_svy} and implements five estimators grounded in the
#' Horvitz--Thompson (HT) framework: weighted mean \eqn{\bar{y}_w}, population
#' total \eqn{\hat{Y}_{HT}}, proportion \eqn{\hat{p}_k}, ratio \eqn{\hat{R}},
#' and quantile \eqn{\hat{q}_p}.
#'
#' Let \eqn{U = \{1,\ldots,N\}} be the finite population and \eqn{s \subset U}
#' the selected sample. For each unit \eqn{k \in s}, \eqn{d_k = 1/\pi_k} is
#' the \strong{basic design weight} (inverse of the first-order inclusion
#' probability \eqn{\pi_k}), and \eqn{w_k} is the \strong{adjusted weight}
#' after non-response or calibration corrections.
#'
#' All estimators account for stratification, clustering and unequal weights.
#' Variance is estimated by Taylor linearization via the \pkg{survey} and
#' \pkg{srvyr} packages, and the design effect
#' \eqn{\widehat{\mathrm{DEFF}} = \hat{V}_p(\hat{\theta}) /
#' \hat{V}_{\mathrm{SRS}}(\hat{\theta})} is reported for mean, total and
#' proportion estimators. Domain estimation (argument \code{by}) is supported
#' for all estimators; variance is computed over the full sample \eqn{s} to
#' avoid subsetting bias.
#'
#' @param design A \code{tbl_svy} object created with
#'   \code{as_survey_design_tbl()}, encoding weights \eqn{w_k}, strata
#'   \eqn{h} and primary sampling units \eqn{\alpha}.
#'
#' @param variable Character string with the name of the target variable
#'   \eqn{y_k}. Required for \code{"mean"}, \code{"total"}, \code{"prop"} and
#'   \code{"quantile"}. Ignored when \code{estimator = "ratio"}.
#'
#' @param estimator Character string specifying the estimator. One of
#'   \code{"mean"}, \code{"total"}, \code{"prop"}, \code{"ratio"}, or
#'   \code{"quantile"}.
#'
#' @param by Optional character vector naming domain variables that define
#'   subpopulations \eqn{U_d \subset U}.
#'
#' @param na_rm Logical. Whether to remove \code{NA} values before estimation.
#'   Default is \code{TRUE}.
#'
#' @param conf_level Numeric in \eqn{(0,1)}. Confidence level for interval
#'   estimation. Default is \code{0.95}.
#'
#' @param numerator Character string with the name of the numerator variable
#'   \eqn{y_k} in ratio estimation.
#'
#' @param denominator Character string with the name of the denominator variable
#'   \eqn{x_k} in ratio estimation.
#'
#' @param ratio_num_level Character. Category of \code{numerator} used as
#'   \eqn{\hat{N}_{\text{num}}} when the numerator variable is categorical.
#'
#' @param ratio_den_level Character. Category of \code{denominator} used as
#'   \eqn{\hat{N}_{\text{den}}} when the denominator variable is categorical.
#'
#' @param probs Numeric vector of probabilities in \eqn{(0,1)} for quantile
#'   estimation.
#'
#' @details
#'
#' \strong{Mean}
#'
#' The weighted mean is the HT ratio estimator:
#'
#' \deqn{
#'   \bar{y}_w = \frac{\hat{Y}_w}{\hat{N}_w}
#'             = \frac{\displaystyle\sum_{k \in s} w_k\, y_k}
#'                    {\displaystyle\sum_{k \in s} w_k}
#' }
#'
#' where \eqn{\hat{N}_w = \sum_{k \in s} w_k} is the estimated population
#' size. Its variance is estimated by Taylor linearization (Sarndal et al.,
#' 1992):
#'
#' \deqn{
#'   \hat{V}_p(\bar{y}_w) =
#'   \frac{1}{\hat{N}_w^2}\,\hat{V}_p(\hat{Y}_w).
#' }
#'
#' \strong{Total}
#'
#' The HT estimator of the population total is (Horvitz & Thompson, 1952):
#'
#' \deqn{
#'   \hat{Y}_{HT} = \sum_{k \in s} d_k\, y_k.
#' }
#'
#' With adjusted weights and a stratified multistage design
#' (\eqn{H} strata, \eqn{\alpha_h} PSUs per stratum \eqn{h},
#' \eqn{n_{h\alpha}} observations per PSU):
#'
#' \deqn{
#'   \hat{Y}_w =
#'   \sum_{h=1}^{H}\sum_{\alpha=1}^{\alpha_h}\sum_{k=1}^{n_{h\alpha}}
#'   \omega_{h\alpha k}\, y_{h\alpha k},
#'   \qquad
#'   \hat{V}_p(\hat{Y}_w) = \sum_{h=1}^{H} \hat{V}_{p,h}(\hat{Y}_{w,h}).
#' }
#'
#' \strong{Proportion}
#'
#' For a binary variable \eqn{y_k \in \{0,1\}}, the estimated proportion is:
#'
#' \deqn{
#'   \hat{p} = \frac{\hat{N}_1}{\hat{N}_w}
#'           = \frac{\displaystyle\sum_{h}\sum_{\alpha}\sum_{k}
#'                   \omega_{h\alpha k}\,I(y_k = 1)}
#'                  {\displaystyle\sum_{h}\sum_{\alpha}\sum_{k}
#'                   \omega_{h\alpha k}}.
#' }
#'
#' For a categorical variable with categories \eqn{\mathcal{K}}, the
#' proportion of category \eqn{c} is \eqn{\hat{p}_c = \hat{N}_c / \hat{N}_w},
#' where \eqn{\hat{N}_c} is the weighted count of units with \eqn{y_k = c}.
#' Variance is approximated by Taylor linearization (Heeringa et al., 2017):
#'
#' \deqn{
#'   \hat{V}_p(\hat{p}) \approx
#'   \frac{\hat{V}_p(\hat{N}_1) + \hat{p}^2\,\hat{V}_p(\hat{N}_w)
#'         - 2\hat{p}\,\widehat{\mathrm{cov}}(\hat{N}_1, \hat{N}_w)}
#'        {\hat{N}_w^2}.
#' }
#'
#' \strong{Ratio}
#'
#' The ratio estimator of two population totals is (Cochran, 1977):
#'
#' \deqn{
#'   \hat{R} = \frac{\hat{Y}_w}{\hat{X}_w}
#'           = \frac{\sum_{k \in s} w_k\, y_k}{\sum_{k \in s} w_k\, x_k}.
#' }
#'
#' Variance is estimated by first-order Taylor linearization:
#'
#' \deqn{
#'   \hat{V}_p(\hat{R}) \approx
#'   \frac{1}{\hat{X}_w^2}\,
#'   \hat{V}_p\!\left(\hat{Y}_w - \hat{R}\,\hat{X}_w\right).
#' }
#'
#' When variables are categorical, indicator variables are constructed for
#' the specified levels before calling \code{survey::svyratio()}.
#' The \code{deff} column is \code{NA} for ratio estimates.
#'
#' \strong{Quantile}
#'
#' Quantiles are derived from the weighted empirical CDF (Woodruff, 1952):
#'
#' \deqn{
#'   \hat{F}_w(t) =
#'   \frac{\sum_{k \in s} w_k\,I(y_k \leq t)}{\sum_{k \in s} w_k},
#'   \qquad
#'   \hat{q}_p = \inf\{t : \hat{F}_w(t) \geq p\}.
#' }
#'
#' Confidence intervals use the Woodruff linearization method, which maps
#' the problem to the cumulative proportion scale:
#'
#' \deqn{
#'   IC_p[\hat{q}_p] =
#'   \left\{t : \hat{F}_w(t) \in
#'   \left[p \pm t_{1-\alpha/2,\,gl}\;
#'   ee(\hat{F}_w(t))\right]\right\}.
#' }
#'
#' The \code{deff} column is \code{NA} for quantile estimates.
#'
#' \strong{Design effect}
#'
#' For estimators that support it, the output column \code{deff} contains
#' (Kish, 1965):
#'
#' \deqn{
#'   \widehat{\mathrm{DEFF}} =
#'   \frac{\hat{V}_p(\hat{\theta})}{\hat{V}_{\mathrm{SRS}}(\hat{\theta})}.
#' }
#'
#' \strong{Domain estimation}
#'
#' When \code{by} is supplied, estimation is performed within each
#' subpopulation \eqn{U_d} defined by the combination of domain variable
#' levels. The domain mean estimator is:
#'
#' \deqn{
#'   \bar{y}_{w,d} =
#'   \frac{\sum_{k \in s} w_k\,y_k\,I(k \in U_d)}
#'        {\sum_{k \in s} w_k\,I(k \in U_d)}
#'   = \frac{\hat{Y}_{w,d}}{\hat{N}_{w,d}}.
#' }
#'
#' Variance is computed over the full sample \eqn{s}, preserving the design
#' structure and avoiding subsetting bias (Lumley, 2010).
#'
#' \strong{Confidence intervals}
#'
#' Intervals are constructed under asymptotic normality as:
#'
#' \deqn{
#'   \hat{\theta} \pm z_{1-\alpha/2}\;ee(\hat{\theta}),
#'   \qquad
#'   ee(\hat{\theta}) = \sqrt{\hat{V}_p(\hat{\theta})}.
#' }
#'
#' @return
#' A tibble with the following columns, plus one column per domain variable
#' (if \code{by} is supplied):
#'
#' \describe{
#'   \item{\code{variable}}{Name of the target variable \eqn{y_k}.}
#'   \item{\code{estimator}}{Type of estimator (\code{"mean"}, \code{"total"},
#'     etc.).}
#'   \item{\code{estimate}}{Point estimate \eqn{\hat{\theta}}.}
#'   \item{\code{se}}{Standard error
#'     \eqn{ee(\hat{\theta}) = \sqrt{\hat{V}_p(\hat{\theta})}}.}
#'   \item{\code{cv}}{Coefficient of variation
#'     \eqn{CV = ee(\hat{\theta}) / \hat{\theta}}.}
#'   \item{\code{deff}}{Design effect
#'     \eqn{\widehat{\mathrm{DEFF}}} (mean, total, proportion only;
#'     \code{NA} for ratio and quantile).}
#'   \item{\code{lci}}{Lower confidence bound.}
#'   \item{\code{uci}}{Upper confidence bound.}
#'   \item{\code{quality}}{Precision label derived from the \eqn{CV}:
#'     \code{"Very high precision"} (\eqn{CV < 5\%}),
#'     \code{"High precision"} (\eqn{5\%}--\eqn{10\%}),
#'     \code{"Acceptable precision"} (\eqn{10\%}--\eqn{20\%}),
#'     \code{"Use with caution"} (\eqn{20\%}--\eqn{30\%}), or
#'     \code{"Low precision"} (\eqn{\geq 30\%}).}
#' }
#'
#' @seealso
#' \code{\link[srvyr]{survey_mean}},
#' \code{\link[srvyr]{survey_total}},
#' \code{\link[survey]{svyratio}},
#' \code{\link[survey]{svyquantile}},
#' \code{\link{as_survey_design_tbl}}
#'
#' @examples
#' # ---------------------------------------------------------
#' # Generate example data and build a stratified multistage
#' # design:  weight = w_k,  strata = h,  cluster = alpha
#' # ---------------------------------------------------------
#' data <- generate_example_data(n_upm = 30, seed = 123)
#'
#' design <- srvyr::as_survey_design(
#'   data,
#'   ids     = upm,
#'   strata  = strata,
#'   weights = weight
#' )
#'
#' # ---------------------------------------------------------
#' # Mean  --  bar{y}_w  with DEFF and 95 % CI
#' # ---------------------------------------------------------
#' estimate_survey(
#'   design,
#'   variable  = "ingreso_pc",
#'   estimator = "mean"
#' )
#'
#' # Domain mean  bar{y}_{w,d}  by region
#' estimate_survey(
#'   design,
#'   variable  = "ingreso_pc",
#'   estimator = "mean",
#'   by        = "region"
#' )
#'
#' # ---------------------------------------------------------
#' # Total  --  hat{Y}_{HT}  with DEFF
#' # ---------------------------------------------------------
#' estimate_survey(
#'   design,
#'   variable  = "ingreso_pc",
#'   estimator = "total"
#' )
#'
#' # ---------------------------------------------------------
#' # Proportion  --  hat{p} = hat{N}_1 / hat{N}_w
#' # ---------------------------------------------------------
#' estimate_survey(
#'   design,
#'   variable  = "pobre",
#'   estimator = "prop"
#' )
#'
#' # Multinomial proportion  hat{p}_c = hat{N}_c / hat{N}_w
#' estimate_survey(
#'   design,
#'   variable  = "empleo",
#'   estimator = "prop"
#' )
#'
#' # ---------------------------------------------------------
#' # Ratio  --  hat{R} = hat{Y}_w / hat{X}_w
#' # ---------------------------------------------------------
#' estimate_survey(
#'   design,
#'   estimator   = "ratio",
#'   numerator   = "ingreso_pc",
#'   denominator = "gasto_pc"
#' )
#'
#' # Ratio with domains
#' estimate_survey(
#'   design,
#'   estimator   = "ratio",
#'   numerator   = "ingreso_pc",
#'   denominator = "gasto_pc",
#'   by          = "region"
#' )
#'
#' # Categorical / Categorical:  hat{N}_{Formal} / hat{N}_{Informal}
#' estimate_survey(
#'   design,
#'   estimator       = "ratio",
#'   numerator       = "empleo",
#'   denominator     = "empleo",
#'   ratio_num_level = "Formal",
#'   ratio_den_level = "Informal"
#' )
#'
#' # ---------------------------------------------------------
#' # Quantile  --  hat{q}_p via Woodruff linearization
#' # ---------------------------------------------------------
#' estimate_survey(
#'   design,
#'   variable  = "ingreso_pc",
#'   estimator = "quantile",
#'   probs     = c(0.25, 0.5, 0.75)
#' )
#'
#' @references
#' Horvitz, D. G. & Thompson, D. J. (1952). A generalization of sampling
#' without replacement from a finite universe. \emph{Journal of the American
#' Statistical Association}, 47(260), 663--685.
#' \doi{10.1080/01621459.1952.10483446}
#'
#' Cochran, W. G. (1977). \emph{Sampling Techniques} (3rd ed.). Wiley.
#'
#' Woodruff, R. S. (1952). Confidence intervals for medians and other
#' position measures. \emph{Journal of the American Statistical Association},
#' 47(260), 635--646. \doi{10.1080/01621459.1952.10483443}
#'
#' Kish, L. (1965). \emph{Survey Sampling}. Wiley.
#'
#' Sarndal, C.-E., Swensson, B. & Wretman, J. (1992).
#' \emph{Model Assisted Survey Sampling}. Springer.
#' \doi{10.1007/978-1-4612-4378-6}
#'
#' Lumley, T. (2010). \emph{Complex Surveys: A Guide to Analysis Using R}.
#' Wiley. \doi{10.1002/9780470580066}
#'
#' Heeringa, S. G., West, B. T. & Berglund, P. A. (2017).
#' \emph{Applied Survey Data Analysis} (2nd ed.). CRC Press.
#' \doi{10.1201/9781315153278}
#'
#' @export



estimate_survey <- function(
    design,
    variable = NULL,
    estimator = c("mean", "total", "prop", "ratio", "quantile"),
    by = NULL,
    na_rm = TRUE,
    conf_level = 0.95,
    numerator = NULL,
    denominator = NULL,
    ratio_num_level = NULL,
    ratio_den_level = NULL,
    probs = c(0.25, 0.5, 0.75)
) {
  
  estimator <- match.arg(estimator)
  
  if (!inherits(design, "tbl_svy")) {
    rlang::abort("`design` must be a tbl_svy object.")
  }
  
  vars <- names(design$variables)
  
  if (!is.null(by)) {
    if (!is.character(by)) {
      rlang::abort("`by` must be a character vector.")
    }
    missing_by <- setdiff(by, vars)
    if (length(missing_by) > 0) {
      rlang::abort(
        paste("Grouping variables not found:", paste(missing_by, collapse = ", "))
      )
    }
  }
  
  alpha <- 1 - conf_level
  z <- stats::qnorm(1 - alpha / 2)
  
  # ----------------------------------------------------------
  # Helpers
  # ----------------------------------------------------------
  
  domain_keys <- function(df, by) {
    if (is.null(by) || length(by) == 0) return(NULL)
    dplyr::distinct(df, dplyr::across(dplyr::all_of(by)))
  }
  
  subset_design_by_row <- function(svy, by, row) {
    if (is.null(by) || length(by) == 0) return(svy)
    
    conds <- lapply(by, function(v) {
      call("==", as.name(v), row[[v]][[1]])
    })
    cond <- Reduce(function(a, b) call("&", a, b), conds)
    
    subset(svy, eval(cond, svy$variables, parent.frame()))
  }
  
  # ==========================================================
  # MEAN / TOTAL / PROP
  # ==========================================================
  
  if (estimator %in% c("mean", "total", "prop")) {
    
    if (!is.character(variable) || length(variable) != 1 || !variable %in% vars) {
      rlang::abort("`variable` must be a single valid variable name.")
    }
    
    var_sym  <- rlang::sym(variable)
    var_data <- design$variables[[variable]]
    
    design_grp <- design
    if (!is.null(by) && length(by) > 0) {
      design_grp <- design_grp %>%
        srvyr::group_by(dplyr::across(dplyr::all_of(by)))
    }
    
    if (estimator == "mean") {
      
      if (!is.numeric(var_data)) {
        rlang::abort("Mean requires numeric variable.")
      }
      
      res <- design_grp %>%
        srvyr::summarise(
          estimate = srvyr::survey_mean(
            !!var_sym, vartype = c("se", "cv"), na.rm = na_rm, deff = TRUE
          )
        )
      
    } else if (estimator == "total") {
      
      if (!is.numeric(var_data)) {
        rlang::abort("Total requires numeric variable.")
      }
      
      res <- design_grp %>%
        srvyr::summarise(
          estimate = srvyr::survey_total(
            !!var_sym, vartype = c("se", "cv"), na.rm = na_rm, deff = TRUE
          )
        )
      
    } else {

      uniq <- unique(stats::na.omit(var_data))
      is_binary <- is.logical(var_data) ||
        (is.numeric(var_data) && all(uniq %in% c(0, 1)))

      if (is_binary) {

        res <- design_grp %>%
          srvyr::summarise(
            estimate = srvyr::survey_mean(
              !!var_sym, vartype = c("se", "cv"), na.rm = na_rm, deff = TRUE
            )
          )

      } else {

        levs <- levels(as.factor(var_data))

        res <- purrr::map_dfr(levs, function(lv) {

          tmp <- design_grp %>%
            dplyr::mutate(.ind = (!!var_sym) == lv) %>%
            srvyr::summarise(
              estimate = srvyr::survey_mean(
                .ind, vartype = c("se", "cv"), na.rm = na_rm, deff = TRUE
              )
            )

          tmp[[variable]] <- lv
          tmp
        })
      }
    }
    
    res <- res %>%
      dplyr::rename(se = estimate_se, cv = estimate_cv, 
                    deff = estimate_deff) %>%
      dplyr::mutate(
        lci = estimate - z * se,
        uci = estimate + z * se,
        estimator = estimator,
        variable = variable
      )
    
    # ==========================================================
    # RATIO
    # ==========================================================
    
  } else if (estimator == "ratio") {
    
    if (is.null(numerator) || is.null(denominator)) {
      rlang::abort("Both numerator and denominator must be provided.")
    }
    
    if (!numerator %in% vars || !denominator %in% vars) {
      rlang::abort("Numerator or denominator not found in data.")
    }
    
    num_data <- design$variables[[numerator]]
    den_data <- design$variables[[denominator]]
    
    num_is_num <- is.numeric(num_data)
    den_is_num <- is.numeric(den_data)
    num_is_cat <- is.factor(num_data) || is.character(num_data)
    den_is_cat <- is.factor(den_data) || is.character(den_data)
    
    if (num_is_num && den_is_num) {
      
      if (numerator == denominator) {
        rlang::abort("Continuous ratios require different variables.")
      }
      
      compute_ratio <- function(svy) {
        
        r <- survey::svyratio(
          stats::as.formula(paste0("~", numerator)),
          stats::as.formula(paste0("~", denominator)),
          design = svy,
          na.rm  = na_rm
        )
        
        est <- as.numeric(coef(r))
        se  <- as.numeric(survey::SE(r))
        
        tibble::tibble(
          estimate = est,
          se       = se,
          cv       = ifelse(abs(est) < .Machine$double.eps, NA_real_, se / est),
          deff = NA_real_
        )
      }
      
    } else {
      
      if (num_is_cat && is.null(ratio_num_level)) {
        rlang::abort("Category for numerator must be specified.")
      }
      if (den_is_cat && is.null(ratio_den_level)) {
        rlang::abort("Category for denominator must be specified.")
      }
      
      compute_ratio <- function(svy) {
        
        svy_tmp <-stats::update(
          svy,
          .num = if (num_is_cat) {
            as.numeric(get(numerator) == ratio_num_level)
          } else {
            get(numerator)
          },
          .den = if (den_is_cat) {
            as.numeric(get(denominator) == ratio_den_level)
          } else {
            get(denominator)
          }
        )
      
        r <- survey::svyratio(~.num, ~.den, design = svy_tmp, na.rm = na_rm)
        
        est <- as.numeric(coef(r))
        se  <- as.numeric(survey::SE(r))
        
        tibble::tibble(
          estimate = est,
          se       = se,
          cv       = ifelse(abs(est) < .Machine$double.eps, NA_real_, se / est),
          deff = NA_real_
        )
      }
    }
    
    keys <- domain_keys(design$variables, by)
    
    if (is.null(keys)) {
      res <- compute_ratio(design)
    } else {
      res <- purrr::map_dfr(seq_len(nrow(keys)), function(i) {
        row <- keys[i, , drop = FALSE]
        dplyr::bind_cols(
          row,
          compute_ratio(subset_design_by_row(design, by, row))
        )
      })
    }
    
    res <- res %>%
      dplyr::mutate(
        lci = estimate - z * se,
        uci = estimate + z * se,
        estimator = "ratio",
        variable = paste0(numerator, "_over_", denominator)
      )
    
    # ==========================================================
    # QUANTILE
    # ==========================================================
    
  } else {
    
    if (!is.character(variable) || length(variable) != 1 || !variable %in% vars) {
      rlang::abort("`variable` must be valid for quantiles.")
    }
    
    probs <- sort(unique(probs))
    if (any(probs <= 0 | probs >= 1)) {
      rlang::abort("`probs` must be strictly between 0 and 1.")
    }
    
    keys <- domain_keys(design$variables, by)
    
    compute_quantile <- function(svy) {
      q <- survey::svyquantile(
        stats::as.formula(paste0("~", variable)),
        design = svy,
        quantiles = probs,
        na.rm = na_rm,
        ci = TRUE
      )
      qdf <- as.data.frame(q[[1]])
      
      tibble::tibble(
        quantile = probs,
        estimate = qdf[, "quantile"],
        se       = qdf[, "se"],
        cv       = ifelse(
          abs(qdf[, "quantile"]) < .Machine$double.eps,
          NA_real_,
          qdf[, "se"] / qdf[, "quantile"]
        ),
        deff = NA_real_
      )
    }
    
    if (is.null(keys)) {
      res <- compute_quantile(design)
    } else {
      res <- purrr::map_dfr(seq_len(nrow(keys)), function(i) {
        row <- keys[i, , drop = FALSE]
        dplyr::bind_cols(
          row,
          compute_quantile(subset_design_by_row(design, by, row))
        )
      })
    }
    
    res <- res %>%
      dplyr::mutate(
        lci = estimate - z * se,
        uci = estimate + z * se,
        estimator = "quantile",
        variable = variable
      )
  }
  
  dplyr::relocate(res, variable, estimator) %>%
    dplyr::mutate(
      quality = dplyr::case_when(
        is.na(cv)  ~ NA_character_,
        cv < 0.05  ~ "Very high precision",
        cv < 0.10  ~ "High precision",
        cv < 0.20  ~ "Acceptable precision",
        cv < 0.30  ~ "Use with caution",
        TRUE       ~ "Low precision"
      )
    )
}



