#' Create a complex survey design object
#'
#' Constructs a \code{tbl_svy} design object from survey microdata by encoding
#' the sampling structure required for design-based inference: adjusted weights
#' \eqn{w_k}, stratification variable \eqn{h}, primary sampling units (PSUs)
#' \eqn{\alpha}, and an optional finite population correction (FPC).
#'
#' Let \eqn{U = \{1,\ldots,N\}} be the finite population and
#' \eqn{s \subset U} the sample. For each unit \eqn{k \in s}, the
#' \strong{first-order inclusion probability} is
#' \eqn{\pi_k = \Pr(k \in s) > 0} and the \strong{basic design weight} is
#' \eqn{d_k = 1/\pi_k}. The \strong{adjusted weight} \eqn{w_k} stored in
#' \code{weight} incorporates any non-response or calibration corrections
#' applied after data collection.
#'
#' @param data A tibble containing survey microdata with one row per
#'   sampled unit \eqn{k}.
#' @param weight Character string. Name of the column holding the adjusted
#'   sampling weights \eqn{w_k}. Must be strictly positive and free of
#'   \code{NA} values.
#' @param strata Optional character string. Name of the column identifying
#'   the stratum \eqn{h \in \{1,\ldots,H\}} to which each unit belongs.
#'   When supplied, variance is estimated within strata.
#' @param cluster Optional character string. Name of the column identifying
#'   the PSU \eqn{\alpha \in \{1,\ldots,\alpha_h\}} within stratum \eqn{h}.
#'   When \code{NULL}, a single-stage design is assumed (\eqn{\alpha_h = 1}
#'   for all \eqn{h}).
#' @param fpc Optional character string. Name of the column holding the
#'   finite population correction value for each unit, used to reduce
#'   variance estimates when the sampling fraction \eqn{n/N} is non-negligible.
#' @param nest Logical. Whether PSU labels are nested within strata, i.e.
#'   the same PSU code may appear in different strata. Default is \code{TRUE}.
#'   Set to \code{FALSE} only when PSU identifiers are globally unique.
#' @param check_psu Logical. If \code{TRUE} (default), emits a warning when
#'   PSUs appear in more than one stratum and \code{nest = FALSE}, which may
#'   indicate a labelling inconsistency.
#'
#' @return
#' A \code{tbl_svy} object (class from \pkg{srvyr}) compatible with
#' \pkg{srvyr} and \pkg{survey} functions. The object carries a
#' \code{"design_vars"} attribute that records \code{weight}, \code{strata},
#' \code{cluster}, \code{fpc} and \code{nest} for downstream diagnostics.
#'
#' @details
#' The function wraps \code{survey::svydesign()} and converts the result to
#' a \pkg{srvyr} object via \code{srvyr::as_survey()}.
#'
#' \strong{Supported design configurations}
#'
#' \tabular{ll}{
#'   \strong{Configuration} \tab \strong{Arguments supplied} \cr
#'   Simple random sampling (SRS) \tab \code{weight} only \cr
#'   Stratified \tab \code{weight} + \code{strata} \cr
#'   Single-stage cluster \tab \code{weight} + \code{cluster} \cr
#'   Stratified multistage \tab \code{weight} + \code{strata} + \code{cluster} \cr
#'   Any of the above with FPC \tab add \code{fpc} \cr
#' }
#'
#' \strong{Design weights and inclusion probabilities}
#'
#' For a stratified multistage design with \eqn{H} strata and
#' \eqn{\alpha_h} PSUs in stratum \eqn{h}, the Horvitz--Thompson (HT)
#' estimator of the population total is:
#'
#' \deqn{
#'   \hat{Y}_{HT} =
#'   \sum_{h=1}^{H}\sum_{\alpha=1}^{\alpha_h}\sum_{k=1}^{n_{h\alpha}}
#'   \omega_{h\alpha k}\, y_{h\alpha k},
#' }
#'
#' where \eqn{\omega_{h\alpha k}} are the adjusted weights of unit \eqn{k}
#' in PSU \eqn{\alpha} of stratum \eqn{h}. The resulting \code{tbl_svy}
#' object encodes this structure so that all subsequent calls to
#' \code{\link{estimate_survey}} use the correct design-based variance
#' estimator \eqn{\hat{V}_p(\hat{Y}_{HT})}.
#'
#' \strong{Lonely PSU handling}
#'
#' When a stratum \eqn{h} contains only one PSU (\eqn{\alpha_h = 1}),
#' the within-stratum variance cannot be estimated by Taylor linearization.
#' The function automatically sets
#' \code{options(survey.lonely.psu = "adjust")} so that variance is
#' approximated by centering the PSU total at the stratum mean, following
#' the conservative recommendation of Cochran (1977).
#'
#' \strong{Finite population correction}
#'
#' When the sampling fraction \eqn{f_h = n_h / N_h} is non-negligible
#' (typically \eqn{> 5\%}), supply \code{fpc} to reduce variance estimates
#' by the factor \eqn{(1 - f_h)}.
#'
#' @seealso
#' \code{\link{describe_survey_design}},
#' \code{\link{estimate_survey}},
#' \code{\link[survey]{svydesign}},
#' \code{\link[srvyr]{as_survey}}
#'
#' @references
#' Horvitz, D. G. & Thompson, D. J. (1952). A generalization of sampling
#' without replacement from a finite universe. \emph{Journal of the American
#' Statistical Association}, 47(260), 663--685.
#' \doi{10.1080/01621459.1952.10483446}
#'
#' Cochran, W. G. (1977). \emph{Sampling Techniques} (3rd ed.). Wiley.
#'
#' Sarndal, C.-E., Swensson, B. & Wretman, J. (1992).
#' \emph{Model Assisted Survey Sampling}. Springer.
#' \doi{10.1007/978-1-4612-4378-6}
#'
#' Lumley, T. (2010). \emph{Complex Surveys: A Guide to Analysis Using R}.
#' Wiley. \doi{10.1002/9780470580066}
#'
#' @examples
#' data <- generate_example_data(n_upm = 30)
#'
#' # Stratified multistage design: weight = w_k, strata = h, cluster = alpha
#' design <- as_survey_design_tbl(
#'   data,
#'   weight  = "weight",
#'   strata  = "strata",
#'   cluster = "upm"
#' )
#'
#' # Cluster design without stratification (H = 1)
#' design2 <- as_survey_design_tbl(
#'   data,
#'   weight  = "weight",
#'   cluster = "upm"
#' )
#'
#' # Weights-only design (SRS approximation, d_k = w_k)
#' design3 <- as_survey_design_tbl(
#'   data,
#'   weight = "weight"
#' )
#'
#' @export
#' 
as_survey_design_tbl <- function(
    data,
    weight,
    strata = NULL,
    cluster = NULL,
    fpc = NULL,
    nest = TRUE,
    check_psu = TRUE
) {
  
  # ---------------------------------------------------------
  # 1. Input validation
  # ---------------------------------------------------------
  if (!tibble::is_tibble(data)) {
    rlang::abort("`data` must be a tibble.")
  }
  
  if (!is.character(weight) || length(weight) != 1) {
    rlang::abort("`weight` must be a single character string.")
  }
  
  vars_required <- c(weight, strata, cluster, fpc)
  vars_required <- vars_required[!is.null(vars_required)]
  
  missing_vars <- setdiff(vars_required, names(data))
  if (length(missing_vars) > 0) {
    rlang::abort(
      message = paste(
        "Missing design variables:",
        paste(missing_vars, collapse = ", ")
      ),
      class = "shinycomplexsurvey_missing_design_vars"
    )
  }
  
  # ---------------------------------------------------------
  # 2. Weight checks
  # ---------------------------------------------------------
  if (any(is.na(data[[weight]]))) {
    rlang::abort("Weights contain missing values.")
  }
  
  if (any(data[[weight]] <= 0)) {
    rlang::abort("Weights must be strictly positive.")
  }
  
  # ---------------------------------------------------------
  # 3. PSU–strata consistency check (mejora agregada)
  # ---------------------------------------------------------
  if (check_psu && !is.null(cluster) && !is.null(strata)) {
    
    psu_check <- data |>
      dplyr::distinct(
        .data[[cluster]],
        .data[[strata]]
      ) |>
      dplyr::count(.data[[cluster]])
    
    if (any(psu_check$n > 1) && !nest) {
      warning(
        "Some PSUs appear in multiple strata. ",
        "Consider setting `nest = TRUE`."
      )
    }
  }
  
  # ---------------------------------------------------------
  # 4. Build formulas
  # ---------------------------------------------------------
  w  <- stats::as.formula(paste0("~", weight))
  id <- if (!is.null(cluster)) stats::as.formula(paste0("~", cluster)) else ~1
  st <- if (!is.null(strata))  stats::as.formula(paste0("~", strata))  else NULL
  fp <- if (!is.null(fpc))     stats::as.formula(paste0("~", fpc))     else NULL
  
  # ---------------------------------------------------------
  # 5. Build survey design
  # Lonely PSU option: when a stratum has a single PSU, variance
  # is approximated by centering at the stratum mean ("adjust").
  # ---------------------------------------------------------
  old_opt <- getOption("survey.lonely.psu")
  if (is.null(old_opt) || old_opt == "fail") {
    options(survey.lonely.psu = "adjust")
  }

  design <- survey::svydesign(
    ids     = id,
    strata  = st,
    weights = w,
    fpc     = fp,
    data    = data,
    nest    = nest
  )
  
  # ---------------------------------------------------------
  # 6. Convert to srvyr
  # ---------------------------------------------------------
  design_tbl <- srvyr::as_survey(design)
  
  # ---------------------------------------------------------
  # 7. Metadata
  # ---------------------------------------------------------
  attr(design_tbl, "design_vars") <- list(
    weight  = weight,
    strata  = strata,
    cluster = cluster,
    fpc     = fpc,
    nest    = nest
  )
  
  design_tbl
}


#' Describe a complex survey design
#'
#' Returns a one-row tibble of design diagnostics: sample size \eqn{n},
#' number of strata \eqn{H}, total number of PSUs \eqn{\sum_h \alpha_h},
#' and summary statistics of the adjusted weights \eqn{w_k} including their
#' coefficient of variation \eqn{CV(w) = s_w / \bar{w}}, which is an
#' indicator of weight variability and its effect on variance inflation.
#'
#' @param design A \code{tbl_svy} object created with
#'   \code{\link{as_survey_design_tbl}}.
#'
#' @return
#' A tibble with one row and the following columns:
#'
#' \describe{
#'   \item{\code{n_obs}}{Total sample size \eqn{n = |s|}.}
#'   \item{\code{n_strata}}{Number of strata \eqn{H}
#'     (\code{NA} if no stratification variable was supplied).}
#'   \item{\code{n_clusters}}{Total number of PSUs
#'     \eqn{\sum_{h=1}^{H} \alpha_h}
#'     (\code{NA} if no cluster variable was supplied).}
#'   \item{\code{weight_min}}{\eqn{\min_{k \in s} w_k}.}
#'   \item{\code{weight_max}}{\eqn{\max_{k \in s} w_k}.}
#'   \item{\code{weight_mean}}{Unweighted mean of the adjusted weights
#'     \eqn{\bar{w} = \hat{N}_w / n}, where
#'     \eqn{\hat{N}_w = \sum_{k \in s} w_k}.}
#'   \item{\code{weight_cv}}{Coefficient of variation of the weights
#'     \eqn{CV(w) = s_w / \bar{w}}. Large values (\eqn{> 0.5}) indicate
#'     high weight variability, which can substantially inflate the
#'     design effect \eqn{\widehat{\mathrm{DEFF}}}.}
#' }
#'
#' @seealso \code{\link{as_survey_design_tbl}}, \code{\link{estimate_survey}}
#'
#' @export
#'
#' @examples
#' data <- generate_example_data(30)
#' design <- as_survey_design_tbl(
#'   data,
#'   weight  = "weight",
#'   strata  = "strata",
#'   cluster = "upm"
#' )
#' describe_survey_design(design)
describe_survey_design <- function(design) {
  
  # ---------------------------------------------------------
  # 1. Validation
  # ---------------------------------------------------------
  if (!inherits(design, "tbl_svy")) {
    rlang::abort("`design` must be a srvyr tbl_svy object.")
  }
  
  # metadata del diseño
  design_vars <- attr(design, "design_vars")
  
  if (is.null(design_vars)) {
    rlang::abort(
      "Design metadata not found. Use `as_survey_design_tbl()`."
    )
  }
  
  data <- design$variables
  
  weight_var  <- design_vars$weight
  strata_var  <- design_vars$strata
  cluster_var <- design_vars$cluster
  
  # ---------------------------------------------------------
  # 2. Weights
  # ---------------------------------------------------------
  w <- data[[weight_var]]
  
  # ---------------------------------------------------------
  # 3. Dimensions
  # ---------------------------------------------------------
  n_obs <- nrow(data)
  
  n_strata <- if (!is.null(strata_var)) {
    dplyr::n_distinct(data[[strata_var]])
  } else {
    NA_integer_
  }
  
  n_clusters <- if (!is.null(cluster_var)) {
    dplyr::n_distinct(data[[cluster_var]])
  } else {
    NA_integer_
  }
  
  # ---------------------------------------------------------
  # 4. Weight diagnostics
  # ---------------------------------------------------------
  weight_min  <- min(w, na.rm = TRUE)
  weight_max  <- max(w, na.rm = TRUE)
  weight_mean <- mean(w, na.rm = TRUE)
  weight_cv   <- stats::sd(w, na.rm = TRUE) / weight_mean
  
  # ---------------------------------------------------------
  # 5. Output
  # ---------------------------------------------------------
  tibble::tibble(
    n_obs        = n_obs,
    n_strata     = n_strata,
    n_clusters   = n_clusters,
    weight_min   = weight_min,
    weight_max   = weight_max,
    weight_mean  = weight_mean,
    weight_cv    = weight_cv
  )
}