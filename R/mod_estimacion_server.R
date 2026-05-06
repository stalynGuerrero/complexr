# ---------------------------------------------------------------------------
# Theory HTML helpers (language-aware)
# ---------------------------------------------------------------------------

.th_mean <- function(lang) {
  if (lang == "en") {
"<b>Population mean under complex design</b><br><br>

The population mean estimator is obtained as:

$$
\\hat{\\bar{Y}} =
\\frac{\\sum_{i \\in s} w_i y_i}
     {\\sum_{i \\in s} w_i}
$$

where:

<ul>
<li>\\(w_i = 1/\\pi_i\\) is the expansion weight</li>
<li>\\(\\pi_i\\) is the inclusion probability</li>
<li>\\(y_i\\) is the variable of interest</li>
</ul>

This estimator can be interpreted as the Horvitz-Thompson total estimator divided by the estimated population size.

$$
\\hat{\\bar{Y}} = \\frac{\\hat{T}_Y}{\\hat{N}}
$$

where

$$
\\hat{T}_Y = \\sum w_i y_i
\\quad
\\hat{N} = \\sum w_i
$$

<br>

<b>Approximate variance</b>

$$
\\widehat{Var}(\\hat{\\bar{Y}}) =
\\frac{1}{(\\sum w_i)^2}
\\widehat{Var}\\left(\\sum w_i y_i\\right)
$$

Variance is estimated via Taylor linearization or replication methods.
"
  } else {
"<b>Media poblacional bajo dise\u00f1o complejo</b><br><br>

El estimador de la media poblacional se obtiene como:

$$
\\hat{\\bar{Y}} =
\\frac{\\sum_{i \\in s} w_i y_i}
     {\\sum_{i \\in s} w_i}
$$

donde:

<ul>
<li>\\(w_i = 1/\\pi_i\\) es el peso de expansi\u00f3n</li>
<li>\\(\\pi_i\\) es la probabilidad de inclusi\u00f3n</li>
<li>\\(y_i\\) es la variable de inter\u00e9s</li>
</ul>

Este estimador puede interpretarse como el estimador de Horvitz-Thompson del total dividido por el tama\u00f1o poblacional estimado.

$$
\\hat{\\bar{Y}} = \\frac{\\hat{T}_Y}{\\hat{N}}
$$

donde

$$
\\hat{T}_Y = \\sum w_i y_i
\\quad
\\hat{N} = \\sum w_i
$$

<br>

<b>Varianza aproximada</b>

$$
\\widehat{Var}(\\hat{\\bar{Y}}) =
\\frac{1}{(\\sum w_i)^2}
\\widehat{Var}\\left(\\sum w_i y_i\\right)
$$

La varianza se estima mediante linearizaci\u00f3n de Taylor o m\u00e9todos de replicaci\u00f3n.
"
  }
}

.th_total <- function(lang) {
  if (lang == "en") {
"<b>Population total</b><br><br>

The population total estimator corresponds to the Horvitz-Thompson estimator:

$$
\\hat{T}_{HT} =
\\sum_{i \\in s} w_i y_i
$$

where:

<ul>
<li>\\(w_i = 1/\\pi_i\\) is the expansion weight</li>
<li>\\(\\pi_i\\) is the inclusion probability</li>
</ul>

Under stratified sampling:

$$
\\hat{T} =
\\sum_{h=1}^{H}
\\sum_{i \\in s_h}
w_{hi} y_{hi}
$$

This estimator is unbiased under the sampling design.
"
  } else {
"<b>Total poblacional</b><br><br>

El estimador del total poblacional corresponde al estimador de Horvitz-Thompson:

$$
\\hat{T}_{HT} =
\\sum_{i \\in s} w_i y_i
$$

donde:

<ul>
<li>\\(w_i = 1/\\pi_i\\) es el peso de expansi\u00f3n</li>
<li>\\(\\pi_i\\) es la probabilidad de inclusi\u00f3n</li>
</ul>

Bajo muestreo estratificado:

$$
\\hat{T} =
\\sum_{h=1}^{H}
\\sum_{i \\in s_h}
w_{hi} y_{hi}
$$

Este estimador es insesgado bajo el dise\u00f1o de muestreo.
"
  }
}

.th_prop <- function(lang) {
  if (lang == "en") {
"<b>Population proportion</b><br><br>

Defining an indicator variable:

$$
I_i =
\\begin{cases}
1 & \\text{if the unit belongs to the category of interest} \\\\
0 & \\text{otherwise}
\\end{cases}
$$

The population proportion is estimated as:

$$
\\hat{P} =
\\frac{\\sum_{i \\in s} w_i I_i}
     {\\sum_{i \\in s} w_i}
$$

This estimator is equivalent to the weighted mean of the indicator variable.

Variance is approximated via Taylor linearization.
"
  } else {
"<b>Proporci\u00f3n poblacional</b><br><br>

Definiendo una variable indicadora:

$$
I_i =
\\begin{cases}
1 & \\text{si la unidad pertenece a la categor\u00eda de inter\u00e9s} \\\\
0 & \\text{en otro caso}
\\end{cases}
$$

La proporci\u00f3n poblacional se estima como:

$$
\\hat{P} =
\\frac{\\sum_{i \\in s} w_i I_i}
     {\\sum_{i \\in s} w_i}
$$

Este estimador es equivalente a la media ponderada de la variable indicadora.

La varianza se aproxima mediante linearizaci\u00f3n de Taylor.
"
  }
}

.th_ratio <- function(lang) {
  if (lang == "en") {
"<b>Ratio estimator</b><br><br>

The ratio estimator compares two estimated population totals
from a sample with expansion weights.

$$
\\hat{R} =
\\frac{\\sum_{i \\in s} w_i y_i}
     {\\sum_{i \\in s} w_i x_i}
$$

where:

<ul>
<li>\\(w_i\\) is the expansion weight</li>
<li>\\(y_i\\) is the numerator variable</li>
<li>\\(x_i\\) is the denominator variable</li>
<li>\\(s\\) represents the set of sample units</li>
</ul>

<br>

<b>Relationship with Horvitz-Thompson estimators</b>

$$
\\hat{R} = \\frac{\\hat{Y}}{\\hat{X}}
$$

with

$$
\\hat{Y} = \\sum_{i \\in s} w_i y_i
\\qquad
\\hat{X} = \\sum_{i \\in s} w_i x_i
$$

<br>

<b>Approximate variance</b>

The estimator is nonlinear, so its variance is approximated via Taylor linearization:

$$
\\widehat{Var}(\\hat{R}) \\approx
\\frac{1}{\\hat{X}^2}
\\widehat{Var}(\\hat{Y} - \\hat{R}\\hat{X})
$$

<br>

<b>Particular cases used in the application</b>

<br>

<b>1. Ratio between categories</b><br>
Example: proportion of men relative to women.

Let

$$
y_i = I(\\text{man})
\\qquad
x_i = I(\\text{woman})
$$

The estimator is:

$$
\\hat{R} =
\\frac{\\sum w_i I(\\text{man})}
     {\\sum w_i I(\\text{woman})}
$$

<br>

<b>2. Continuous variable over category</b><br>
Example: average income of men.

$$
\\hat{R} =
\\frac{\\sum w_i \\, income_i}
     {\\sum w_i \\, I(\\text{man})}
$$

<br>

<b>3. Ratio between continuous variables</b><br>
Example: income / expenditure ratio.

$$
\\hat{R} =
\\frac{\\sum w_i \\, income_i}
     {\\sum w_i \\, expenditure_i}
$$

<br>

<b>Interpretation</b>

The ratio estimator is especially efficient when there is a strong
correlation between \\(Y\\) and \\(X\\), as the denominator acts as
an auxiliary variable reducing the variance of the estimator.
"
  } else {
"<b>Estimador de raz\u00f3n</b><br><br>

El estimador de raz\u00f3n compara dos totales poblacionales estimados
a partir de una muestra con pesos de expansi\u00f3n.

$$
\\hat{R} =
\\frac{\\sum_{i \\in s} w_i y_i}
     {\\sum_{i \\in s} w_i x_i}
$$

donde:

<ul>
<li>\\(w_i\\) es el peso de expansi\u00f3n</li>
<li>\\(y_i\\) es la variable del numerador</li>
<li>\\(x_i\\) es la variable del denominador</li>
<li>\\(s\\) representa el conjunto de unidades de la muestra</li>
</ul>

<br>

<b>Relaci\u00f3n con los estimadores de Horvitz-Thompson</b>

$$
\\hat{R} = \\frac{\\hat{Y}}{\\hat{X}}
$$

con

$$
\\hat{Y} = \\sum_{i \\in s} w_i y_i
\\qquad
\\hat{X} = \\sum_{i \\in s} w_i x_i
$$

<br>

<b>Varianza aproximada</b>

El estimador es no lineal, por lo que su varianza se aproxima mediante
linearizaci\u00f3n de Taylor:

$$
\\widehat{Var}(\\hat{R}) \\approx
\\frac{1}{\\hat{X}^2}
\\widehat{Var}(\\hat{Y} - \\hat{R}\\hat{X})
$$

<br>

<b>Casos particulares utilizados en la aplicaci\u00f3n</b>

<br>

<b>1. Raz\u00f3n entre categor\u00edas</b><br>
Ejemplo: proporci\u00f3n de hombres respecto a mujeres.

Sea

$$
y_i = I(\\text{hombre})
\\qquad
x_i = I(\\text{mujer})
$$

El estimador es:

$$
\\hat{R} =
\\frac{\\sum w_i I(\\text{hombre})}
     {\\sum w_i I(\\text{mujer})}
$$

<br>

<b>2. Variable continua sobre categor\u00eda</b><br>
Ejemplo: ingreso promedio de los hombres.

$$
\\hat{R} =
\\frac{\\sum w_i \\, ingreso_i}
     {\\sum w_i \\, I(\\text{hombre})}
$$

<br>

<b>3. Raz\u00f3n entre variables continuas</b><br>
Ejemplo: relaci\u00f3n ingreso / gasto.

$$
\\hat{R} =
\\frac{\\sum w_i \\, ingreso_i}
     {\\sum w_i \\, gasto_i}
$$

<br>

<b>Interpretaci\u00f3n</b>

El estimador de raz\u00f3n es especialmente eficiente cuando existe
una fuerte correlaci\u00f3n entre \\(Y\\) y \\(X\\), ya que el denominador
act\u00faa como variable auxiliar reduciendo la varianza del estimador.
"
  }
}

.th_quantile <- function(lang) {
  if (lang == "en") {
"<b>Weighted quantiles</b><br><br>

Population quantiles are defined from the weighted cumulative distribution function:

$$
F_w(y) =
\\frac{\\sum w_i I(Y_i \\le y)}{\\sum w_i}
$$

The quantile \\(q_p\\) satisfies:

$$
F_w(q_p) = p
$$

Weighted quantiles allow estimating percentiles of the population distribution under complex designs.

Variance is usually estimated via:

<ul>
<li>Woodruff linearization</li>
<li>Replication methods (Bootstrap, Jackknife, BRR)</li>
</ul>
"
  } else {
"<b>Cuantiles ponderados</b><br><br>

Los cuantiles poblacionales se definen a partir de la funci\u00f3n de distribuci\u00f3n acumulada ponderada:

$$
F_w(y) =
\\frac{\\sum w_i I(Y_i \\le y)}{\\sum w_i}
$$

El cuantil \\(q_p\\) satisface:

$$
F_w(q_p) = p
$$

Los cuantiles ponderados permiten estimar percentiles de la distribuci\u00f3n poblacional bajo dise\u00f1os complejos.

La varianza suele estimarse mediante:

<ul>
<li>linearizaci\u00f3n de Woodruff</li>
<li>m\u00e9todos de replicaci\u00f3n (Bootstrap, Jackknife, BRR)</li>
</ul>
"
  }
}

.th_quality <- function(lang) {
  if (lang == "en") {
"<b>Coefficient of Variation (CV)</b><br><br>

The coefficient of variation measures the relative precision of the estimator.

$$
CV(\\hat{\\theta}) =
\\frac{SE(\\hat{\\theta})}{\\hat{\\theta}}
$$

In percentage:

$$
CV(\\hat{\\theta}) =
\\frac{SE(\\hat{\\theta})}{\\hat{\\theta}} \\times 100
$$

<br>

<b>Interpretation</b>

<ul>
<li><b>&lt; 5%</b> &rarr; Very high precision</li>
<li><b>5% &ndash; 10%</b> &rarr; High precision</li>
<li><b>10% &ndash; 20%</b> &rarr; Acceptable precision</li>
<li><b>20% &ndash; 30%</b> &rarr; Use with caution</li>
<li><b>&gt; 30%</b> &rarr; Low precision</li>
</ul>

<br>

<hr>

<b>Design Effect</b><br><br>

The design effect measures how much the variance increases due to the complex sampling design compared to simple random sampling.

$$
DEFF =
\\frac{Var_{design}(\\hat{\\theta})}
{Var_{SRS}(\\hat{\\theta})}
$$

<br>

<b>Effective sample size</b>

$$
n_{eff} =
\\frac{n}{DEFF}
$$

<br>
<hr><b>References</b>
<ul>
<li>Cochran, W. (1977). <i>Sampling Techniques</i>. Wiley.</li>
<li>S&auml;rndal, C., Swensson, B., &amp; Wretman, J. (1992). <i>Model Assisted Survey Sampling</i>. Springer.</li>
<li>Lohr, S. (2021). <i>Sampling: Design and Analysis</i>. CRC Press.</li>
<li>Lumley, T. (2010). <i>Complex Surveys: A Guide to Analysis Using R</i>. Wiley.</li>
</ul>
"
  } else {
"<b>Coeficiente de Variaci\u00f3n (CV)</b><br><br>

El coeficiente de variaci\u00f3n mide la precisi\u00f3n relativa del estimador.

$$
CV(\\hat{\\theta}) =
\\frac{SE(\\hat{\\theta})}{\\hat{\\theta}}
$$

En porcentaje:

$$
CV(\\hat{\\theta}) =
\\frac{SE(\\hat{\\theta})}{\\hat{\\theta}} \\times 100
$$

<br>

<b>Interpretaci\u00f3n</b>

<ul>
<li><b>&lt; 5%</b> &rarr; Muy alta precisi\u00f3n</li>
<li><b>5% &ndash; 10%</b> &rarr; Alta precisi\u00f3n</li>
<li><b>10% &ndash; 20%</b> &rarr; Precisi\u00f3n aceptable</li>
<li><b>20% &ndash; 30%</b> &rarr; Uso con cautela</li>
<li><b>&gt; 30%</b> &rarr; Baja precisi\u00f3n</li>
</ul>

<br>

<hr>

<b>Efecto de dise\u00f1o (Design Effect)</b><br><br>

El efecto de dise\u00f1o mide cu\u00e1nto aumenta la varianza debido al dise\u00f1o muestral complejo comparado con un muestreo aleatorio simple.

$$
DEFF =
\\frac{Var_{dise\u00f1o}(\\hat{\\theta})}
{Var_{MAS}(\\hat{\\theta})}
$$

<br>

<b>Tama\u00f1o de muestra efectivo</b>

$$
n_{eff} =
\\frac{n}{DEFF}
$$

<br>
<hr><b>Referencias</b>
<ul>
<li>Cochran, W. (1977). <i>Sampling Techniques</i>. Wiley.</li>
<li>S&auml;rndal, C., Swensson, B., &amp; Wretman, J. (1992). <i>Model Assisted Survey Sampling</i>. Springer.</li>
<li>Lohr, S. (2021). <i>Sampling: Design and Analysis</i>. CRC Press.</li>
<li>Lumley, T. (2010). <i>Complex Surveys: A Guide to Analysis Using R</i>. Wiley.</li>
</ul>
"
  }
}

# ---------------------------------------------------------------------------
# Module server
# ---------------------------------------------------------------------------

mod_estimacion_server <- function(id, design, dict) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # --------------------------------------------------
    # 0. Static labels
    # --------------------------------------------------
    output$title        <- shiny::renderText({ i18n_t(dict(), "mod_estimacion.title") })
    output$subtitle     <- shiny::renderText({ i18n_t(dict(), "mod_estimacion.subtitle") })
    output$params_card  <- shiny::renderText({ i18n_t(dict(), "mod_estimacion.params_card") })
    output$run_btn      <- shiny::renderText({ i18n_t(dict(), "mod_estimacion.run_btn") })
    output$results_card <- shiny::renderText({ i18n_t(dict(), "mod_estimacion.results_card") })
    output$theory_title <- shiny::renderText({ i18n_t(dict(), "mod_estimacion.theory_title") })
    output$status_title <- shiny::renderText({ i18n_t(dict(), "mod_estimacion.status_title") })
    output$result_title <- shiny::renderText({ i18n_t(dict(), "mod_estimacion.result_title") })
    output$quality_title <- shiny::renderText({ i18n_t(dict(), "mod_estimacion.quality_title") })

    # --------------------------------------------------
    # 0b. Update input labels/choices when language changes
    # --------------------------------------------------
    shiny::observe({
      d <- dict()
      shiny::updateSelectInput(session, "estimator",
        label   = i18n_t(d, "mod_estimacion.estimator_label"),
        choices = stats::setNames(
          c("mean", "total", "prop", "ratio", "quantile"),
          c(i18n_t(d, "mod_estimacion.mean"),
            i18n_t(d, "mod_estimacion.total"),
            i18n_t(d, "mod_estimacion.prop"),
            i18n_t(d, "mod_estimacion.ratio"),
            i18n_t(d, "mod_estimacion.quantile"))
        )
      )
      shiny::updateSelectInput(session, "numerator",
        label = i18n_t(d, "mod_estimacion.numerator"))
      shiny::updateSelectInput(session, "denominator",
        label = i18n_t(d, "mod_estimacion.denominator"))
      shiny::updateTextInput(session, "probs",
        label = i18n_t(d, "mod_estimacion.probs"))
      shiny::updateSelectInput(session, "y_var",
        label = i18n_t(d, "mod_estimacion.y_var"))
      shiny::updateSelectInput(session, "domain_vars",
        label = i18n_t(d, "mod_estimacion.domain_vars"))
    })

    # --------------------------------------------------
    # 1. Theory box
    # --------------------------------------------------
    output$theory_box <- shiny::renderUI({

      shiny::req(input$estimator)
      d    <- dict()
      lang <- i18n_t(d, "lang")

      theory_text <- switch(
        input$estimator,
        "mean"     = .th_mean(lang),
        "total"    = .th_total(lang),
        "prop"     = .th_prop(lang),
        "ratio"    = .th_ratio(lang),
        "quantile" = .th_quantile(lang),
        i18n_t(d, "mod_estimacion.select_theory")
      )

      shiny::tagList(
        shiny::withMathJax(),
        shiny::div(
          style = "background-color:#f8f9fa; padding:20px; border-radius:10px; font-size:14px;",
          shiny::HTML(theory_text)
        )
      )
    })

    shiny::observeEvent(input$estimator, {
      if (input$estimator == "ratio") {
        shiny::updateSelectInput(session, "y_var", selected = NULL)
      }
    })

    # --------------------------------------------------
    # 2. Quality indicators
    # --------------------------------------------------
    output$quality_theory <- shiny::renderUI({
      lang <- i18n_t(dict(), "lang")
      shiny::withMathJax(
        shiny::div(
          style = "background:#f8f9fa; padding:20px; border-radius:10px; font-size:14px;",
          shiny::HTML(.th_quality(lang))
        )
      )
    })

    # --------------------------------------------------
    # 3. Populate selectors from design
    # --------------------------------------------------
    shiny::observeEvent(design(), {

      des <- design()
      shiny::req(des)
      shiny::req(!is.null(des$variables))

      vars <- names(des$variables)
      shiny::req(length(vars) > 0)

      d    <- dict()
      none <- stats::setNames("", i18n_t(d, "mod_estimacion.none"))

      shiny::updateSelectInput(session, "y_var",       choices = vars)
      shiny::updateSelectInput(session, "numerator",   choices = vars)
      shiny::updateSelectInput(session, "denominator", choices = vars)
      shiny::updateSelectInput(session, "domain_vars", choices = c(none, vars))
    })

    # --------------------------------------------------
    # 3b. Filter variables by estimator type
    # --------------------------------------------------
    shiny::observeEvent(list(design(), input$estimator), {

      des <- design()
      shiny::req(des)
      shiny::req(!is.null(des$variables))
      shiny::req(input$estimator)

      vars <- des$variables

      if (input$estimator %in% c("mean", "total", "quantile")) {
        valid_vars <- names(vars)[sapply(vars, is.numeric)]
      } else {
        valid_vars <- names(vars)
      }

      shiny::updateSelectInput(session, "y_var", choices = valid_vars)

    }, ignoreInit = TRUE)

    # --------------------------------------------------
    # 4. Dynamic ratio level UI
    # --------------------------------------------------
    output$ratio_levels_ui <- shiny::renderUI({

      shiny::req(input$estimator == "ratio")
      shiny::req(input$numerator, input$denominator)
      shiny::req(design())

      vars      <- design()$variables
      d         <- dict()

      num_is_cat <- is.factor(vars[[input$numerator]]) ||
        is.character(vars[[input$numerator]])

      den_is_cat <- is.factor(vars[[input$denominator]]) ||
        is.character(vars[[input$denominator]])

      ui <- list()

      if (num_is_cat) {
        ui <- c(ui, list(
          shiny::selectInput(
            ns("ratio_num_level"),
            i18n_t(d, "mod_estimacion.ratio_num_level"),
            choices = sort(unique(stats::na.omit(vars[[input$numerator]])))
          )
        ))
      }

      if (den_is_cat) {
        ui <- c(ui, list(
          shiny::selectInput(
            ns("ratio_den_level"),
            i18n_t(d, "mod_estimacion.ratio_den_level"),
            choices = sort(unique(stats::na.omit(vars[[input$denominator]])))
          )
        ))
      }

      if (length(ui) == 0) return(NULL)
      shiny::tagList(ui)
    })

    # --------------------------------------------------
    # 5. Run estimation
    # --------------------------------------------------
    results_r <- shiny::eventReactive(input$run, {

      des <- design()
      shiny::req(des, input$estimator)

      domain <- input$domain_vars
      if (length(domain) == 0 || all(domain == "")) domain <- NULL

      if (input$estimator == "ratio") {

        num <- input$numerator
        den <- input$denominator
        shiny::req(num, den)

        vars <- design()$variables

        num_is_cat <- is.factor(vars[[num]]) || is.character(vars[[num]])
        den_is_cat <- is.factor(vars[[den]]) || is.character(vars[[den]])

        return(
          estimate_survey(
            design          = des,
            estimator       = "ratio",
            by              = domain,
            numerator       = num,
            denominator     = den,
            ratio_num_level = if (num_is_cat) input$ratio_num_level else NULL,
            ratio_den_level = if (den_is_cat) input$ratio_den_level else NULL
          )
        )
      }

      if (input$estimator == "quantile") {

        shiny::req(input$y_var)
        probs <- trimws(unlist(strsplit(input$probs, ",")))
        probs <- sort(unique(as.numeric(probs)))
        shiny::req(length(probs) > 0, !any(is.na(probs)))

        return(
          estimate_survey(
            design    = des,
            estimator = "quantile",
            variable  = input$y_var,
            by        = domain,
            probs     = probs
          )
        )
      }

      shiny::req(input$y_var)
      estimate_survey(
        design    = des,
        estimator = input$estimator,
        variable  = input$y_var,
        by        = domain
      )

    }, ignoreInit = TRUE)

    # --------------------------------------------------
    # 6. Outputs
    # --------------------------------------------------
    output$log <- shiny::renderPrint({
      if (is.null(results_r())) {
        return(i18n_t(dict(), "mod_estimacion.not_run"))
      }
      d <- dict()
      stats::setNames(
        list(
          input$y_var,
          input$estimator,
          if (is.null(input$domain_vars) || all(input$domain_vars == "")) {
            i18n_t(d, "mod_estimacion.global")
          } else {
            input$domain_vars
          }
        ),
        c(i18n_t(d, "mod_estimacion.var_interes"),
          i18n_t(d, "mod_estimacion.tipo_est"),
          i18n_t(d, "mod_estimacion.dominios"))
      )
    })

    output$preview <- DT::renderDT({
      res <- results_r()
      shiny::req(res)

      res_fmt <- res %>%
        dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4)))

      DT::datatable(
        res_fmt,
        rownames = FALSE,
        options  = list(pageLength = 10, scrollX = TRUE)
      )
    })

    list(
      results = shiny::reactive(results_r()),
      meta    = shiny::reactive({
        res <- results_r()
        if (is.null(res)) return(NULL)
        list(
          estimator = input$estimator,
          variable  = if (input$estimator != "ratio") {
            input$y_var
          } else {
            paste0(input$numerator, " / ", input$denominator)
          },
          domains   = {
            d <- input$domain_vars
            if (length(d) == 0 || all(d == "")) character(0) else d
          },
          timestamp = Sys.time()
        )
      })
    )
  })
}
