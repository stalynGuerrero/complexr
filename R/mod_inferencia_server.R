# ---------------------------------------------------------------------------
# Theory HTML helpers \u2013 language-aware
# ---------------------------------------------------------------------------

.th_gini <- function(lang) {
  if (lang == "en") {
"<b>Gini Coefficient under Complex Sampling Design</b><br><br>

The Gini coefficient measures the degree of inequality in a distribution.
A value of 0 indicates perfect equality; a value of 1 indicates maximum
inequality.

<br><br>

<b>Lorenz Curve</b>

The Lorenz curve \\(L(p)\\) represents the cumulative share of income held
by the bottom \\(p\\) proportion of the population:

$$
L(p) = \\frac{\\int_0^p Q(u)\\,du}{\\mu}
$$

where \\(Q(u)\\) is the quantile function and \\(\\mu\\) is the population mean.

<br>

<b>Gini Estimator</b>

The Gini coefficient equals twice the area between the line of perfect
equality and the Lorenz curve:

$$
G = 1 - 2\\int_0^1 L(p)\\,dp
$$

Under a complex sampling design, the weighted estimator is:

$$
\\hat{G} = \\frac{2\\displaystyle\\sum_{i \\in s} w_i\\,\\hat{r}_i\\,y_i}
           {\\hat{N}\\,\\hat{\\mu}} - 1
$$

where \\(\\hat{r}_i\\) is the weighted rank of unit \\(i\\), computed
via the weighted cumulative distribution function.

<br>

<b>Variance Estimation</b>

Variance is estimated via Taylor linearization implemented in the
<code>convey</code> package, which extends the <code>survey</code> package
for inequality measures.

<br>
<hr>
<b>References</b>
<ul>
<li>Deville, J.C. (1999). Variance estimation for complex statistics and
estimators: Linearization and residual techniques. <i>Survey Methodology</i>,
25, 193\u2013203.</li>
<li>Osier, G. (2009). Variance estimation for complex indicators of poverty
and inequality. <i>Journal of the European Survey Research Association</i>.</li>
<li>Lumley, T. &amp; Schneider, B. (2023). <i>convey: Income Concentration
Analysis with Complex Survey Samples</i>. R package.</li>
</ul>
"
  } else {
"<b>Coeficiente de Gini bajo dise\u00f1o muestral complejo</b><br><br>

El coeficiente de Gini mide el grado de desigualdad en una distribuci\u00f3n.
Un valor de 0 indica perfecta igualdad; un valor de 1 indica m\u00e1xima
desigualdad.

<br><br>

<b>Curva de Lorenz</b>

La curva de Lorenz \\(L(p)\\) representa la proporci\u00f3n acumulada del
ingreso en poder del \\(p\\) por ciento m\u00e1s pobre de la poblaci\u00f3n:

$$
L(p) = \\frac{\\int_0^p Q(u)\\,du}{\\mu}
$$

donde \\(Q(u)\\) es la funci\u00f3n de cuantil y \\(\\mu\\) es la media poblacional.

<br>

<b>Estimador de Gini</b>

El coeficiente de Gini es el doble del \u00e1rea entre la l\u00ednea de perfecta
igualdad y la curva de Lorenz:

$$
G = 1 - 2\\int_0^1 L(p)\\,dp
$$

Bajo un dise\u00f1o muestral complejo, el estimador ponderado es:

$$
\\hat{G} = \\frac{2\\displaystyle\\sum_{i \\in s} w_i\\,\\hat{r}_i\\,y_i}
           {\\hat{N}\\,\\hat{\\mu}} - 1
$$

donde \\(\\hat{r}_i\\) es el rango ponderado de la unidad \\(i\\), calculado
mediante la funci\u00f3n de distribuci\u00f3n acumulada ponderada.

<br>

<b>Estimaci\u00f3n de la varianza</b>

La varianza se estima mediante linearizaci\u00f3n de Taylor implementada en el
paquete <code>convey</code>, que extiende el paquete <code>survey</code>
para medidas de desigualdad.

<br>
<hr>
<b>Referencias</b>
<ul>
<li>Deville, J.C. (1999). Variance estimation for complex statistics and
estimators: Linearization and residual techniques. <i>Survey Methodology</i>,
25, 193\u2013203.</li>
<li>Osier, G. (2009). Variance estimation for complex indicators of poverty
and inequality. <i>Journal of the European Survey Research Association</i>.</li>
<li>Lumley, T. &amp; Schneider, B. (2023). <i>convey: Income Concentration
Analysis with Complex Survey Samples</i>. R package.</li>
</ul>
"
  }
}

.th_correlation <- function(lang) {
  if (lang == "en") {
"<b>Weighted Pearson Correlation under Complex Sampling Design</b><br><br>

The Pearson correlation coefficient measures the linear association between
two quantitative variables.

<br><br>

<b>Design-Weighted Estimator</b>

$$
\\hat{\\rho}_{XY} =
\\frac{\\hat{\\text{Cov}}(X, Y)}
     {\\sqrt{\\hat{\\text{Var}}(X) \\cdot \\hat{\\text{Var}}(Y)}}
$$

where the design-weighted covariance is:

$$
\\hat{\\text{Cov}}(X, Y) =
\\frac{\\displaystyle\\sum_{i \\in s} w_i (x_i - \\hat{\\bar{X}})(y_i - \\hat{\\bar{Y}})}
{\\displaystyle\\sum_{i \\in s} w_i - 1}
$$

<br>

<b>Inference</b>

The degrees of freedom for design-based inference are obtained from the
sampling design (number of PSUs minus number of strata):

$$
df_{design} = \\sum_{h=1}^{H}(n_h - 1)
$$

The t-statistic is:

$$
t = \\hat{\\rho} \\sqrt{\\frac{df_{design} - 2}{1 - \\hat{\\rho}^2}}
$$

<b>Confidence Interval (Fisher's Z transformation)</b>

$$
z = \\tanh^{-1}(\\hat{\\rho}), \\quad
SE(z) \\approx \\frac{1}{\\sqrt{df_{design} - 3}}
$$

$$
CI_{\\rho} = \\tanh\\!\\left(z \\pm z_{\\alpha/2} \\cdot SE(z)\\right)
$$

<br>
<hr>
<b>References</b>
<ul>
<li>Lumley, T. (2010). <i>Complex Surveys: A Guide to Analysis Using R</i>.
Wiley.</li>
<li>Lohr, S. (2021). <i>Sampling: Design and Analysis</i>. CRC Press.</li>
</ul>
"
  } else {
"<b>Correlaci\u00f3n de Pearson ponderada bajo dise\u00f1o muestral complejo</b><br><br>

El coeficiente de correlaci\u00f3n de Pearson mide la asociaci\u00f3n lineal entre
dos variables cuantitativas.

<br><br>

<b>Estimador ponderado por dise\u00f1o</b>

$$
\\hat{\\rho}_{XY} =
\\frac{\\hat{\\text{Cov}}(X, Y)}
     {\\sqrt{\\hat{\\text{Var}}(X) \\cdot \\hat{\\text{Var}}(Y)}}
$$

donde la covarianza ponderada por el dise\u00f1o es:

$$
\\hat{\\text{Cov}}(X, Y) =
\\frac{\\displaystyle\\sum_{i \\in s} w_i (x_i - \\hat{\\bar{X}})(y_i - \\hat{\\bar{Y}})}
{\\displaystyle\\sum_{i \\in s} w_i - 1}
$$

<br>

<b>Inferencia</b>

Los grados de libertad para la inferencia basada en el dise\u00f1o se obtienen
del dise\u00f1o muestral (n\u00famero de UPM menos n\u00famero de estratos):

$$
df_{dise\u00f1o} = \\sum_{h=1}^{H}(n_h - 1)
$$

El estad\u00edstico t es:

$$
t = \\hat{\\rho} \\sqrt{\\frac{df_{dise\u00f1o} - 2}{1 - \\hat{\\rho}^2}}
$$

<b>Intervalo de confianza (transformaci\u00f3n Z de Fisher)</b>

$$
z = \\tanh^{-1}(\\hat{\\rho}), \\quad
SE(z) \\approx \\frac{1}{\\sqrt{df_{dise\u00f1o} - 3}}
$$

$$
IC_{\\rho} = \\tanh\\!\\left(z \\pm z_{\\alpha/2} \\cdot SE(z)\\right)
$$

<br>
<hr>
<b>Referencias</b>
<ul>
<li>Lumley, T. (2010). <i>Complex Surveys: A Guide to Analysis Using R</i>.
Wiley.</li>
<li>Lohr, S. (2021). <i>Sampling: Design and Analysis</i>. CRC Press.</li>
</ul>
"
  }
}

.th_hyptest <- function(lang) {
  if (lang == "en") {
"<b>Hypothesis Test: Difference of Means under Complex Design</b><br><br>

<b>Hypotheses</b>

$$
H_0: \\mu_1 = \\mu_2
\\qquad \\text{vs} \\qquad
H_1: \\mu_1 \\neq \\mu_2
$$

<b>Test Statistic</b>

The design-based t-test statistic for the difference of two population
means is:

$$
t =
\\frac{\\hat{\\bar{Y}}_1 - \\hat{\\bar{Y}}_2}
     {\\sqrt{\\widehat{\\text{Var}}(\\hat{\\bar{Y}}_1 - \\hat{\\bar{Y}}_2)}}
$$

where the variance of the difference accounts for the survey design through
Taylor linearization.

<br>

<b>Degrees of Freedom</b>

Under the complex design, the degrees of freedom are:

$$
df = \\sum_{h=1}^{H}(n_h - 1)
$$

where \\(n_h\\) is the number of PSUs (primary sampling units) in stratum
\\(h\\).

<br>

<b>Confidence Interval for the Difference</b>

$$
(\\hat{\\bar{Y}}_1 - \\hat{\\bar{Y}}_2)
\\pm t_{\\alpha/2,\\,df} \\cdot SE(\\hat{\\bar{Y}}_1 - \\hat{\\bar{Y}}_2)
$$

<br>

<b>Decision Rule</b>

<ul>
<li>\\(p\\text{-value} > \\alpha\\): Fail to reject \\(H_0\\) \u2014 no statistically
significant difference detected.</li>
<li>\\(p\\text{-value} \\leq \\alpha\\): Reject \\(H_0\\) \u2014 statistically
significant difference detected.</li>
</ul>

Implemented via <code>survey::svyttest()</code>.

<br>
<hr>
<b>References</b>
<ul>
<li>Lumley, T. (2010). <i>Complex Surveys: A Guide to Analysis Using R</i>.
Wiley.</li>
<li>S&auml;rndal, C., Swensson, B., &amp; Wretman, J. (1992).
<i>Model Assisted Survey Sampling</i>. Springer.</li>
</ul>
"
  } else {
"<b>Prueba de hip\u00f3tesis: diferencia de medias bajo dise\u00f1o complejo</b><br><br>

<b>Hip\u00f3tesis</b>

$$
H_0: \\mu_1 = \\mu_2
\\qquad \\text{vs} \\qquad
H_1: \\mu_1 \\neq \\mu_2
$$

<b>Estad\u00edstico de prueba</b>

El estad\u00edstico t basado en el dise\u00f1o para la diferencia de dos medias
poblacionales es:

$$
t =
\\frac{\\hat{\\bar{Y}}_1 - \\hat{\\bar{Y}}_2}
     {\\sqrt{\\widehat{\\text{Var}}(\\hat{\\bar{Y}}_1 - \\hat{\\bar{Y}}_2)}}
$$

donde la varianza de la diferencia incorpora el dise\u00f1o muestral mediante
linearizaci\u00f3n de Taylor.

<br>

<b>Grados de libertad</b>

Bajo el dise\u00f1o complejo, los grados de libertad son:

$$
df = \\sum_{h=1}^{H}(n_h - 1)
$$

donde \\(n_h\\) es el n\u00famero de UPM (unidades primarias de muestreo) en
el estrato \\(h\\).

<br>

<b>Intervalo de confianza para la diferencia</b>

$$
(\\hat{\\bar{Y}}_1 - \\hat{\\bar{Y}}_2)
\\pm t_{\\alpha/2,\\,df} \\cdot SE(\\hat{\\bar{Y}}_1 - \\hat{\\bar{Y}}_2)
$$

<br>

<b>Regla de decisi\u00f3n</b>

<ul>
<li>\\(p\\text{-valor} > \\alpha\\): No se rechaza \\(H_0\\) \u2014 no se detecta
diferencia estad\u00edsticamente significativa.</li>
<li>\\(p\\text{-valor} \\leq \\alpha\\): Se rechaza \\(H_0\\) \u2014 diferencia
estad\u00edsticamente significativa.</li>
</ul>

Implementado mediante <code>survey::svyttest()</code>.

<br>
<hr>
<b>Referencias</b>
<ul>
<li>Lumley, T. (2010). <i>Complex Surveys: A Guide to Analysis Using R</i>.
Wiley.</li>
<li>S&auml;rndal, C., Swensson, B., &amp; Wretman, J. (1992).
<i>Model Assisted Survey Sampling</i>. Springer.</li>
</ul>
"
  }
}

.th_crosstable <- function(lang) {
  if (lang == "en") {
"<b>Cross-tabulation and Independence Testing under Complex Design</b><br><br>

<b>Cross-table of Proportions</b>

For an R\u00d7C contingency table, the joint proportion for cell \\((r,c)\\) is:

$$
\\hat{p}_{rc} =
\\frac{\\displaystyle\\sum_{i \\in s} w_i\\,I(x_i = r;\\; y_i = c)}
     {\\displaystyle\\sum_{i \\in s} w_i}
$$

<br>

<b>Rao-Scott Chi-square Adjustment</b>

The classical Pearson statistic under independence
\\((H_0: p_{rc} = p_{r+} \\cdot p_{+c})\\) is:

$$
X_P^2 = n_{++}
\\sum_r \\sum_c
\\frac{(\\hat{p}_{rc} - \\hat{p}_{rc}^0)^2}{\\hat{p}_{rc}^0}
$$

For complex designs, the Rao-Scott correction accounts for the
generalized design effect (GDEFF):

$$
X_{RS}^2 = \\frac{X_P^2}{\\overline{\\delta}}
$$

where \\(\\overline{\\delta}\\) is the mean generalized design effect across
cells, and \\(X_{RS}^2 \\sim \\chi^2_{(R-1)(C-1)}\\) asymptotically.

<br>

<b>F-version (recommended)</b>

The F-version converts the chi-square to an F statistic for better
finite-sample performance:

$$
F = \\frac{X_{RS}^2}{(R-1)(C-1)}
\\sim F_{(R-1)(C-1),\\; df_{design}-(R-1)(C-1)}
$$

<br>

Implemented via <code>survey::svychisq()</code>.

<br>
<hr>
<b>References</b>
<ul>
<li>Rao, J.N.K. &amp; Scott, A.J. (1981). The analysis of categorical data
from complex sample surveys. <i>JASA</i>, 76, 221\u2013230.</li>
<li>Lumley, T. (2010). <i>Complex Surveys: A Guide to Analysis Using R</i>.
Wiley.</li>
</ul>
"
  } else {
"<b>Tablas cruzadas y prueba de independencia bajo dise\u00f1o complejo</b><br><br>

<b>Tabla de proporciones</b>

Para una tabla de contingencia R\u00d7C, la proporci\u00f3n conjunta para la celda
\\((r,c)\\) es:

$$
\\hat{p}_{rc} =
\\frac{\\displaystyle\\sum_{i \\in s} w_i\\,I(x_i = r;\\; y_i = c)}
     {\\displaystyle\\sum_{i \\in s} w_i}
$$

<br>

<b>Ajuste chi-cuadrado de Rao-Scott</b>

El estad\u00edstico de Pearson cl\u00e1sico bajo independencia
\\((H_0: p_{rc} = p_{r+} \\cdot p_{+c})\\) es:

$$
X_P^2 = n_{++}
\\sum_r \\sum_c
\\frac{(\\hat{p}_{rc} - \\hat{p}_{rc}^0)^2}{\\hat{p}_{rc}^0}
$$

Para dise\u00f1os complejos, la correcci\u00f3n de Rao-Scott incorpora el efecto de
dise\u00f1o generalizado (GDEFF):

$$
X_{RS}^2 = \\frac{X_P^2}{\\overline{\\delta}}
$$

donde \\(\\overline{\\delta}\\) es el efecto de dise\u00f1o generalizado promedio
de las celdas, y \\(X_{RS}^2 \\sim \\chi^2_{(R-1)(C-1)}\\) asint\u00f3ticamente.

<br>

<b>Versi\u00f3n F (recomendada)</b>

La versi\u00f3n F transforma el chi-cuadrado en un estad\u00edstico F para mejor
desempe\u00f1o en muestras finitas:

$$
F = \\frac{X_{RS}^2}{(R-1)(C-1)}
\\sim F_{(R-1)(C-1),\\; df_{dise\u00f1o}-(R-1)(C-1)}
$$

<br>

Implementado mediante <code>survey::svychisq()</code>.

<br>
<hr>
<b>Referencias</b>
<ul>
<li>Rao, J.N.K. &amp; Scott, A.J. (1981). The analysis of categorical data
from complex sample surveys. <i>JASA</i>, 76, 221\u2013230.</li>
<li>Lumley, T. (2010). <i>Complex Surveys: A Guide to Analysis Using R</i>.
Wiley.</li>
</ul>
"
  }
}

.th_contrasts <- function(lang) {
  if (lang == "en") {
"<b>Multiple Pairwise Comparisons via Linear Contrasts</b><br><br>

Given \\(K\\) population subgroups, all pairwise differences between group
means can be expressed as linear contrasts.

<br>

<b>Contrast Estimator</b>

For groups \\(i\\) and \\(j\\), the contrast is:

$$
\\hat{\\delta}_{ij} = \\hat{\\bar{Y}}_i - \\hat{\\bar{Y}}_j =
\\mathbf{c}_{ij}^\\top \\hat{\\boldsymbol{\\mu}}
$$

where \\(\\mathbf{c}_{ij}\\) is a vector with \\(+1\\) in position \\(i\\),
\\(-1\\) in position \\(j\\), and \\(0\\) elsewhere, and
\\(\\hat{\\boldsymbol{\\mu}}\\) is the vector of group means.

<br>

<b>Variance via Delta Method</b>

The joint covariance matrix \\(\\hat{\\boldsymbol{V}}\\) of all group means
is estimated under the complex design. The variance of the contrast is:

$$
\\widehat{\\text{Var}}(\\hat{\\delta}_{ij}) =
\\mathbf{c}_{ij}^\\top \\hat{\\boldsymbol{V}} \\, \\mathbf{c}_{ij}
$$

<b>Test Statistic</b>

$$
t_{ij} =
\\frac{\\hat{\\delta}_{ij}}
     {\\sqrt{\\widehat{\\text{Var}}(\\hat{\\delta}_{ij})}}
\\sim t_{df_{design}}
$$

<b>Multiple Testing: Bonferroni Correction</b>

For \\(m = \\binom{K}{2}\\) simultaneous tests, the Bonferroni-adjusted
significance threshold is:

$$
\\alpha^* = \\frac{\\alpha}{m}
$$

The adjusted p-value is \\(\\min(m \\cdot p_{ij},\\, 1)\\).

<br>

<b>Significance codes</b>

<table style='font-size:13px;border-collapse:collapse;'>
<tr><td><b>***</b></td><td>&nbsp; \\(p &lt; 0.001\\)</td></tr>
<tr><td><b>**</b></td><td>&nbsp; \\(p &lt; 0.01\\)</td></tr>
<tr><td><b>*</b></td><td>&nbsp; \\(p &lt; 0.05\\)</td></tr>
<tr><td><b>.</b></td><td>&nbsp; \\(p &lt; 0.10\\)</td></tr>
<tr><td></td><td>&nbsp; \\(p \\geq 0.10\\)</td></tr>
</table>

<br>

Implemented via <code>survey::svyby(covmat = TRUE)</code> and
<code>survey::svycontrast()</code>.

<br>
<hr>
<b>References</b>
<ul>
<li>Lumley, T. (2010). <i>Complex Surveys: A Guide to Analysis Using R</i>.
Wiley.</li>
<li>Rao, J.N.K. &amp; Scott, A.J. (1984). On chi-squared tests for multiway
contingency tables with cell proportions estimated from survey data.
<i>The Annals of Statistics</i>, 12(1), 46\u201360.</li>
</ul>
"
  } else {
"<b>Comparaciones m\u00faltiples por pares mediante contrastes lineales</b><br><br>

Dados \\(K\\) subgrupos poblacionales, todas las diferencias entre pares de
medias grupales pueden expresarse como contrastes lineales.

<br>

<b>Estimador del contraste</b>

Para los grupos \\(i\\) y \\(j\\), el contraste es:

$$
\\hat{\\delta}_{ij} = \\hat{\\bar{Y}}_i - \\hat{\\bar{Y}}_j =
\\mathbf{c}_{ij}^\\top \\hat{\\boldsymbol{\\mu}}
$$

donde \\(\\mathbf{c}_{ij}\\) es un vector con \\(+1\\) en la posici\u00f3n \\(i\\),
\\(-1\\) en la posici\u00f3n \\(j\\) y \\(0\\) en las restantes, y
\\(\\hat{\\boldsymbol{\\mu}}\\) es el vector de medias grupales.

<br>

<b>Varianza mediante el m\u00e9todo delta</b>

La matriz de covarianza conjunta \\(\\hat{\\boldsymbol{V}}\\) de todas las medias
grupales se estima bajo el dise\u00f1o complejo. La varianza del contraste es:

$$
\\widehat{\\text{Var}}(\\hat{\\delta}_{ij}) =
\\mathbf{c}_{ij}^\\top \\hat{\\boldsymbol{V}} \\, \\mathbf{c}_{ij}
$$

<b>Estad\u00edstico de prueba</b>

$$
t_{ij} =
\\frac{\\hat{\\delta}_{ij}}
     {\\sqrt{\\widehat{\\text{Var}}(\\hat{\\delta}_{ij})}}
\\sim t_{df_{dise\u00f1o}}
$$

<b>Comparaciones m\u00faltiples: correcci\u00f3n de Bonferroni</b>

Para \\(m = \\binom{K}{2}\\) pruebas simult\u00e1neas, el umbral de significancia
ajustado por Bonferroni es:

$$
\\alpha^* = \\frac{\\alpha}{m}
$$

El p-valor ajustado es \\(\\min(m \\cdot p_{ij},\\, 1)\\).

<br>

<b>C\u00f3digos de significancia</b>

<table style='font-size:13px;border-collapse:collapse;'>
<tr><td><b>***</b></td><td>&nbsp; \\(p &lt; 0.001\\)</td></tr>
<tr><td><b>**</b></td><td>&nbsp; \\(p &lt; 0.01\\)</td></tr>
<tr><td><b>*</b></td><td>&nbsp; \\(p &lt; 0.05\\)</td></tr>
<tr><td><b>.</b></td><td>&nbsp; \\(p &lt; 0.10\\)</td></tr>
<tr><td></td><td>&nbsp; \\(p \\geq 0.10\\)</td></tr>
</table>

<br>

Implementado mediante <code>survey::svyby(covmat = TRUE)</code> y
<code>survey::svycontrast()</code>.

<br>
<hr>
<b>Referencias</b>
<ul>
<li>Lumley, T. (2010). <i>Complex Surveys: A Guide to Analysis Using R</i>.
Wiley.</li>
<li>Rao, J.N.K. &amp; Scott, A.J. (1984). On chi-squared tests for multiway
contingency tables with cell proportions estimated from survey data.
<i>The Annals of Statistics</i>, 12(1), 46\u201360.</li>
</ul>
"
  }
}

# ---------------------------------------------------------------------------
# Computation helpers
# ---------------------------------------------------------------------------

.inf_gini <- function(design, variable, domain = NULL, conf_level = 0.95) {

  if (!requireNamespace("convey", quietly = TRUE)) {
    stop(paste0(
      "El paquete 'convey' es necesario para el coeficiente de Gini.\n",
      "Inst\u00e1lalo con: install.packages('convey')"
    ))
  }

  # Validate: variable must be numeric
  y <- design$variables[[variable]]
  if (is.null(y))        stop(paste0("Variable '", variable, "' no encontrada en el dise\u00f1o."))
  if (!is.numeric(y))    stop(paste0("La variable '", variable, "' debe ser num\u00e9rica para calcular el Gini."))
  if (any(y < 0, na.rm = TRUE)) stop(paste0("La variable '", variable, "' contiene valores negativos; el Gini requiere valores >= 0."))

  frm    <- stats::as.formula(paste0("~", variable))
  des_cv <- convey::convey_prep(design)

  if (is.null(domain) || domain == "") {
    res    <- convey::svygini(frm, design = des_cv, na.rm = TRUE)
    gini   <- as.numeric(stats::coef(res))
    se_val <- as.numeric(sqrt(diag(stats::vcov(res))))
    ci     <- as.numeric(stats::confint(res, level = conf_level))
    cv_val <- se_val / abs(gini) * 100

    tibble::tibble(
      variable = variable,
      gini     = round(gini, 6),
      se       = round(se_val, 6),
      cv       = round(cv_val, 2),
      lci      = round(ci[1], 6),
      uci      = round(ci[2], 6)
    )

  } else {
    frm_by <- stats::as.formula(paste0("~", domain))
    res    <- survey::svyby(
      formula    = frm,
      by         = frm_by,
      design     = des_cv,
      FUN        = convey::svygini,
      na.rm      = TRUE,
      keep.names = FALSE
    )

    # Robust CI: fall back to NA matrix if confint fails (e.g. singleton strata)
    ci <- tryCatch(
      stats::confint(res, level = conf_level),
      error = function(e) {
        matrix(NA_real_, nrow = nrow(res), ncol = 2,
               dimnames = list(NULL, c("2.5 %", "97.5 %")))
      }
    )

    # Identify estimate and SE columns.
    # svyby with scalar FUN (svygini) names the SE column "se" (no dot-prefix).
    # svyby with vector FUN (svymean) names them "se.varname".
    # Both patterns are covered by "^se" \u2014 we exclude the domain column first.
    non_domain <- setdiff(names(res), domain)
    se_cols    <- grep("^se", non_domain, value = TRUE)
    est_col    <- setdiff(non_domain, se_cols)[1]
    se_col     <- se_cols[1]

    if (is.na(est_col)) {
      stop(paste0(
        "No se pudo identificar la columna del Gini en el resultado. ",
        "Columnas disponibles: ", paste(names(res), collapse = ", ")
      ))
    }

    gini_vals <- as.numeric(res[[est_col]])
    se_vals   <- as.numeric(res[[se_col]])

    tibble::tibble(
      domain_var = domain,
      domain_val = as.character(res[[domain]]),
      variable   = variable,
      gini       = round(gini_vals, 6),
      se         = round(se_vals, 6),
      cv         = round(se_vals / abs(gini_vals) * 100, 2),
      lci        = round(as.numeric(ci[, 1]), 6),
      uci        = round(as.numeric(ci[, 2]), 6)
    )
  }
}

.inf_correlation <- function(design, x_var, y_var, conf_level = 0.95) {

  frm      <- stats::as.formula(paste0("~", x_var, "+", y_var))
  vcov_mat <- as.matrix(survey::svyvar(frm, design = design, na.rm = TRUE))
  var_x    <- vcov_mat[x_var, x_var]
  var_y    <- vcov_mat[y_var, y_var]
  cov_xy   <- vcov_mat[x_var, y_var]
  r        <- cov_xy / sqrt(var_x * var_y)

  df     <- survey::degf(design)
  t_stat <- r * sqrt((df - 2) / (1 - r^2))
  p_val  <- 2 * stats::pt(-abs(t_stat), df = df - 2)

  # Fisher Z CI
  z      <- atanh(r)
  se_z   <- 1 / sqrt(max(df - 3, 1))
  z_crit <- stats::qnorm(1 - (1 - conf_level) / 2)
  ci     <- tanh(c(z - z_crit * se_z, z + z_crit * se_z))

  tibble::tibble(
    x_var       = x_var,
    y_var       = y_var,
    correlation = round(r, 6),
    t_stat      = round(t_stat, 4),
    df          = df - 2,
    p_value     = round(p_val, 6),
    lci         = round(ci[1], 6),
    uci         = round(ci[2], 6)
  )
}

.inf_hyptest <- function(design, y_var, group_var, conf_level = 0.95) {

  vars     <- design$variables
  grp_vals <- sort(unique(stats::na.omit(vars[[group_var]])))

  if (length(grp_vals) != 2L) {
    stop(paste0(
      "La variable de grupo '", group_var,
      "' debe tener exactamente 2 categorias (tiene ", length(grp_vals), ")."
    ))
  }

  frm    <- stats::as.formula(paste0(y_var, "~", group_var))
  result <- survey::svyttest(frm, design = design)

  t_val    <- as.numeric(result$statistic)
  df_val   <- as.numeric(result$parameter)
  p_val    <- result$p.value
  ci       <- as.numeric(result$conf.int)
  diff_est <- as.numeric(result$estimate)

  # SE approximated from CI
  se_val <- (ci[2] - ci[1]) / (2 * stats::qt(0.975, df = df_val))

  # Group means via svyby
  grp_means <- survey::svyby(
    formula    = stats::as.formula(paste0("~", y_var)),
    by         = stats::as.formula(paste0("~", group_var)),
    design     = design,
    FUN        = survey::svymean,
    na.rm      = TRUE,
    keep.names = FALSE
  )
  mean_col <- grep(paste0("^", y_var), names(grp_means), value = TRUE)[1]
  se_col   <- grep("^se", names(grp_means), value = TRUE)[1]
  g1 <- as.character(grp_means[[group_var]][1])
  g2 <- as.character(grp_means[[group_var]][2])
  m1 <- grp_means[[mean_col]][1]
  m2 <- grp_means[[mean_col]][2]
  s1 <- if (!is.null(se_col) && !is.na(se_col)) grp_means[[se_col]][1] else NA_real_
  s2 <- if (!is.null(se_col) && !is.na(se_col)) grp_means[[se_col]][2] else NA_real_

  tibble::tibble(
    variable  = y_var,
    group_var = group_var,
    group_1   = g1,
    group_2   = g2,
    mean_1    = round(m1, 4),
    mean_2    = round(m2, 4),
    se_1      = round(s1, 4),
    se_2      = round(s2, 4),
    mean_diff = round(diff_est, 4),
    se        = round(se_val, 4),
    t_stat    = round(t_val, 4),
    df        = round(df_val, 2),
    p_value   = round(p_val, 6),
    lci       = round(ci[1], 4),
    uci       = round(ci[2], 4)
  )
}

.inf_crosstable <- function(design, row_var, col_var, statistic = "F") {

  frm     <- stats::as.formula(paste0("~", row_var, "+", col_var))
  chi_res <- survey::svychisq(frm, design = design, statistic = statistic)

  stat_val  <- as.numeric(chi_res$statistic)
  p_val     <- chi_res$p.value
  params    <- chi_res$parameter

  test_tbl <- tibble::tibble(
    row_var      = row_var,
    col_var      = col_var,
    statistic    = statistic,
    value        = round(stat_val, 4),
    df_num       = round(params[1], 2),
    df_den       = if (length(params) > 1) round(params[2], 2) else NA_real_,
    p_value      = round(p_val, 6)
  )

  # Proportion cross-table by row variable
  prop_res <- survey::svyby(
    formula    = stats::as.formula(paste0("~", col_var)),
    by         = stats::as.formula(paste0("~", row_var)),
    design     = design,
    FUN        = survey::svymean,
    na.rm      = TRUE,
    keep.names = FALSE
  )

  list(test = test_tbl, props = tibble::as_tibble(prop_res))
}

.inf_contrasts <- function(design, y_var, group_var,
                           bonferroni = FALSE, conf_level = 0.95) {

  # Group means with joint covariance matrix (required for svycontrast)
  grp_obj <- survey::svyby(
    formula    = stats::as.formula(paste0("~", y_var)),
    by         = stats::as.formula(paste0("~", group_var)),
    design     = design,
    FUN        = survey::svymean,
    na.rm      = TRUE,
    keep.names = FALSE,
    covmat     = TRUE
  )

  grp_labels <- as.character(grp_obj[[group_var]])
  k          <- length(grp_labels)
  mean_col   <- grep(paste0("^", y_var), names(grp_obj), value = TRUE)[1]
  means_vec  <- grp_obj[[mean_col]]
  m_pairs    <- k * (k - 1L) / 2L
  df_val     <- survey::degf(design)
  z_crit     <- stats::qt(1 - (1 - conf_level) / 2, df = df_val)

  # Coef names from svyby are "1", "2", ..., "k"
  cf_names <- names(stats::coef(grp_obj))

  rows <- vector("list", m_pairs)
  idx  <- 1L

  for (i in seq_len(k - 1L)) {
    for (j in seq(i + 1L, k)) {

      cv         <- stats::setNames(rep(0, k), cf_names)
      cv[i]      <-  1
      cv[j]      <- -1

      ctr      <- survey::svycontrast(grp_obj, cv)
      diff_est <- as.numeric(stats::coef(ctr))
      se_val   <- as.numeric(sqrt(stats::vcov(ctr)))
      t_stat   <- diff_est / se_val
      p_raw    <- 2 * stats::pt(-abs(t_stat), df = df_val)
      p_adj    <- if (bonferroni) min(p_raw * m_pairs, 1) else p_raw
      ci       <- diff_est + c(-1, 1) * z_crit * se_val

      rows[[idx]] <- tibble::tibble(
        group_var  = group_var,
        variable   = y_var,
        group_i    = grp_labels[i],
        group_j    = grp_labels[j],
        mean_i     = round(means_vec[i], 4),
        mean_j     = round(means_vec[j], 4),
        diff       = round(diff_est, 4),
        se         = round(se_val, 4),
        t_stat     = round(t_stat, 4),
        df         = round(df_val, 2),
        p_value    = round(p_adj, 6),
        lci        = round(ci[1], 4),
        uci        = round(ci[2], 4),
        sig        = dplyr::case_when(
          p_adj < 0.001 ~ "***",
          p_adj < 0.01  ~ "**",
          p_adj < 0.05  ~ "*",
          p_adj < 0.10  ~ ".",
          TRUE          ~ ""
        )
      )
      idx <- idx + 1L
    }
  }

  dplyr::bind_rows(rows)
}

# Parse coefficient strings entered by the user ("1/3", "-2/3", "0.5", "-0.25").
# Supports plain decimals and simple a/b fractions; returns NA_real_ on failure.
.parse_coef_expr <- function(x) {
  x <- trimws(as.character(x))
  if (nchar(x) == 0L) return(NA_real_)

  # Plain numeric (integer or decimal, optional leading sign)
  v <- suppressWarnings(as.numeric(x))
  if (!is.na(v)) return(v)

  # Simple fraction: [sign] numerator / [sign] denominator
  m <- regmatches(
    x,
    regexpr(
      "^([+-]?[0-9]*\\.?[0-9]+)\\s*/\\s*([+-]?[0-9]*\\.?[0-9]+)$",
      x
    )
  )
  if (length(m) == 1L && nchar(m) > 0L) {
    parts <- strsplit(m, "/", fixed = TRUE)[[1L]]
    num   <- suppressWarnings(as.numeric(parts[1L]))
    den   <- suppressWarnings(as.numeric(parts[2L]))
    if (!is.na(num) && !is.na(den) && den != 0) return(num / den)
  }

  NA_real_
}

.inf_custom_contrasts <- function(design, y_var, group_var,
                                   contrast_df, conf_level = 0.95) {
  # contrast_df: data.frame, first col = label (char), rest = numeric coefficients
  #              one row per contrast, one coef column per group level

  grp_obj <- survey::svyby(
    formula    = stats::as.formula(paste0("~", y_var)),
    by         = stats::as.formula(paste0("~", group_var)),
    design     = design,
    FUN        = survey::svymean,
    na.rm      = TRUE,
    keep.names = FALSE,
    covmat     = TRUE
  )

  grp_labels <- as.character(grp_obj[[group_var]])
  cf_names   <- names(stats::coef(grp_obj))
  df_val     <- survey::degf(design)
  z_crit     <- stats::qt(1 - (1 - conf_level) / 2, df = df_val)

  label_col <- names(contrast_df)[1]
  coef_cols <- names(contrast_df)[-1]

  rows <- vector("list", nrow(contrast_df))

  for (r in seq_len(nrow(contrast_df))) {

    cv_raw <- suppressWarnings(as.numeric(contrast_df[r, coef_cols]))
    cv_raw[is.na(cv_raw)] <- 0
    cv <- stats::setNames(cv_raw, cf_names)

    label <- as.character(contrast_df[r, label_col])
    if (is.na(label) || label == "") label <- paste0("C", r)

    ctr      <- survey::svycontrast(grp_obj, cv)
    diff_est <- as.numeric(stats::coef(ctr))
    se_val   <- as.numeric(sqrt(stats::vcov(ctr)))
    t_stat   <- diff_est / se_val
    p_val    <- 2 * stats::pt(-abs(t_stat), df = df_val)
    ci       <- diff_est + c(-1, 1) * z_crit * se_val

    coef_str <- paste0(
      coef_cols, "=",
      vapply(cv_raw, function(v) {
        if (v == round(v)) as.character(as.integer(v)) else as.character(round(v, 3))
      }, character(1)),
      collapse = ", "
    )

    rows[[r]] <- tibble::tibble(
      contraste    = label,
      group_var    = group_var,
      variable     = y_var,
      coeficientes = coef_str,
      estimado     = round(diff_est, 4),
      se           = round(se_val, 4),
      t_stat       = round(t_stat, 4),
      df           = round(df_val, 2),
      p_value      = round(p_val, 6),
      lci          = round(ci[1], 4),
      uci          = round(ci[2], 4),
      sig          = dplyr::case_when(
        p_val < 0.001 ~ "***",
        p_val < 0.01  ~ "**",
        p_val < 0.05  ~ "*",
        p_val < 0.10  ~ ".",
        TRUE          ~ ""
      )
    )
  }

  dplyr::bind_rows(rows)
}

# Compute weighted Lorenz curve coordinates from a survey design object
.compute_lorenz <- function(design, variable) {
  df <- design$variables
  w  <- as.numeric(weights(design))
  y  <- df[[variable]]

  valid <- !is.na(y) & !is.na(w) & w > 0 & is.finite(y)
  y <- y[valid]
  w <- w[valid]

  ord   <- order(y)
  y     <- y[ord]
  w     <- w[ord]
  cumw  <- cumsum(w)
  cumwy <- cumsum(w * y)

  data.frame(
    p = c(0, cumw  / cumw[length(cumw)]),
    L = c(0, cumwy / cumwy[length(cumwy)])
  )
}

# ---------------------------------------------------------------------------
# Module server
# ---------------------------------------------------------------------------

mod_inferencia_server <- function(id, design, dict) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # --------------------------------------------------
    # 0. Static labels
    # --------------------------------------------------
    output$title <- shiny::renderText({
      i18n_t(dict(), "mod_inferencia.title")
    })
    output$subtitle <- shiny::renderText({
      i18n_t(dict(), "mod_inferencia.subtitle")
    })
    output$params_card <- shiny::renderText({
      i18n_t(dict(), "mod_inferencia.params_card")
    })
    output$run_btn <- shiny::renderText({
      i18n_t(dict(), "mod_inferencia.run_btn")
    })
    output$theory_title <- shiny::renderText({
      i18n_t(dict(), "mod_inferencia.theory_title")
    })
    output$status_title <- shiny::renderText({
      i18n_t(dict(), "mod_inferencia.status_title")
    })
    output$result_title <- shiny::renderText({
      i18n_t(dict(), "mod_inferencia.result_title")
    })
    output$download_title <- shiny::renderText({
      i18n_t(dict(), "mod_inferencia.download_title")
    })
    output$ctr_matrix_title <- shiny::renderText({
      i18n_t(dict(), "mod_inferencia.ctr_matrix_title")
    })
    output$ctr_matrix_hint <- shiny::renderText({
      i18n_t(dict(), "mod_inferencia.ctr_matrix_hint")
    })
    output$ctr_add_lbl <- shiny::renderText({
      i18n_t(dict(), "mod_inferencia.ctr_add")
    })
    output$ctr_del_lbl <- shiny::renderText({
      i18n_t(dict(), "mod_inferencia.ctr_del")
    })
    output$ctr_sum_hint <- shiny::renderText({
      i18n_t(dict(), "mod_inferencia.ctr_sum_hint")
    })

    # --------------------------------------------------
    # 0b. Update input labels when language changes
    # --------------------------------------------------
    shiny::observe({
      d <- dict()
      shiny::updateSelectInput(session, "analysis_type",
        label   = i18n_t(d, "mod_inferencia.analysis_label"),
        choices = stats::setNames(
          c("gini", "correlation", "hyptest", "crosstable", "contrasts"),
          c(i18n_t(d, "mod_inferencia.gini"),
            i18n_t(d, "mod_inferencia.correlation"),
            i18n_t(d, "mod_inferencia.hyptest"),
            i18n_t(d, "mod_inferencia.crosstable"),
            i18n_t(d, "mod_inferencia.contrasts"))
        )
      )
      shiny::updateSelectInput(session, "gini_var",    label = i18n_t(d, "mod_inferencia.gini_var"))
      shiny::updateSelectInput(session, "gini_domain", label = i18n_t(d, "mod_inferencia.domain_opt"))
      shiny::updateSelectInput(session, "cor_x",       label = i18n_t(d, "mod_inferencia.cor_x"))
      shiny::updateSelectInput(session, "cor_y",       label = i18n_t(d, "mod_inferencia.cor_y"))
      shiny::updateSelectInput(session, "hyp_y",       label = i18n_t(d, "mod_inferencia.hyp_y"))
      shiny::updateSelectInput(session, "hyp_group",   label = i18n_t(d, "mod_inferencia.hyp_group"))
      shiny::updateSelectInput(session, "ct_row",      label = i18n_t(d, "mod_inferencia.ct_row"))
      shiny::updateSelectInput(session, "ct_col",       label = i18n_t(d, "mod_inferencia.ct_col"))
      shiny::updateSelectInput(session, "ctr_y",        label = i18n_t(d, "mod_inferencia.ctr_y"))
      shiny::updateSelectInput(session, "ctr_group",    label = i18n_t(d, "mod_inferencia.ctr_group"))
      shiny::updateCheckboxInput(session, "ctr_bonferroni",
        label = i18n_t(d, "mod_inferencia.ctr_bonferroni"))
      shiny::updateRadioButtons(session, "ctr_mode",
        label   = i18n_t(d, "mod_inferencia.ctr_mode"),
        choices = stats::setNames(
          c("all_pairs", "custom"),
          c(i18n_t(d, "mod_inferencia.ctr_mode_all"),
            i18n_t(d, "mod_inferencia.ctr_mode_custom"))
        )
      )
      shiny::updateSelectInput(session, "ct_statistic",
        label   = i18n_t(d, "mod_inferencia.ct_statistic"),
        choices = stats::setNames(
          c("F", "Chisq", "Wald"),
          c(i18n_t(d, "mod_inferencia.stat_F"),
            i18n_t(d, "mod_inferencia.stat_chisq"),
            i18n_t(d, "mod_inferencia.stat_wald"))
        )
      )
    })

    # --------------------------------------------------
    # 1. Theory box
    # --------------------------------------------------
    output$theory_box <- shiny::renderUI({
      shiny::req(input$analysis_type)
      d    <- dict()
      lang <- i18n_t(d, "lang")

      theory_text <- switch(
        input$analysis_type,
        "gini"        = .th_gini(lang),
        "correlation" = .th_correlation(lang),
        "hyptest"     = .th_hyptest(lang),
        "crosstable"  = .th_crosstable(lang),
        "contrasts"   = .th_contrasts(lang),
        ""
      )

      shiny::tagList(
        shiny::withMathJax(),
        shiny::div(
          style = paste0(
            "background-color:#f8f9fa; padding:20px;",
            " border-radius:10px; font-size:14px;"
          ),
          shiny::HTML(theory_text)
        )
      )
    })

    # --------------------------------------------------
    # 2. Populate selectors from design
    # --------------------------------------------------
    shiny::observeEvent(design(), {

      des <- design()
      shiny::req(des, !is.null(des$variables))

      vars     <- names(des$variables)
      num_vars <- vars[vapply(des$variables, is.numeric, logical(1))]
      cat_vars <- vars[!vapply(des$variables, is.numeric, logical(1))]

      d    <- dict()
      none <- stats::setNames("", i18n_t(d, "mod_inferencia.none"))

      shiny::updateSelectInput(session, "gini_var",    choices = num_vars)
      shiny::updateSelectInput(session, "gini_domain", choices = c(none, cat_vars))
      shiny::updateSelectInput(session, "cor_x",       choices = num_vars)
      shiny::updateSelectInput(session, "cor_y",       choices = if (length(num_vars) > 1) num_vars[-1] else num_vars)
      shiny::updateSelectInput(session, "hyp_y",       choices = num_vars)
      shiny::updateSelectInput(session, "hyp_group",   choices = cat_vars)
      shiny::updateSelectInput(session, "ct_row",      choices = cat_vars)
      shiny::updateSelectInput(session, "ct_col",      choices = if (length(cat_vars) > 1) cat_vars[-1] else cat_vars)
      shiny::updateSelectInput(session, "ctr_y",       choices = num_vars)
      shiny::updateSelectInput(session, "ctr_group",   choices = cat_vars)
    })

    # --------------------------------------------------
    # 3. Contrast matrix \u2013 reactive state + DT editor
    # --------------------------------------------------

    # Reactive storage for the editable contrast matrix
    ctr_mat <- shiny::reactiveValues(data = NULL)

    # DT proxy for in-place updates (avoids full re-render on edits)
    ctr_proxy <- DT::dataTableProxy("contrast_matrix", session = session)

    # Helper: build default pairwise matrix for k groups (double columns)
    .build_pairwise_mat <- function(lvls) {
      k    <- length(lvls)
      rows <- list()
      idx  <- 1L
      for (i in seq_len(k - 1L)) {
        for (j in seq(i + 1L, k)) {
          cv    <- rep(0.0, k)
          cv[i] <-  1.0
          cv[j] <- -1.0
          row   <- as.data.frame(
            stats::setNames(
              c(list(label = paste0("C", idx)), as.list(cv)),
              c("label", lvls)
            ),
            stringsAsFactors = FALSE
          )
          rows[[idx]] <- row
          idx <- idx + 1L
        }
      }
      do.call(rbind, rows)
    }

    # Rebuild matrix when group variable changes (or when switching to custom mode)
    shiny::observe({
      shiny::req(input$ctr_group, design())
      des  <- design()
      vars <- des$variables
      grp  <- input$ctr_group
      shiny::req(grp %in% names(vars))
      lvls <- sort(unique(stats::na.omit(as.character(vars[[grp]]))))
      shiny::req(length(lvls) >= 2L)
      ctr_mat$data <- .build_pairwise_mat(lvls)
    })

    # Render editable DT with sum-per-row indicator
    output$contrast_matrix <- DT::renderDT({
      mat <- ctr_mat$data
      shiny::req(mat)

      # Append a read-only "\u03a3" column showing row sum
      coef_cols <- seq_len(ncol(mat) - 1L) + 1L   # R indices of coefficient cols
      row_sums  <- rowSums(mat[, coef_cols, drop = FALSE])
      mat_disp  <- cbind(mat, "\u03a3" = round(row_sums, 6))

      dt <- DT::datatable(
        mat_disp,
        rownames  = FALSE,
        selection = "multiple",
        editable  = list(
          target  = "cell",
          # Make the \u03a3 column (last) non-editable
          disable = list(columns = ncol(mat_disp) - 1L)
        ),
        options = list(
          pageLength = 30,
          dom        = "t",
          scrollX    = TRUE,
          columnDefs = list(
            list(className = "dt-center", targets = "_all")
          )
        )
      )

      # Format numeric columns (all except label col 0 and \u03a3 col last)
      num_js_cols <- seq_len(ncol(mat) - 1L)  # 0-indexed JS cols: 1..(k)
      dt <- DT::formatRound(dt, columns = num_js_cols, digits = 6)

      # Color \u03a3 column: green if \u2248 0, orange otherwise
      dt <- DT::formatStyle(
        dt, "\u03a3",
        color = DT::styleInterval(
          c(-1e-9, 1e-9),
          c("#E6A817", "#00843D", "#E6A817")
        ),
        fontWeight = "bold"
      )
      dt
    })

    # Apply cell edits \u2014 supports arithmetic expressions (e.g. "1/3", "-2/3")
    shiny::observeEvent(input$contrast_matrix_cell_edit, {
      info <- input$contrast_matrix_cell_edit
      mat  <- ctr_mat$data
      r    <- info$row
      # DT: 0-indexed col, rownames=FALSE \u2192 col 0 is R col 1
      c_r  <- info$col + 1L

      if (c_r == 1L) {
        # Label column: plain character
        mat[r, 1L] <- as.character(info$value)
      } else {
        # Coefficient column: evaluate arithmetic expression
        parsed <- .parse_coef_expr(info$value)
        if (is.na(parsed)) {
          shiny::showNotification(
            paste0("\u00ab", info$value,
                   "\u00bb \u2014 ",
                   i18n_t(dict(), "mod_inferencia.err_coef_expr")),
            type = "warning", duration = 4
          )
          # Restore old value in DT without changing the store
          DT::replaceData(ctr_proxy, mat, resetPaging = FALSE, rownames = FALSE)
          return()
        }
        mat[r, c_r] <- parsed
      }

      ctr_mat$data <- mat
      DT::replaceData(ctr_proxy, mat, resetPaging = FALSE, rownames = FALSE)
    })

    # Add a new blank contrast row
    shiny::observeEvent(input$ctr_add, {
      mat <- ctr_mat$data
      shiny::req(mat)
      k       <- ncol(mat) - 1L
      new_row <- as.data.frame(
        stats::setNames(
          c(list(label = paste0("C", nrow(mat) + 1L)),
            as.list(rep(0.0, k))),
          names(mat)
        ),
        stringsAsFactors = FALSE
      )
      mat <- rbind(mat, new_row)
      ctr_mat$data <- mat
      DT::replaceData(ctr_proxy, mat, resetPaging = FALSE, rownames = FALSE)
    })

    # Delete selected rows
    shiny::observeEvent(input$ctr_del, {
      sel <- input$contrast_matrix_rows_selected
      mat <- ctr_mat$data
      shiny::req(mat, !is.null(sel), length(sel) > 0L)
      mat <- mat[-sel, , drop = FALSE]
      # Re-label to keep sequence tidy
      mat[["label"]] <- paste0("C", seq_len(nrow(mat)))
      ctr_mat$data <- mat
      DT::replaceData(ctr_proxy, mat, resetPaging = FALSE, rownames = FALSE)
    })

    # --------------------------------------------------
    # 3b. Execute analysis
    # --------------------------------------------------
    results_r <- shiny::eventReactive(input$run, {

      des <- design()
      shiny::req(des, input$analysis_type)

      type <- input$analysis_type

      tryCatch({

        if (type == "gini") {
          shiny::req(input$gini_var)
          domain <- input$gini_domain
          if (is.null(domain) || domain == "") domain <- NULL
          list(
            type   = "gini",
            domain = domain,
            result = .inf_gini(des, input$gini_var, domain)
          )

        } else if (type == "correlation") {
          shiny::req(input$cor_x, input$cor_y)
          if (input$cor_x == input$cor_y) {
            shiny::showNotification(
              i18n_t(dict(), "mod_inferencia.err_same_var"),
              type = "error"
            )
            return(NULL)
          }
          list(
            type   = "correlation",
            result = .inf_correlation(des, input$cor_x, input$cor_y)
          )

        } else if (type == "hyptest") {
          shiny::req(input$hyp_y, input$hyp_group)
          list(
            type   = "hyptest",
            result = .inf_hyptest(des, input$hyp_y, input$hyp_group)
          )

        } else if (type == "crosstable") {
          shiny::req(input$ct_row, input$ct_col)
          if (input$ct_row == input$ct_col) {
            shiny::showNotification(
              i18n_t(dict(), "mod_inferencia.err_same_var"),
              type = "error"
            )
            return(NULL)
          }
          list(
            type   = "crosstable",
            result = .inf_crosstable(des, input$ct_row, input$ct_col, input$ct_statistic)
          )

        } else if (type == "contrasts") {
          shiny::req(input$ctr_y, input$ctr_group)

          if (isTRUE(input$ctr_mode == "custom")) {
            mat <- ctr_mat$data
            if (is.null(mat) || nrow(mat) == 0L) {
              shiny::showNotification(
                i18n_t(dict(), "mod_inferencia.err_no_contrasts"),
                type = "error"
              )
              return(NULL)
            }
            list(
              type   = "contrasts",
              mode   = "custom",
              result = .inf_custom_contrasts(des, input$ctr_y, input$ctr_group, mat)
            )
          } else {
            list(
              type   = "contrasts",
              mode   = "all_pairs",
              result = .inf_contrasts(
                des, input$ctr_y, input$ctr_group,
                bonferroni = isTRUE(input$ctr_bonferroni)
              )
            )
          }
        }

      }, error = function(e) {
        shiny::showNotification(
          paste0(i18n_t(dict(), "mod_inferencia.err_compute"), "\n", conditionMessage(e)),
          type = "error", duration = 8
        )
        NULL
      })

    }, ignoreInit = TRUE)

    # --------------------------------------------------
    # 4. Status log
    # --------------------------------------------------
    output$log <- shiny::renderPrint({
      r <- results_r()
      if (is.null(r)) {
        return(i18n_t(dict(), "mod_inferencia.not_run"))
      }
      d    <- dict()
      type <- r$type
      base <- switch(type,
        "gini"        = i18n_t(d, "mod_inferencia.gini"),
        "correlation" = i18n_t(d, "mod_inferencia.correlation"),
        "hyptest"     = i18n_t(d, "mod_inferencia.hyptest"),
        "crosstable"  = i18n_t(d, "mod_inferencia.crosstable"),
        "contrasts"   = i18n_t(d, "mod_inferencia.contrasts")
      )
      analysis_lbl <- if (type == "contrasts" && !is.null(r$mode)) {
        mode_lbl <- if (r$mode == "custom") {
          i18n_t(d, "mod_inferencia.ctr_mode_custom")
        } else {
          i18n_t(d, "mod_inferencia.ctr_mode_all")
        }
        paste0(base, " [", mode_lbl, "]")
      } else {
        base
      }
      list(
        analysis = analysis_lbl,
        status   = i18n_t(d, "mod_inferencia.status_ok")
      )
    })

    # --------------------------------------------------
    # 5. Dynamic results UI
    # --------------------------------------------------
    output$results_ui <- shiny::renderUI({

      r <- results_r()
      shiny::req(r)

      if (r$type == "crosstable") {
        shiny::tagList(
          shiny::tags$b(i18n_t(dict(), "mod_inferencia.ct_test_title")),
          DT::DTOutput(ns("dt_ct_test")),
          shiny::br(),
          shiny::tags$b(i18n_t(dict(), "mod_inferencia.ct_props_title")),
          DT::DTOutput(ns("dt_ct_props"))
        )
      } else if (r$type == "contrasts") {
        mode_tag <- if (!is.null(r$mode) && r$mode == "custom") {
          i18n_t(dict(), "mod_inferencia.ctr_mode_custom")
        } else {
          i18n_t(dict(), "mod_inferencia.ctr_mode_all")
        }
        shiny::tagList(
          shiny::tags$p(
            shiny::tags$b(i18n_t(dict(), "mod_inferencia.ctr_pairs_title")),
            shiny::tags$span(
              style = "margin-left:8px; color:#7B5EA7; font-size:13px;",
              paste0(nrow(r$result), " ", i18n_t(dict(), "mod_inferencia.ctr_pairs_count"),
                     " \u2022 ", mode_tag)
            )
          ),
          DT::DTOutput(ns("dt_main"))
        )
      } else {
        DT::DTOutput(ns("dt_main"))
      }
    })

    output$dt_main <- DT::renderDT({
      r <- results_r()
      shiny::req(r, r$type != "crosstable")
      res_fmt <- r$result %>%
        dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4)))

      dt <- DT::datatable(res_fmt, rownames = FALSE,
        options = list(pageLength = 20, scrollX = TRUE))

      # Highlight significance column in contrasts
      if (r$type == "contrasts" && "sig" %in% names(res_fmt)) {
        dt <- DT::formatStyle(
          dt, "sig",
          color = DT::styleEqual(
            c("***", "**", "*", ".", ""),
            c("#00843D", "#2A9D8F", "#E6A817", "#888888", "#BBBBBB")
          ),
          fontWeight = DT::styleEqual(
            c("***", "**", "*", ".", ""),
            c("bold", "bold", "bold", "normal", "normal")
          )
        )
      }
      dt
    })

    output$dt_ct_test <- DT::renderDT({
      r <- results_r()
      shiny::req(r, r$type == "crosstable")
      DT::datatable(r$result$test, rownames = FALSE,
        options = list(pageLength = 5, scrollX = TRUE, dom = "t"))
    })

    output$dt_ct_props <- DT::renderDT({
      r <- results_r()
      shiny::req(r, r$type == "crosstable")
      res_fmt <- r$result$props %>%
        dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4)))
      DT::datatable(res_fmt, rownames = FALSE,
        options = list(pageLength = 20, scrollX = TRUE))
    })

    # --------------------------------------------------
    # 6. Plot
    # --------------------------------------------------
    output$plot_section <- shiny::renderText({
      i18n_t(dict(), "mod_inferencia.plot_section")
    })

    output$analysis_plot <- shiny::renderPlot({
      r   <- results_r()
      shiny::req(r)
      des  <- design()
      lang <- i18n_t(dict(), "lang")

      COL  <- "#7B5EA7"
      FILL <- "#E8E0F5"

      if (r$type == "gini") {
        var_name   <- r$result$variable[1]
        domain_val <- r$domain
        ttl  <- if (lang == "en") "Lorenz Curve" else "Curva de Lorenz"
        xlab <- if (lang == "en") "Cumulative share of units" else "Proporci\u00f3n acumulada de unidades"
        ylab <- if (lang == "en") "Cumulative share"          else "Proporci\u00f3n acumulada"

        if (!is.null(domain_val)) {
          des_vars <- des$variables
          lvls <- sort(unique(stats::na.omit(as.character(des_vars[[domain_val]]))))
          lorenz_list <- lapply(lvls, function(lvl) {
            idx     <- !is.na(des_vars[[domain_val]]) & des_vars[[domain_val]] == lvl
            sub_des <- subset(des, idx)
            lrz     <- .compute_lorenz(sub_des, var_name)
            lrz$domain <- lvl
            lrz
          })
          lorenz_all <- do.call(rbind, lorenz_list)
          res   <- r$result
          g_ann <- paste0(res$domain_val, ": G=", round(res$gini, 4), collapse = "\n")

          ggplot2::ggplot(lorenz_all,
            ggplot2::aes(x = .data[["p"]], y = .data[["L"]],
                         color = .data[["domain"]])) +
            ggplot2::geom_line(linewidth = 1.1) +
            ggplot2::geom_abline(intercept = 0, slope = 1,
                                 linetype = "dashed", color = "gray50") +
            ggplot2::annotate("label", x = 0.68, y = 0.12, label = g_ann,
                              size = 3.5, fill = "white", color = "gray30",
                              hjust = 0, label.padding = ggplot2::unit(0.4, "lines")) +
            ggplot2::scale_x_continuous(labels = function(x) paste0(round(x * 100), "%")) +
            ggplot2::scale_y_continuous(labels = function(x) paste0(round(x * 100), "%")) +
            ggplot2::labs(title = paste0(ttl, " \u2014 ", domain_val),
                          x = xlab, y = ylab, color = domain_val) +
            ggplot2::theme_minimal(base_size = 13) +
            ggplot2::theme(
              plot.title      = ggplot2::element_text(face = "bold", color = COL),
              legend.position = "right"
            )

        } else {
          lorenz <- .compute_lorenz(des, var_name)
          g_val  <- r$result$gini[1]
          g_lci  <- r$result$lci[1]
          g_uci  <- r$result$uci[1]
          ann    <- sprintf("G = %.4f\n[%.4f; %.4f]", g_val, g_lci, g_uci)

          ggplot2::ggplot(lorenz, ggplot2::aes(x = .data[["p"]], y = .data[["L"]])) +
            ggplot2::geom_ribbon(ggplot2::aes(ymin = .data[["L"]], ymax = .data[["p"]]),
                                 fill = COL, alpha = 0.12) +
            ggplot2::geom_line(color = COL, linewidth = 1.3) +
            ggplot2::geom_abline(intercept = 0, slope = 1,
                                 linetype = "dashed", color = "gray50") +
            ggplot2::annotate("label", x = 0.70, y = 0.18, label = ann,
                              color = COL, size = 4.5, fontface = "bold", fill = "white",
                              label.padding = ggplot2::unit(0.45, "lines")) +
            ggplot2::scale_x_continuous(labels = function(x) paste0(round(x * 100), "%")) +
            ggplot2::scale_y_continuous(labels = function(x) paste0(round(x * 100), "%")) +
            ggplot2::labs(title = ttl, x = xlab, y = ylab) +
            ggplot2::theme_minimal(base_size = 13) +
            ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", color = COL))
        }

      } else if (r$type == "correlation") {
        res  <- r$result
        xv   <- res$x_var[1]
        yv   <- res$y_var[1]
        df_p <- des$variables[c(xv, yv)]
        df_p <- df_p[stats::complete.cases(df_p), ]
        if (nrow(df_p) > 5000) df_p <- df_p[sample.int(nrow(df_p), 5000), ]
        ttl <- if (lang == "en") {
          sprintf("Correlation: r = %.4f  (p = %.4f)", res$correlation[1], res$p_value[1])
        } else {
          sprintf("Correlaci\u00f3n: r = %.4f  (p = %.4f)", res$correlation[1], res$p_value[1])
        }
        ggplot2::ggplot(df_p, ggplot2::aes(x = .data[[xv]], y = .data[[yv]])) +
          ggplot2::geom_point(alpha = 0.25, color = COL, size = 1.5) +
          ggplot2::geom_smooth(method = "lm", formula = y ~ x,
                               se = TRUE, color = COL, fill = FILL, alpha = 0.5) +
          ggplot2::labs(title = ttl, x = xv, y = yv) +
          ggplot2::theme_minimal(base_size = 13) +
          ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", color = COL))

      } else if (r$type == "hyptest") {
        res    <- r$result
        groups <- c(as.character(res$group_1), as.character(res$group_2))
        means  <- c(res$mean_1, res$mean_2)
        ses    <- c(res$se_1, res$se_2)
        plot_df <- data.frame(
          group = factor(groups, levels = groups),
          mean  = means,
          lci   = means - stats::qnorm(0.975) * ses,
          uci   = means + stats::qnorm(0.975) * ses
        )
        plab <- if (lang == "en") "Group" else "Grupo"
        mlab <- if (lang == "en") "Mean"  else "Media"
        ttl  <- if (lang == "en") {
          sprintf("Group means \u00b7 p = %.4f", res$p_value)
        } else {
          sprintf("Medias por grupo \u00b7 p = %.4f", res$p_value)
        }
        ggplot2::ggplot(plot_df,
                        ggplot2::aes(x = .data[["group"]], y = .data[["mean"]])) +
          ggplot2::geom_col(fill = FILL, color = COL, width = 0.5, linewidth = 0.8) +
          ggplot2::geom_errorbar(
            ggplot2::aes(ymin = .data[["lci"]], ymax = .data[["uci"]]),
            width = 0.12, color = COL, linewidth = 0.9
          ) +
          ggplot2::geom_text(
            ggplot2::aes(label = round(.data[["mean"]], 3)),
            vjust = -1.8, color = COL, size = 4.5, fontface = "bold"
          ) +
          ggplot2::labs(title = ttl, x = plab, y = mlab) +
          ggplot2::theme_minimal(base_size = 13) +
          ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", color = COL))

      } else if (r$type == "crosstable") {
        props    <- r$result$props
        row_name <- r$result$test$row_var[1]
        col_name <- r$result$test$col_var[1]
        prop_cols <- names(props)[
          !names(props) %in% row_name & !grepl("^se\\.", names(props))
        ]
        if (length(prop_cols) == 0) return(ggplot2::ggplot() + ggplot2::theme_void())
        long_df <- tidyr::pivot_longer(
          props,
          cols      = tidyselect::all_of(prop_cols),
          names_to  = "cat",
          values_to = "prop"
        )
        long_df$cat <- sub(paste0("^", col_name), "", long_df$cat)
        ttl  <- if (lang == "en") "Row proportions" else "Proporciones por fila"
        plab <- if (lang == "en") "Proportion"      else "Proporci\u00f3n"
        ggplot2::ggplot(long_df,
          ggplot2::aes(x = .data[[row_name]], y = .data[["prop"]],
                       fill = .data[["cat"]])) +
          ggplot2::geom_col(position = "dodge", color = "white", width = 0.7) +
          ggplot2::scale_fill_viridis_d(option = "mako",
                                        begin = 0.2, end = 0.8, alpha = 0.85) +
          ggplot2::scale_y_continuous(
            labels = function(x) paste0(round(x * 100, 1), "%")) +
          ggplot2::labs(title = ttl, x = row_name, y = plab, fill = col_name) +
          ggplot2::theme_minimal(base_size = 13) +
          ggplot2::theme(
            plot.title      = ggplot2::element_text(face = "bold", color = COL),
            legend.position = "bottom"
          )

      } else if (r$type == "contrasts") {
        res <- r$result
        if ("group_i" %in% names(res)) {
          plot_df <- dplyr::mutate(res,
            comparison = paste0(.data[["group_i"]], " \u2212 ", .data[["group_j"]]),
            estimate   = .data[["diff"]]
          )
        } else {
          plot_df <- dplyr::mutate(res,
            comparison = .data[["contraste"]],
            estimate   = .data[["estimado"]]
          )
        }
        plot_df$comparison <- factor(plot_df$comparison,
                                     levels = rev(unique(plot_df$comparison)))
        plot_df$sig_cat    <- ifelse(plot_df$p_value < 0.05, "sig", "ns")
        ttl  <- if (lang == "en") "Contrasts (95% CI)" else "Contrastes (IC 95%)"
        dlab <- if (lang == "en") "Difference"         else "Diferencia"
        ggplot2::ggplot(plot_df,
          ggplot2::aes(x = .data[["estimate"]], y = .data[["comparison"]],
                       color = .data[["sig_cat"]])) +
          ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
          ggplot2::geom_errorbarh(
            ggplot2::aes(xmin = .data[["lci"]], xmax = .data[["uci"]]),
            height = 0.25, linewidth = 0.8
          ) +
          ggplot2::geom_point(size = 3.5) +
          ggplot2::geom_text(
            ggplot2::aes(label = paste0(round(.data[["estimate"]], 3),
                                         " ", .data[["sig"]])),
            hjust = -0.15, size = 3.5, color = "gray30"
          ) +
          ggplot2::scale_color_manual(
            values = c("sig" = COL, "ns" = "gray60"),
            guide  = "none"
          ) +
          ggplot2::labs(title = ttl, x = dlab, y = NULL) +
          ggplot2::theme_minimal(base_size = 13) +
          ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", color = COL))
      }
    })

    # --------------------------------------------------
    # 7. Download handlers
    # --------------------------------------------------

    # Helper: get flat tibble for download
    .get_flat_result <- function() {
      r <- results_r()
      shiny::req(r)
      if (r$type == "crosstable") {
        dplyr::bind_rows(
          dplyr::mutate(r$result$test, .section = "test"),
          dplyr::mutate(r$result$props, .section = "props")
        )
      } else {
        r$result
      }
    }

    output$dl_csv <- shiny::downloadHandler(
      filename = function() {
        paste0("inferencia_", results_r()$type, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        readr::write_csv(.get_flat_result(), file)
      }
    )

    output$dl_xlsx <- shiny::downloadHandler(
      filename = function() {
        paste0("inferencia_", results_r()$type, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
      },
      content = function(file) {
        r <- results_r()
        shiny::req(r)
        if (r$type == "crosstable") {
          writexl::write_xlsx(
            list(test = r$result$test, proporciones = r$result$props),
            file
          )
        } else {
          writexl::write_xlsx(r$result, file)
        }
      }
    )

    output$dl_rds <- shiny::downloadHandler(
      filename = function() {
        paste0("inferencia_", results_r()$type, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
      },
      content = function(file) {
        saveRDS(results_r()$result, file)
      }
    )

    # --------------------------------------------------
    # 8. Return results for mod_tablas integration
    # --------------------------------------------------
    list(
      results = shiny::reactive({
        r <- results_r()
        if (is.null(r)) return(NULL)
        if (r$type == "crosstable") r$result$test else r$result
      }),
      meta = shiny::reactive({
        r <- results_r()
        if (is.null(r)) return(NULL)
        d    <- dict()
        type <- r$type

        variable_label <- switch(type,
          "gini"        = input$gini_var,
          "correlation" = paste0(input$cor_x, " \u2194 ", input$cor_y),
          "hyptest"     = paste0(input$hyp_y, " ~ ", input$hyp_group),
          "crosstable"  = paste0(input$ct_row, " \u00d7 ", input$ct_col),
          "contrasts"   = {
            mode_sfx <- if (!is.null(r$mode) && r$mode == "custom") {
              paste0(" [", i18n_t(d, "mod_inferencia.ctr_mode_custom"), "]")
            } else ""
            paste0(input$ctr_y, " ~ ", input$ctr_group, mode_sfx)
          }
        )

        estimator_name <- switch(type,
          "gini"        = i18n_t(d, "mod_inferencia.gini"),
          "correlation" = i18n_t(d, "mod_inferencia.correlation"),
          "hyptest"     = i18n_t(d, "mod_inferencia.hyptest"),
          "crosstable"  = i18n_t(d, "mod_inferencia.crosstable"),
          "contrasts"   = i18n_t(d, "mod_inferencia.contrasts")
        )

        list(
          estimator = paste0(i18n_t(d, "mod_inferencia.meta_prefix"), " ", estimator_name),
          variable  = variable_label,
          domains   = character(0),
          timestamp = Sys.time()
        )
      })
    )
  })
}
