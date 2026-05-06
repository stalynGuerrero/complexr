# complexr <img src="inst/img/complexr_hex.png" align="right" width="120"/>

![Version](https://img.shields.io/badge/version-1.0.0-blue)
![R](https://img.shields.io/badge/R-%3E%3D%204.1.0-276DC3)
![License](https://img.shields.io/badge/license-GPL--3-green)
![Language](https://img.shields.io/badge/lang-ES%20%7C%20EN-orange)

---

<!-- ESPAÑOL ----------------------------------------------------------------->
## Español

**complexr** es una aplicación web desarrollada en `R Shiny` para el
análisis estadístico de encuestas de muestreo complejo. Permite definir diseños
muestrales, derivar variables, estimar indicadores con corrección por diseño y
visualizar resultados con medidas de incertidumbre, todo sin necesidad de
escribir código.

Está orientada a investigadores, estadísticos y analistas que trabajan con datos
provenientes de encuestas probabilísticas (encuestas nacionales, censos,
estudios epidemiológicos) y requieren estimaciones rigurosas que respeten la
estructura del diseño muestral.

### Módulos principales

#### Datos
- Carga de archivos en formatos **CSV**, **TSV/TXT**, **XLSX**, **XLS**, **SAV** (SPSS), **POR** (SPSS portable), **DTA** (Stata), **SAS7BDAT**, **XPT** (SAS transport) y **RDS**.
- Conjunto de datos de ejemplo integrado para exploración inmediata.
- Vista previa tabular interactiva con resumen de dimensiones y variables.

#### Diseño muestral
- Soporte para tres tipos de diseño:
  - **Simple aleatorio (SRS)**
  - **Estratificado**
  - **Conglomerados / Multietápico**
- Configuración de variables de estrato, conglomerado, pesos de expansión y
  corrección de población finita (FPC).
- Generación automática del código R equivalente (`svydesign`).
- Diagnóstico del diseño construido.

#### Variables derivadas *(nuevo)*
- Creación de nuevas variables sobre el diseño activo.
- **Modo básico**: operaciones aritméticas, funciones matemáticas, indicadores
  binarios, recodificaciones y cortes por intervalos.
- **Modo experto**: expresiones R arbitrarias de múltiples líneas.
- Previsualización del código generado antes de aplicar cambios.
- Gestión de variables derivadas (eliminar individualmente o en bloque).

#### Estimación
- Estimadores disponibles: **media**, **total**, **proporción**, **razón** y
  **cuantiles**.
- Desglose por **dominios** (una o varias variables de clasificación).
- Marco teórico con formulación matemática (renderizado con MathJax).
- Tabla de resultados con error estándar, intervalo de confianza y coeficiente
  de variación (CV).
- Indicadores de calidad para evaluar la confiabilidad de las estimaciones.

#### Tablas de resultados *(nuevo)*
- Gestión centralizada de todas las estimaciones generadas en la sesión.
- Exportación de resultados en **CSV**, **Excel (.xlsx)**, **RDS**, **JSON** y
  **TXT (tab)**.
- Vista por pestañas con contador de tablas activas.
- Limpieza selectiva o total de resultados almacenados.

### Interfaz

La aplicación cuenta con una interfaz de estilo empresarial diseñada para
entornos profesionales:

- **Barra de navegación** con iconos FontAwesome por módulo.
- **Cards** con cabecera, separación visual y borde de acento en paneles laterales.
- **Cabeceras de página** con ícono descriptivo y subtítulo contextual.
- **Salida de código** en estilo terminal (fondo oscuro, tipografía monoespaciada).
- **Soporte multiidioma** (Español / English) desde la barra superior.
- Tipografía **Inter** con jerarquía tipográfica clara.
- Paleta institucional CEPAL / MSPS.

### Requisitos e instalación

`complexr` requiere **R >= 4.1.0**. Las dependencias incluyen:
`shiny`, `DT`, `srvyr`, `survey`, `dplyr`, `tibble`, `tidyr`, `readr`,
`readxl`, `haven`, `ggplot2`, `jsonlite`, `writexl`, entre otras.

La forma más sencilla de instalar `complexr` directamente desde GitHub es:

```r
# Opción 1 — con remotes
install.packages("remotes")
remotes::install_github("stalynGuerrero/complexr")

# Opción 2 — con pak
install.packages("pak")
pak::pak("stalynGuerrero/complexr")
```

### Ejecutar la aplicación

```r
complexr::run_app()
```

### Estructura del paquete

```
R/
├── app_ui.R               # UI raíz (navbarPage + tema)
├── app_server.R           # Servidor raíz (i18n + orquestación)
├── mod_datos_*            # Módulo de carga de datos
├── mod_diseno_*           # Módulo de diseño muestral
├── mod_variables_*        # Módulo de variables derivadas
├── mod_estimacion_*       # Módulo de estimación
├── mod_tablas_*           # Módulo de tablas de resultados
├── design.R               # Lógica de construcción del diseño
├── estimate_survey.R      # Estimadores con corrección por diseño
├── generate_example_data.R
└── i18n.R                 # Internacionalización
inst/
├── app/www/custom.css     # Tema visual enterprise
└── app/www/i18n/          # Diccionarios ES / EN
```

### Autor

**Stalyn Guerrero Gómez** — [guerrerostalyn@gmail.com](mailto:guerrerostalyn@gmail.com)  
Licencia: GPL-3

---

<!-- ENGLISH ----------------------------------------------------------------->
## English

**complexr** is a web application built with `R Shiny` for the
statistical analysis of complex survey data. It enables users to define sampling
designs, derive new variables, estimate indicators with design-based corrections,
and visualize results with uncertainty measures — all without writing code.

It is aimed at researchers, statisticians, and analysts working with data from
probability surveys (national surveys, censuses, epidemiological studies) who
require rigorous estimates that respect the underlying sampling design structure.

### Main modules

#### Data
- File upload in **CSV**, **TSV/TXT**, **XLSX**, **XLS**, **SAV** (SPSS), **POR** (SPSS portable), **DTA** (Stata), **SAS7BDAT**, **XPT** (SAS transport), and **RDS** formats.
- Built-in example dataset for immediate exploration.
- Interactive tabular preview with dimension and variable summary.

#### Sampling design
- Support for three design types:
  - **Simple random sampling (SRS)**
  - **Stratified**
  - **Cluster / Multi-stage**
- Configuration of strata, cluster, expansion weight variables, and finite
  population correction (FPC).
- Automatic generation of the equivalent R code (`svydesign`).
- Built-in design diagnostics.

#### Derived variables *(new)*
- Create new variables on top of the active survey design.
- **Basic mode**: arithmetic operations, math functions, binary indicators,
  recodings, and interval cuts.
- **Expert mode**: arbitrary multi-line R expressions.
- Preview of generated code before applying changes.
- Variable management (delete individually or all at once).

#### Estimation
- Available estimators: **mean**, **total**, **proportion**, **ratio**, and
  **quantiles**.
- Breakdown by **domains** (one or more classification variables).
- Theoretical framework with mathematical formulation (rendered with MathJax).
- Results table with standard error, confidence interval, and coefficient of
  variation (CV).
- Quality indicators to assess estimation reliability.

#### Results tables *(new)*
- Centralized management of all estimates generated in the session.
- Export results as **CSV**, **Excel (.xlsx)**, **RDS**, **JSON**, or
  **TXT (tab-delimited)**.
- Tab-based view with active table counter.
- Selective or bulk clearing of stored results.

### Interface

The application features an enterprise-style interface designed for professional
environments:

- **Navigation bar** with FontAwesome icons per module.
- **Cards** with headers, visual separation, and accent borders on side panels.
- **Page headers** with descriptive icon and contextual subtitle.
- **Code output** in terminal style (dark background, monospace font).
- **Multilingual support** (Spanish / English) from the top bar.
- **Inter** typeface with a clear typographic hierarchy.
- Institutional CEPAL / MSPS color palette.

### Requirements and installation

`complexr` requires **R >= 4.1.0**. Dependencies include:
`shiny`, `DT`, `srvyr`, `survey`, `dplyr`, `tibble`, `tidyr`, `readr`,
`readxl`, `haven`, `ggplot2`, `jsonlite`, `writexl`, among others.

The easiest way to install `complexr` directly from GitHub is:

```r
# Option 1 — with remotes
install.packages("remotes")
remotes::install_github("stalynGuerrero/complexr")

# Option 2 — with pak
install.packages("pak")
pak::pak("stalynGuerrero/complexr")
```

### Run the application

```r
complexr::run_app()
```

### Package structure

```
R/
├── app_ui.R               # Root UI (navbarPage + theme)
├── app_server.R           # Root server (i18n + orchestration)
├── mod_datos_*            # Data loading module
├── mod_diseno_*           # Sampling design module
├── mod_variables_*        # Derived variables module
├── mod_estimacion_*       # Estimation module
├── mod_tablas_*           # Results tables module
├── design.R               # Design construction logic
├── estimate_survey.R      # Design-based estimators
├── generate_example_data.R
└── i18n.R                 # Internationalisation
inst/
├── app/www/custom.css     # Enterprise visual theme
└── app/www/i18n/          # ES / EN dictionaries
```

### Author

**Stalyn Guerrero Gómez** — [guerrerostalyn@gmail.com](mailto:guerrerostalyn@gmail.com)  
License: GPL-3
