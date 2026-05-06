#' Shiny UI for complexr
#'
#' @return A Shiny UI object.
app_ui <- function() {
  
  shiny::withMathJax(
    
    shiny::navbarPage(
      title = shiny::tagList(
        shiny::tags$img(
          src    = "img/complexr_hex.png",
          height = "36px",
          alt    = "complexr",
          class  = "navbar-hex-logo"
        ),
        shiny::tags$span("complexr")
      ),
      id    = "navbar",
      fluid = TRUE,
      theme = NULL,
      
      # ============================================================
      # HEAD
      # ============================================================
      header = shiny::tags$head(
        
        shiny::tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
        shiny::tags$link(
          rel  = "stylesheet",
          href = "https://fonts.googleapis.com/css2?family=Syne:wght@600;700;800&family=DM+Sans:opsz,wght@9..40,300;9..40,400;9..40,500&family=JetBrains+Mono:wght@400;500&display=swap"
        ),
        shiny::tags$link(
          rel  = "stylesheet",
          href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.1/css/all.min.css"
        ),
        
        # Hojas de estilo del proyecto
        shiny::tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
        
        # Overrides de navbar para usar fuente Syne en la marca
        shiny::tags$style(shiny::HTML("
          .navbar-brand span,
          .navbar-header .navbar-brand span {
            font-family: 'Syne', sans-serif !important;
            font-weight: 700 !important;
            font-size: 15px !important;
            letter-spacing: -0.01em !important;
          }

          /* Selector de idioma */
          .navbar-custom-menu {
            float: right !important;
            margin-right: 14px;
            display: flex;
            align-items: center;
            height: 54px;
            gap: 8px;
          }
          .navbar-custom-menu .lang-label {
            color: rgba(139,188,208,0.65);
            font-size: 11px;
            letter-spacing: .10em;
            text-transform: uppercase;
            font-weight: 600;
          }
          .navbar-custom-menu .form-group { margin-bottom: 0 !important; }
          .navbar-custom-menu .fa-globe { color: #33B7E8 !important; }
        ")),
        
        # Mover el contenedor de idioma al extremo derecho de la navbar
        shiny::tags$script(shiny::HTML("
          $(document).ready(function() {
            $('.navbar-nav').after($('#lang-container'));
          });
        "))
      ),
      
      # ============================================================
      # PESTAÑA 0 — Portada Premium
      # ============================================================
      shiny::tabPanel(
        title = shiny::tagList(
          shiny::tags$i(class = "fa fa-home"),
          shiny::textOutput("tab_portada", inline = TRUE)
        ),
        value = "portada",
        
        shiny::div(
          class = "cover-wrapper",
          
          shiny::div(
            class = "cover-container",
            
            # ── TOP BAR ──────────────────────────────────────────
            shiny::div(
              class = "cover-topbar",
              shiny::div(
                class = "cover-logo",
                shiny::div(
                  class = "cover-logo-mark",
                  shiny::tags$i(class = "fa-solid fa-chart-column")
                ),
                shiny::tags$span("complexr")
              ),
              shiny::div(
                class = "cover-pill",
                shiny::div(class = "dot"),
                shiny::textOutput("cover_app_active", inline = TRUE)
              )
            ),
            
            # ── HERO ─────────────────────────────────────────────
            shiny::div(
              class = "cover-hero",
              
              # Columna izquierda
              shiny::div(
                class = "cover-hero-left",
                
                shiny::div(
                  class = "cover-eyebrow",
                  shiny::tags$span("R \u00b7 survey \u00b7 i18n")
                ),
                
                shiny::tags$h1(
                  class = "cover-title",
                  "Encuestas complejas,",
                  shiny::tags$br(),
                  shiny::tags$span(class = "accent", "resultados claros.")
                ),
                
                shiny::tags$p(
                  class = "cover-subtitle",
                  shiny::textOutput("cover_desc", inline = TRUE)
                ),
                
                # Botones CTA
                shiny::div(
                  class = "cover-btn-group",
                  shiny::actionButton(
                    inputId = "cover_btn_start",
                    label   = shiny::tagList(
                      shiny::tags$i(class = "fa-solid fa-rocket"),
                      shiny::textOutput("cover_btn", inline = TRUE),
                      shiny::tags$span(shiny::HTML("&thinsp;&rarr;"),
                                       style = "font-weight:300; font-size:16px;")
                    ),
                    class = "cover-btn cover-btn-primary"
                  ),
                  shiny::tags$a(
                    href   = "https://github.com/stalynGuerrero/complexr",
                    target = "_blank",
                    class  = "cover-btn cover-btn-ghost",
                    shiny::tags$i(class = "fa-brands fa-github"),
                    shiny::tags$span("GitHub")
                  )
                ),
                
                # Badges de tecnología
                shiny::div(
                  class = "cover-badges",
                  shiny::tags$span(class = "cover-badge", "CSV \u00b7 SAV \u00b7 DTA \u00b7 XLSX \u00b7 +"),
                  shiny::tags$span(class = "cover-badge", "survey"),
                  shiny::tags$span(class = "cover-badge", "i18n")
                ),
                
                # Stats
                shiny::div(
                  class = "cover-stats",
                  shiny::div(
                    shiny::div(class = "cover-stat-num", "3+"),
                    shiny::div(class = "cover-stat-lbl", "Tipos de dise\u00f1o")
                  ),
                  shiny::div(
                    shiny::div(class = "cover-stat-num", "10+"),
                    shiny::div(class = "cover-stat-lbl", "Estimadores con IC")
                  ),
                  shiny::div(
                    shiny::div(class = "cover-stat-num", "100%"),
                    shiny::div(class = "cover-stat-lbl", "Sin c\u00f3digo")
                  )
                )
              ),
              
              # Columna derecha — Mock dashboard
              shiny::div(
                class = "cover-visual",
                shiny::div(
                  class = "cover-mock",
                  
                  # Barra de título del mock
                  shiny::div(
                    class = "mock-header",
                    shiny::div(class = "mock-dot dot-red"),
                    shiny::div(class = "mock-dot dot-amber"),
                    shiny::div(class = "mock-dot dot-green"),
                    shiny::tags$span(class = "mock-title", "estimaciones_dominio.R")
                  ),
                  
                  # KPIs
                  shiny::div(
                    class = "mock-kpis",
                    shiny::div(
                      class = "mock-kpi",
                      shiny::div(class = "mock-kpi-label", "Aceptables"),
                      shiny::div(class = "mock-kpi-value green", "42")
                    ),
                    shiny::div(
                      class = "mock-kpi",
                      shiny::div(class = "mock-kpi-label", "Regulares"),
                      shiny::div(class = "mock-kpi-value amber", "18")
                    ),
                    shiny::div(
                      class = "mock-kpi",
                      shiny::div(class = "mock-kpi-label", "No conf."),
                      shiny::div(class = "mock-kpi-value red", "7")
                    )
                  ),
                  
                  # Minigrafico de línea (sparkline simulado)
                  shiny::div(
                    class = "mock-sparkline",
                    shiny::div(class = "mock-spark", style = "height:55%; background:rgba(0,159,219,0.18);"),
                    shiny::div(class = "mock-spark", style = "height:70%; background:rgba(0,159,219,0.22);"),
                    shiny::div(class = "mock-spark", style = "height:50%; background:rgba(0,159,219,0.18);"),
                    shiny::div(class = "mock-spark", style = "height:80%; background:rgba(0,180,100,0.25);"),
                    shiny::div(class = "mock-spark", style = "height:65%; background:rgba(0,180,100,0.20);"),
                    shiny::div(class = "mock-spark", style = "height:90%; background:rgba(0,180,100,0.28);"),
                    shiny::div(class = "mock-spark", style = "height:72%; background:rgba(0,159,219,0.22);")
                  ),
                  
                  # Gráfico de barras animado
                  shiny::div(
                    class = "mock-chart",
                    shiny::div(class = "mock-bar", style = "height:30%; animation-delay:0.08s;"),
                    shiny::div(class = "mock-bar", style = "height:55%; animation-delay:0.16s;"),
                    shiny::div(class = "mock-bar", style = "height:78%; animation-delay:0.24s;"),
                    shiny::div(class = "mock-bar", style = "height:42%; animation-delay:0.32s;"),
                    shiny::div(class = "mock-bar", style = "height:90%; animation-delay:0.40s;"),
                    shiny::div(class = "mock-bar", style = "height:65%; animation-delay:0.48s;"),
                    shiny::div(class = "mock-bar", style = "height:38%; animation-delay:0.56s;"),
                    shiny::div(class = "mock-bar", style = "height:72%; animation-delay:0.64s;"),
                    shiny::div(class = "mock-bar", style = "height:50%; animation-delay:0.72s;"),
                    shiny::div(class = "mock-bar", style = "height:84%; animation-delay:0.80s;")
                  ),
                  
                  # Etiquetas eje X
                  shiny::div(
                    class = "mock-chart-labels",
                    shiny::tags$span("E"),
                    shiny::tags$span("F"),
                    shiny::tags$span("M"),
                    shiny::tags$span("A"),
                    shiny::tags$span("M"),
                    shiny::tags$span("J"),
                    shiny::tags$span("J"),
                    shiny::tags$span("A"),
                    shiny::tags$span("S"),
                    shiny::tags$span("O")
                  )
                )
              )
            ),
            
            # ── FEATURES ─────────────────────────────────────────
            shiny::div(
              class = "cover-features-section",
              shiny::div(class = "cover-section-title", "Capacidades principales"),
              shiny::tags$h2(
                class = "cover-section-heading",
                "Todo lo que necesitas para muestreo complejo"
              ),
              
              shiny::div(
                class = "cover-features",
                
                # Feature 1
                shiny::div(
                  class = "cover-feature-card",
                  shiny::div(class = "cover-feat-num", "01"),
                  shiny::div(
                    class = "cover-feature-icon",
                    shiny::tags$i(class = "fa-solid fa-database")
                  ),
                  shiny::tags$h3(shiny::textOutput("cover_feat1_title", inline = TRUE)),
                  shiny::tags$p(shiny::textOutput("cover_feat1_desc", inline = TRUE))
                ),
                
                # Feature 2
                shiny::div(
                  class = "cover-feature-card",
                  shiny::div(class = "cover-feat-num", "02"),
                  shiny::div(
                    class = "cover-feature-icon",
                    shiny::tags$i(class = "fa-solid fa-sitemap")
                  ),
                  shiny::tags$h3(shiny::textOutput("cover_feat2_title", inline = TRUE)),
                  shiny::tags$p(shiny::textOutput("cover_feat2_desc", inline = TRUE))
                ),
                
                # Feature 3
                shiny::div(
                  class = "cover-feature-card",
                  shiny::div(class = "cover-feat-num", "03"),
                  shiny::div(
                    class = "cover-feature-icon",
                    shiny::tags$i(class = "fa-solid fa-chart-line")
                  ),
                  shiny::tags$h3(shiny::textOutput("cover_feat3_title", inline = TRUE)),
                  shiny::tags$p(shiny::textOutput("cover_feat3_desc", inline = TRUE))
                )
              )
            ),
            
            # ── FOOTER ───────────────────────────────────────────
            shiny::div(
              class = "cover-footer",
              shiny::tags$span(shiny::textOutput("app_footer", inline = TRUE)),
              shiny::tags$span(class = "sep", "\u00b7"),
              shiny::tags$a(
                href   = "https://github.com/stalynGuerrero/complexr",
                target = "_blank",
                shiny::tags$i(class = "fa-brands fa-github"),
                " GitHub"
              )
            )
          )
        )
      ),
      
      # ============================================================
      # PESTAÑA 1 — Datos
      # ============================================================
      shiny::tabPanel(
        title = shiny::tagList(
          shiny::tags$i(class = "fa fa-database"),
          shiny::textOutput("tab_datos", inline = TRUE)
        ),
        value = "datos",
        mod_datos_ui("datos")
      ),
      
      # ============================================================
      # PESTAÑA 2 — Diseño muestral
      # ============================================================
      shiny::tabPanel(
        title = shiny::tagList(
          shiny::tags$i(class = "fa fa-sitemap"),
          shiny::textOutput("tab_diseno", inline = TRUE)
        ),
        value = "diseno",
        mod_diseno_ui("diseno")
      ),
      
      # ============================================================
      # PESTAÑA 3 — Estimación
      # ============================================================
      shiny::tabPanel(
        title = shiny::tagList(
          shiny::tags$i(class = "fa fa-bar-chart"),
          shiny::textOutput("tab_estimacion", inline = TRUE)
        ),
        value = "estimacion",
        mod_estimacion_ui("estimacion")
      ),
      
      # ============================================================
      # PESTAÑA 4 — Tablas de estimaciones
      # ============================================================
      shiny::tabPanel(
        title = shiny::tagList(
          shiny::tags$i(class = "fa fa-table"),
          shiny::textOutput("tab_tablas", inline = TRUE)
        ),
        value = "tablas",
        mod_tablas_ui("tablas")
      ),
      
      # ── Selector de idioma + botón Salir ──────────────────────
      footer = shiny::tags$div(
        id    = "lang-container",
        class = "navbar-custom-menu",
        shiny::tags$div(
          style = "display:flex; align-items:center; gap:8px;",
          shiny::tags$span(class = "lang-label", "Idioma"),
          shiny::tags$span(class = "fa fa-globe", style = "color:#33B7E8;"),
          shiny::selectizeInput(
            inputId  = "lang",
            label    = NULL,
            choices  = c("ES" = "es", "EN" = "en"),
            selected = "es",
            width    = "80px",
            options  = list(maxItems = 1, searchField = FALSE)
          ),
          shiny::actionButton(
            inputId = "exit_app",
            label   = shiny::tagList(
              shiny::tags$i(class = "fa fa-power-off"),
              shiny::textOutput("btn_exit", inline = TRUE)
            ),
            class = "btn btn-danger btn-sm",
            style = "margin-left:4px;"
          )
        )
      )
    )
  )
}