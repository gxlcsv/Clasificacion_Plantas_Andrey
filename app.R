# ==============================================================================
# Shiny App - Gestión Agrícola: Mortalidad de Plantas
# Green Xpo Lab (GXL)
# ==============================================================================
# Estructura esperada:
#   /app.R
#   /data/*.gpkg
#   /www/logo.jpg
#
# Paquetes requeridos:
# install.packages(c("shiny","leaflet","sf","plotly","dplyr","DT","writexl","conflicted"))
# ==============================================================================

library(shiny)
library(leaflet)
library(sf)
library(plotly)
library(dplyr)
library(DT)
library(writexl)
library(conflicted)

conflicts_prefer(shiny::observe)
conflicts_prefer(dplyr::filter)
conflicts_prefer(plotly::layout)

# ------------------------------------------------------------------------------
# HELPER: ordenamiento numérico de bloques (evita orden léxico 1,10,11,2,...)
# ------------------------------------------------------------------------------
extraer_num_bloque <- function(x) {
  suppressWarnings(as.numeric(gsub("[^0-9.]", "", as.character(x))))
}

ordenar_bloques_num <- function(x) {
  x_chr <- as.character(x)
  nums  <- extraer_num_bloque(x_chr)
  x_chr[order(is.na(nums), nums, x_chr)]
}

# ==============================================================================
# 1. INTERFAZ DE USUARIO (UI)
# ==============================================================================

ui <- fluidPage(

  tags$head(
    tags$style(HTML("
      :root {
        --gxl-green:      #0d5c3f;
        --gxl-green-dark: #083726;
        --gxl-green-soft: #edf6f1;
        --gxl-border:     #d7e4dd;
        --gxl-text:       #173228;
        --gxl-muted:      #61756c;
        --gxl-white:      #ffffff;
        --gxl-shadow:     0 10px 26px rgba(13,92,63,0.08);
        --gxl-radius:     18px;
      }

      body {
        background: linear-gradient(180deg,#f5faf7 0%,#eef4f1 100%);
        color: var(--gxl-text);
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
      }

      .container-fluid {
        max-width: 1750px;
        padding-left: 22px;
        padding-right: 22px;
      }

      /* ── HERO ──────────────────────────────────────────────── */
      .hero {
        background: linear-gradient(135deg, var(--gxl-green-dark) 0%, var(--gxl-green) 100%);
        color: white;
        border-radius: 22px;
        padding: 20px 24px;
        margin-top: 10px;
        margin-bottom: 18px;
        box-shadow: 0 14px 30px rgba(8,55,38,0.16);
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 18px;
      }
      .hero-title   { margin:0; font-size:28px; font-weight:800; letter-spacing:0.2px; }
      .hero-subtitle{ margin-top:6px; margin-bottom:0; color:rgba(255,255,255,0.85); font-size:13px; }
      .brand-logo   { height:82px; width:auto; border-radius:14px;
                      background:rgba(255,255,255,0.94); padding:6px; }

      /* ── TABS ──────────────────────────────────────────────── */
      .nav-tabs { border-bottom:none; margin-bottom:12px; }
      .nav-tabs > li > a {
        border:none !important; border-radius:14px !important;
        margin-right:10px; background:#e7f0eb;
        color:var(--gxl-green-dark); font-weight:700; padding:11px 18px;
      }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        background: linear-gradient(135deg, var(--gxl-green-dark) 0%, var(--gxl-green) 100%) !important;
        color:white !important;
        box-shadow:0 10px 24px rgba(13,92,63,0.20);
      }
      .tab-content { padding-top:4px; }

      /* ── CARDS ─────────────────────────────────────────────── */
      .gxl-card {
        background: var(--gxl-white);
        border: 1px solid var(--gxl-border);
        border-radius: var(--gxl-radius);
        box-shadow: var(--gxl-shadow);
        padding: 18px 18px 12px 18px;
        margin-bottom: 16px;
      }
      .gxl-sidebar-card {
        position: sticky;
        top: 12px;
      }
      .gxl-card-title {
        font-size: 17px; font-weight: 800;
        color: var(--gxl-green-dark);
        margin-top: 0; margin-bottom: 6px;
      }
      .gxl-card-desc {
        font-size: 12px; color: var(--gxl-muted); margin-bottom: 10px;
      }

      /* ── KPI BOXES ─────────────────────────────────────────── */
      .kpi-box {
        background: var(--gxl-white);
        border: 1px solid var(--gxl-border);
        border-radius: var(--gxl-radius);
        box-shadow: var(--gxl-shadow);
        padding: 16px 18px;
        margin-bottom: 14px;
        min-height: 110px;
      }
      .kpi-label { color:var(--gxl-muted); font-size:13px; font-weight:600; margin-bottom:6px; }
      .kpi-value { color:var(--gxl-green-dark); font-size:28px; font-weight:800; line-height:1.15; }
      .kpi-note  { color:var(--gxl-muted); font-size:11px; margin-top:6px; }

      /* ── SIDEBAR CONTROLS ──────────────────────────────────── */
      .ctrl-card { background:var(--gxl-white); border:1px solid var(--gxl-border);
                   border-radius:var(--gxl-radius); box-shadow:var(--gxl-shadow);
                   padding:14px 16px 8px; margin-bottom:14px; }
      .ctrl-title { font-size:17px; font-weight:800; color:var(--gxl-green-dark);
                    margin-top:0; margin-bottom:10px; }
      .ctrl-note  { color:var(--gxl-muted); font-size:12px; line-height:1.45; margin-top:4px; }

      /* ── FORM ELEMENTS ─────────────────────────────────────── */
      .selectize-input, .form-control {
        border-radius:10px !important; border-color:#c9d9d1 !important; box-shadow:none !important;
      }
      .selectize-input.focus, .form-control:focus {
        border-color:var(--gxl-green) !important;
      }

      /* ── BUTTONS ───────────────────────────────────────────── */
      .btn-gxl {
        background: linear-gradient(135deg,var(--gxl-green-dark),var(--gxl-green));
        color: white; border:none; border-radius:10px; font-weight:700;
        padding:8px 16px; width:100%; margin-top:6px; cursor:pointer;
      }
      .btn-gxl:hover { opacity:0.9; color:white; }
      .btn-default, .btn-primary { border-radius:10px; font-weight:700; }

      /* ── MISC ──────────────────────────────────────────────── */
      .leaflet-container { border-radius:16px; }
      .data-header { display:flex; justify-content:space-between;
                     align-items:center; gap:12px; margin-bottom:10px; }

      @media (max-width:767px) {
        .hero { flex-direction:column; align-items:flex-start; }
        .brand-logo { height:64px; }
      }
    "))
  ),

  # HERO HEADER
  div(class = "hero",
    div(
      tags$h1(class = "hero-title", "Gestión Agrícola: Mortalidad de Plantas"),
      tags$p(class = "hero-subtitle",
             "Distribución espacial, inventario y KPIs de mortalidad por bloque.")
    ),
    tags$img(src = "logo.jpg", class = "brand-logo")
  ),

  tabsetPanel(id = "tabs_main",

    # ── TAB 1: DASHBOARD ─────────────────────────────────────────
    tabPanel("Dashboard Operativo",
      br(),
      fluidRow(

        # SIDEBAR
        column(3,
          div(class = "ctrl-card gxl-sidebar-card",
            tags$h3(class = "ctrl-title", "Panel de Control"),
            selectInput("select_gpkg", "Archivo GeoPackage:",
                        choices = list.files("data/", pattern = "\\.gpkg$"),
                        width = "100%"),
            uiOutput("ui_selector_bloque"),
            tags$hr(),
            tags$button("Re-centrar Mapa", class = "btn-gxl",
                        onclick = "Shiny.setInputValue('zoom_btn', Math.random())"),
            tags$br(), tags$br(),
            div(class = "ctrl-note",
                "Selecciona un archivo .gpkg y filtra por bloque para actualizar
                 el mapa, gráficas e indicadores.")
          )
        ),

        # CONTENT
        column(9,

          # KPI BOXES
          fluidRow(
            column(4, uiOutput("kpi_vivas")),
            column(4, uiOutput("kpi_muertas")),
            column(4, uiOutput("kpi_mort"))
          ),

          # MAPA
          div(class = "gxl-card",
            tags$h4(class = "gxl-card-title", "Distribución Espacial"),
            div(class = "gxl-card-desc",
                "Bloques coloreados según porcentaje de mortalidad (escala YlOrRd)."),
            leafletOutput("mapa_plantas", height = "460px")
          ),

          # GRÁFICAS
          fluidRow(
            column(7,
              div(class = "gxl-card",
                tags$h4(class = "gxl-card-title", "Estado de Plantas"),
                div(class = "gxl-card-desc",
                    "Comparación de plantas vivas, muertas y en alerta por bloque."),
                plotlyOutput("grafica_barras", height = "340px")
              )
            ),
            column(5,
              div(class = "gxl-card",
                tags$h4(class = "gxl-card-title", "KPI de Mortalidad"),
                div(class = "gxl-card-desc",
                    "Mortalidad promedio del conjunto seleccionado."),
                plotlyOutput("grafica_gauge", height = "340px")
              )
            )
          )
        )
      )
    ),

    # ── TAB 2: DATOS ─────────────────────────────────────────────
    tabPanel("Datos Detallados",
      br(),
      div(class = "gxl-card",
        div(class = "data-header",
          div(
            tags$h4(class = "gxl-card-title", "Resumen de Inventario"),
            div(class = "gxl-card-desc",
                "Tabla exportable con todos los bloques del archivo seleccionado.")
          ),
          div(
            downloadButton("download_csv",   "CSV",   class = "btn-default"),
            tags$span(" "),
            downloadButton("download_excel", "Excel", class = "btn-primary")
          )
        ),
        DTOutput("tabla_maestra")
      )
    )
  )
)

# ==============================================================================
# 2. SERVIDOR
# ==============================================================================

server <- function(input, output, session) {

  # ── 2.1 LECTURA DE DATOS ─────────────────────────────────────
  datos <- reactive({
    req(input$select_gpkg)
    ruta <- file.path("data", input$select_gpkg)
    if (!file.exists(ruta)) return(NULL)

    df <- st_read(ruta, quiet = TRUE) %>% st_transform(4326)

    # Normalizar nombres a minúsculas para uniformidad
    names(df) <- tolower(names(df))

    # Columnas obligatorias con fallback seguro
    if (!"name"      %in% names(df)) df$name      <- seq_len(nrow(df))
    if (!"area"      %in% names(df)) df$area      <- NA_real_
    if (!"numpoints" %in% names(df)) df$numpoints <- NA_real_
    if (!"viva"      %in% names(df)) df$viva      <- NA_real_
    if (!"muerta"    %in% names(df)) df$muerta    <- NA_real_
    if (!"alerta"    %in% names(df)) df$alerta    <- NA_real_
    if (!"total_pl"  %in% names(df)) df$total_pl  <- NA_real_
    if (!"perc_mort" %in% names(df)) df$perc_mort <- NA_real_

    # Columnas opcionales (pueden no existir según el archivo)
    tiene_area_ef <- "area_efectiva" %in% names(df)
    tiene_densidad <- any(grepl("^dens", names(df)))

    if (tiene_densidad) {
      col_dens <- names(df)[grepl("^dens", names(df))][1]
      df <- df %>% rename(densidad = !!sym(col_dens))
    } else {
      df$densidad <- NA_real_
    }

    df <- df %>%
      mutate(
        name      = as.character(name),
        area      = suppressWarnings(as.numeric(area)),
        numpoints = suppressWarnings(as.numeric(numpoints)),
        viva      = suppressWarnings(as.numeric(viva)),
        muerta    = suppressWarnings(as.numeric(muerta)),
        alerta    = suppressWarnings(as.numeric(alerta)),
        total_pl  = suppressWarnings(as.numeric(total_pl)),
        perc_mort = suppressWarnings(as.numeric(perc_mort)),
        densidad  = suppressWarnings(as.numeric(densidad))
      )

    attr(df, "tiene_area_ef") <- tiene_area_ef
    df
  })

  datos_filtrados <- reactive({
    req(datos(), input$bloque_filtro)
    df <- datos()
    if (input$bloque_filtro != "Todos") {
      df <- dplyr::filter(df, name == input$bloque_filtro)
    }
    df
  })

  output$ui_selector_bloque <- renderUI({
    req(datos())
    selectInput("bloque_filtro", "Filtrar por Bloque:",
                choices = c("Todos", ordenar_bloques_num(unique(datos()$name))))
  })

  # ── 2.2 TABLA FINAL ──────────────────────────────────────────
  # FIX: construir la tabla solo con columnas que existen en el archivo
  tabla_final_df <- reactive({
    req(datos_filtrados())
    df <- datos_filtrados() %>% st_drop_geometry()

    # Columnas base (siempre presentes tras la lectura)
    result <- df %>%
      transmute(
        Bloque                    = name,
        `Área Total (ha)`         = area,
        `Cantidad Total`          = numpoints,
        `Densidad (Plantas/ha)`   = densidad,
        `Plantas Vivas`           = viva,
        `Plantas Muertas`         = muerta,
        `Plantas en Alerta`       = alerta,
        `Total Plantas`           = total_pl,
        `Porcentaje de Mortalidad`= perc_mort
      )

    # Columna opcional: solo se agrega si existía en el archivo original
    if (isTRUE(attr(datos(), "tiene_area_ef"))) {
      result <- result %>%
        mutate(`Área Efectiva (ha)` = df$area_efectiva)
    }

    result
  })

  # ── 2.3 KPI BOXES ────────────────────────────────────────────
  kpi_box <- function(label, value_expr, note = NULL, color = "var(--gxl-green-dark)") {
    div(class = "kpi-box",
      div(class = "kpi-label", label),
      div(class = "kpi-value", style = paste0("color:", color), value_expr),
      if (!is.null(note)) div(class = "kpi-note", note)
    )
  }

  output$kpi_vivas <- renderUI({
    val <- format(sum(datos_filtrados()$viva, na.rm = TRUE), big.mark = ",")
    kpi_box("Plantas Vivas", val, "Total en selección", color = "#0b6e3b")
  })

  output$kpi_muertas <- renderUI({
    val <- format(sum(datos_filtrados()$muerta, na.rm = TRUE), big.mark = ",")
    kpi_box("Plantas Muertas", val, "Total en selección", color = "#c0392b")
  })

  output$kpi_mort <- renderUI({
    val <- mean(datos_filtrados()$perc_mort, na.rm = TRUE)
    val <- if (is.na(val) || is.nan(val)) 0 else val
    kpi_box("Mortalidad Promedio", paste0(round(val, 2), "%"),
            "Promedio de bloques", color = "#b7770d")
  })

  # ── 2.4 MAPA ─────────────────────────────────────────────────
  output$mapa_plantas <- renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satélite") %>%
      addProviderTiles(providers$CartoDB.Positron,  group = "Base clara") %>%
      addLayersControl(
        baseGroups = c("Satélite", "Base clara"),
        options    = layersControlOptions(collapsed = FALSE)
      ) %>%
      setView(lng = -84.0, lat = 10.0, zoom = 8)
  })

  observe({
    df <- datos_filtrados()
    req(nrow(df) > 0)

    pal <- colorNumeric(
      palette = "YlOrRd",
      domain  = datos()$perc_mort,
      na.color = "#bdbdbd"
    )

    bbox <- st_bbox(df)

    leafletProxy("mapa_plantas", data = df) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        fillColor  = ~pal(perc_mort),
        weight     = 1.5,
        color      = "white",
        fillOpacity = 0.75,
        layerId    = ~name,
        popup      = ~paste0(
          "<b>Bloque: </b>",       name,      "<br><hr>",
          "<b>Total Plantas: </b>", numpoints, "<br>",
          "<b>Vivas: </b>",         viva,      "<br>",
          "<b>Muertas: </b>",       muerta,    "<br>",
          "<b>Alertas: </b>",       alerta,    "<br>",
          "<b>Mortalidad: </b>",    round(perc_mort, 2), "%"
        ),
        label = ~name,
        labelOptions = labelOptions(
          textsize  = "11px",
          direction = "center",
          style = list("font-weight" = "bold", "color" = "#111",
                       "text-shadow" = "1px 1px 2px white")
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal      = pal,
        values   = datos()$perc_mort,
        title    = "Mortalidad (%)",
        opacity  = 0.9
      ) %>%
      addLayersControl(
        baseGroups = c("Satélite", "Base clara"),
        options    = layersControlOptions(collapsed = FALSE)
      ) %>%
      flyToBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]])
  })

  observeEvent(input$zoom_btn, {
    req(datos())
    bbox <- st_bbox(datos())
    leafletProxy("mapa_plantas") %>%
      flyToBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]])
  })

  # ── 2.5 GRÁFICA DE BARRAS ────────────────────────────────────
  output$grafica_barras <- renderPlotly({
    df <- tabla_final_df()
    req(nrow(df) > 0)

    # Ordenar bloques numéricamente y convertir a factor para que plotly respete el orden
    orden <- ordenar_bloques_num(df$Bloque)
    df <- df %>%
      mutate(Bloque = factor(Bloque, levels = orden)) %>%
      arrange(Bloque)

    plot_ly(df,
      x = ~Bloque, y = ~`Plantas Vivas`,
      type = "bar", name = "Vivas",
      marker = list(color = "#0d5c3f")
    ) %>%
      add_trace(y = ~`Plantas Muertas`,  name = "Muertas",
                marker = list(color = "#c0392b")) %>%
      add_trace(y = ~`Plantas en Alerta`, name = "Alertas",
                marker = list(color = "#d4a200")) %>%
      plotly::layout(
        barmode = "group",
        paper_bgcolor = "white",
        plot_bgcolor  = "white",
        font   = list(family = "Segoe UI", color = "#173228"),
        xaxis  = list(title = "Bloque", type = "category",
                      categoryorder = "array",
                      categoryarray = orden,
                      tickfont = list(size = 11)),
        yaxis  = list(title = "Cantidad", gridcolor = "#e0ece5"),
        legend = list(orientation = "h", x = 0, y = -0.25),
        margin = list(t = 10, b = 40)
      )
  })

  # ── 2.6 GAUGE ────────────────────────────────────────────────
  output$grafica_gauge <- renderPlotly({
    df  <- tabla_final_df()
    req(nrow(df) > 0)

    val <- mean(df$`Porcentaje de Mortalidad`, na.rm = TRUE)
    val <- if (is.na(val) || is.nan(val)) 0 else round(val, 2)

    plot_ly(
      type  = "indicator",
      mode  = "gauge+number+delta",
      value = val,
      number = list(suffix = "%", font = list(size = 36, color = "#083726")),
      gauge = list(
        axis  = list(range = list(NULL, 100), ticksuffix = "%",
                     tickcolor = "#173228"),
        bar   = list(color = "#083726"),
        bgcolor = "white",
        steps = list(
          list(range = c(0,  10), color = "#d4edda"),
          list(range = c(10, 20), color = "#fff3cd"),
          list(range = c(20,100), color = "#f8d7da")
        ),
        threshold = list(
          line      = list(color = "#c0392b", width = 3),
          thickness = 0.75,
          value     = 20
        )
      )
    ) %>%
      plotly::layout(
        paper_bgcolor = "white",
        font   = list(family = "Segoe UI"),
        margin = list(t = 20, b = 20, l = 30, r = 30)
      )
  })

  # ── 2.7 TABLA DATOS DETALLADOS ───────────────────────────────
  output$tabla_maestra <- renderDT({
    df <- tabla_final_df()
    req(nrow(df) > 0)

    # Columnas numéricas a redondear (excluye "Bloque" y enteros)
    cols_round <- which(names(df) %in%
      c("Área Total (ha)", "Área Efectiva (ha)",
        "Densidad (Plantas/ha)", "Porcentaje de Mortalidad"))

    datatable(
      df,
      rownames = FALSE,
      filter   = "top",
      options  = list(pageLength = 15, scrollX = TRUE,
                      dom = "lfrtip",
                      language = list(url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/es-ES.json"))
    ) %>%
      formatRound(columns = cols_round, digits = 2)
  })

  # ── 2.8 DESCARGAS ────────────────────────────────────────────
  output$download_csv <- downloadHandler(
    filename = function() { paste0("Inventario_", Sys.Date(), ".csv") },
    content  = function(file) { write.csv(tabla_final_df(), file, row.names = FALSE) }
  )

  output$download_excel <- downloadHandler(
    filename = function() { paste0("Inventario_", Sys.Date(), ".xlsx") },
    content  = function(file) { write_xlsx(tabla_final_df(), file) }
  )
}

# ==============================================================================
# 3. EJECUTAR
# ==============================================================================
shinyApp(ui, server)
