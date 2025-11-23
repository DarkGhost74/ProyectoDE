library(shiny)
library(DT)

ui <- fluidPage(
  
  # ---- CSS Moderno Mejorado ----
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;600;700&display=swap');

      body {
        font-family: 'Poppins', sans-serif;
        background: #f3f4f7;
        margin: 0;
        padding: 0;
      }

      .main-card {
        background: white;
        padding: 25px;
        border-radius: 20px;
        box-shadow: 0 8px 25px rgba(0,0,0,0.08);
        margin-bottom: 30px;
        animation: fadeIn 0.5s ease;
      }

      h1 {
        font-weight: 700;
        color: #5a4fcf;
      }
      h3, h4 {
        color: #5a4fcf;
        font-weight: 600;
      }

      .sidebar {
        background: white;
        padding: 25px;
        border-radius: 20px;
        box-shadow: 0 8px 25px rgba(0,0,0,0.08);
        position: sticky;
        top: 15px;
        animation: fadeIn 0.6s ease;
      }

      .form-control, .selectize-input {
        border-radius: 10px;
        border: 1.5px solid #dadcef;
        padding: 10px;
        transition: .3s;
      }
      .form-control:focus,
      .selectize-input.focus {
        border-color: #5a4fcf;
        box-shadow: 0 0 0 3px rgba(90, 79, 207, .2);
      }

      /* -------- BOTONES VERDES -------- */
      .btn-primary {
        background: #28a745;        /* verde */
        border-radius: 10px;
        border: none;
        font-weight: 600;
        padding: 12px;
        color: white;
      }
      .btn-primary:hover {
        background: #218838;        /* verde más oscuro */
        color: white;
      }

      .btn-default {
        background: #d4edda;        /* verde claro */
        border-radius: 10px;
        border: 1px solid #c3e6cb;
        color: #155724;
      }
      .btn-default:hover {
        background: #c3e6cb;
      }

      .nav-tabs > li > a {
        font-weight: 600;
        color: #6b6c7e;
      }
      .nav-tabs > li.active > a {
        color: #5a4fcf;
        border-bottom: 3px solid #5a4fcf;
      }

      .tab-content {
        background: white;
        padding: 25px;
        border-radius: 20px;
        box-shadow: 0 8px 25px rgba(0,0,0,0.08);
        animation: fadeIn 0.5s ease;
      }

      table.dataTable thead th {
        background: #5a4fcf !important;
        color: white;
        font-weight: 600;
      }
      table.dataTable tbody tr:hover {
        background: #f5f6ff;
      }

      pre {
        background: #f7f8fb;
        border-radius: 10px;
        padding: 20px;
        border-left: 5px solid #5a4fcf;
      }

      @keyframes fadeIn {
        from { opacity: 0; transform: translateY(10px); }
        to { opacity: 1; transform: translateY(0); }
      }
    "))
  ),

  # ---- TÍTULO ----
  div(class = "main-card",
      h1("📊 Resolución de Problemas de Regresión"),
      p(style="color:#6b6c7e; font-size: 15px;",
        "Análisis estadístico de regresión lineal simple y múltiple")
  ),

  # ---- LAYOUT ----
  sidebarLayout(

    # -------- SIDEBAR --------
    sidebarPanel(
      class = "sidebar",

      h3("📁 Datos"),
      fileInput("file", NULL, accept = ".csv",
                placeholder = "Selecciona un archivo CSV..."),
      checkboxInput("header", "Archivo con encabezado", TRUE),

      actionButton("manualData",
                   "Ingresar datos manualmente",
                   class = "btn-default",
                   style = "width:100%; margin-bottom:15px;"),

      hr(),

      h3("⚙️ Configuración del Modelo"),

      selectInput("modelType", "Tipo de modelo:",
                  choices = c(
                    "Regresión Lineal Simple" = "simple",
                    "Regresión Lineal Múltiple" = "multiple"
                  )),

      uiOutput("varSelectUI"),

      hr(),

      actionButton(
        "runModel",
        "Ejecutar Modelo",
        class = "btn-primary",
        style = "width:100%; font-size: 16px;"
      )
    ),

    # -------- MAIN PANEL --------
    mainPanel(
      tabsetPanel(
        id = "mainTabs",

        tabPanel("Datos",
                 br(),
                 DTOutput("tabla")),

        tabPanel("Resultados",
                 br(),
                 h4("📘 Resumen del Modelo"),
                 pre(verbatimTextOutput("resultado"))),

        tabPanel("Gráfica",
                 br(),
                 plotOutput("grafico", height="550px")),

        tabPanel("Interpretación",
                 br(),
                 h4("📝 Interpretación"),
                 pre(verbatimTextOutput("interpretacion")))
      )
    )
  )
)
