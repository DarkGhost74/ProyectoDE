library(shiny)
library(DT)

ui <- fluidPage(
  # CSS personalizado para diseño moderno
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap');
      
      body {
        font-family: 'Inter', sans-serif;
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        min-height: 100vh;
      }
      
      .container-fluid {
        padding: 30px;
      }
      
      /* Título principal */
      .main-title {
        background: white;
        padding: 25px;
        border-radius: 15px;
        box-shadow: 0 10px 30px rgba(0,0,0,0.1);
        margin-bottom: 30px;
        text-align: center;
      }
      
      .main-title h1 {
        color: #667eea;
        font-weight: 700;
        margin: 0;
        font-size: 2em;
      }
      
      /* Panel lateral */
      .well {
        background: white;
        border: none;
        border-radius: 15px;
        box-shadow: 0 10px 30px rgba(0,0,0,0.1);
        padding: 25px;
      }
      
      .well h3, .well h4 {
        color: #667eea;
        font-weight: 600;
        margin-top: 0;
      }
      
      /* Inputs */
      .form-control, .selectize-input {
        border: 2px solid #e0e7ff;
        border-radius: 8px;
        padding: 10px;
        transition: all 0.3s;
      }
      
      .form-control:focus, .selectize-input.focus {
        border-color: #667eea;
        box-shadow: 0 0 0 3px rgba(102, 126, 234, 0.1);
      }
      
      /* Botones */
      .btn {
        border-radius: 8px;
        padding: 10px 20px;
        font-weight: 600;
        border: none;
        transition: all 0.3s;
        cursor: pointer;
      }
      
      .btn-primary {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
      }
      
      .btn-primary:hover {
        transform: translateY(-2px);
        box-shadow: 0 5px 15px rgba(102, 126, 234, 0.4);
      }
      
      .btn-success {
        background: linear-gradient(135deg, #11998e 0%, #38ef7d 100%);
        color: white;
      }
      
      .btn-success:hover {
        transform: translateY(-2px);
        box-shadow: 0 5px 15px rgba(17, 153, 142, 0.4);
      }
      
      .btn-default {
        background: #f3f4f6;
        color: #4b5563;
      }
      
      .btn-default:hover {
        background: #e5e7eb;
      }
      
      /* Panel principal */
      .tabbable > .nav > li > a {
        border-radius: 10px 10px 0 0;
        color: #6b7280;
        font-weight: 600;
        padding: 12px 20px;
      }
      
      .tabbable > .nav > li.active > a {
        background: white;
        color: #667eea;
        border-bottom: 3px solid #667eea;
      }
      
      .tab-content {
        background: white;
        padding: 30px;
        border-radius: 0 15px 15px 15px;
        box-shadow: 0 10px 30px rgba(0,0,0,0.1);
      }
      
      /* Tablas */
      .dataTables_wrapper {
        font-size: 14px;
      }
      
      table.dataTable thead th {
        background: #667eea;
        color: white;
        font-weight: 600;
        padding: 12px;
      }
      
      table.dataTable tbody tr:hover {
        background: #f3f4f6;
      }
      
      /* Checkboxes */
      .checkbox label {
        font-weight: 500;
        color: #4b5563;
        padding-left: 5px;
      }
      
      input[type='checkbox'] {
        width: 18px;
        height: 18px;
        cursor: pointer;
      }
      
      /* Resultados */
      pre {
        background: #f9fafb;
        border: 2px solid #e5e7eb;
        border-radius: 8px;
        padding: 20px;
        font-size: 13px;
        color: #1f2937;
      }
      
      /* Separadores */
      hr {
        border: none;
        border-top: 2px solid #e5e7eb;
        margin: 20px 0;
      }
      
      /* Progress bar */
      .progress {
        height: 25px;
        border-radius: 8px;
        background: #e5e7eb;
      }
      
      .progress-bar {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        border-radius: 8px;
      }
      
      /* Modales */
      .modal-content {
        border-radius: 15px;
        border: none;
        box-shadow: 0 20px 60px rgba(0,0,0,0.3);
      }
      
      .modal-header {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        border-radius: 15px 15px 0 0;
        padding: 20px;
      }
      
      .modal-title {
        font-weight: 700;
      }
      
      /* Etiquetas de sección */
      .section-label {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        padding: 10px 15px;
        border-radius: 8px;
        font-weight: 600;
        margin-bottom: 15px;
        display: inline-block;
      }
      
      /* Animaciones */
      @keyframes fadeIn {
        from { opacity: 0; transform: translateY(10px); }
        to { opacity: 1; transform: translateY(0); }
      }
      
      .well, .tab-content {
        animation: fadeIn 0.5s ease;
      }
    "))
  ),
  
  # Contenido
  div(class = "main-title",
    h1("Resolución de Problemas de Regresión"),
    p(style = "color: #6b7280; margin: 10px 0 0 0;", 
      "Análisis de Regresión Lineal Simple, Múltiple y No Lineal")
  ),
  
  sidebarLayout(
    sidebarPanel(
      style = "position: sticky; top: 20px;",
      
      # Sección de carga de datos
      div(class = "section-label", "Datos"),
      
      fileInput("file", 
                label = NULL,
                placeholder = "Seleccionar archivo CSV...",
                accept = ".csv",
                buttonLabel = "Buscar...",
                width = "100%"),
      
      checkboxInput("header", "Archivo con encabezado", TRUE),
      
      actionButton("manualData", 
                   "Ingresar datos manualmente",
                   class = "btn-default",
                   style = "width: 100%; margin-bottom: 10px;"),
      
      hr(),
      
      # Sección de configuración
      div(class = "section-label", "Configuración del Modelo"),
      
      uiOutput("varSelectUI"),
      
      selectInput("modelType", 
                  "Tipo de modelo:",
                  choices = c(
                    "Regresión Lineal Simple" = "simple",
                    "Regresión Lineal Múltiple" = "multiple",
                    "Regresión No Lineal (NLS)" = "nls"
                  )),
      
      uiOutput("modelOptionsUI"),
      
      hr(),
      
      actionButton("runModel", 
                   "Ejecutar Modelo", 
                   class = "btn-primary",
                   style = "width: 100%; font-size: 16px; padding: 15px;")
    ),
    
    mainPanel(
      tabsetPanel(
        id = "mainTabs",
        tabPanel("Datos", 
                 br(),
                 DTOutput("tabla")),
        
        tabPanel("Resultados", 
                 br(),
                 div(style = "background: #f0f9ff; padding: 15px; border-radius: 8px; border-left: 4px solid #667eea; margin-bottom: 20px;",
                     strong("ℹ️ Resumen Estadístico del Modelo")
                 ),
                 verbatimTextOutput("resultado")),
        
        tabPanel("Gráfica", 
                 br(),
                 plotOutput("grafico", height = "550px")),
        
        tabPanel("Interpretación", 
                 br(),
                 div(style = "background: #f0fdf4; padding: 15px; border-radius: 8px; border-left: 4px solid #10b981; margin-bottom: 20px;",
                     strong("💡 Interpretación de Coeficientes")
                 ),
                 verbatimTextOutput("interpretacion"))
      )
    )
  )
)