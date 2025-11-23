library(shiny)
library(ggplot2)
library(DT)

server <- function(input, output, session) {
  
  # Valor reactivo donde se guardarán los datos (cargados o manuales)
  datos <- reactiveVal(data.frame())
  
  # Cargar archivo CSV
  observeEvent(input$file, {
    df <- tryCatch({
      read.csv(input$file$datapath, header = input$header, stringsAsFactors = FALSE)
    }, error = function(e) {
      showNotification(paste("Error leyendo CSV:", e$message), type = "error")
      return(data.frame())
    })
    datos(df)
  })
  
  # Modal para ingresar datos manualmente
  observeEvent(input$manualData, {
    showModal(modalDialog(
      title = "Crea tu tabla manualmente",
      
      textInput("colNames", "Nombres de columnas (separados por coma):", "grupo,valor"),
      numericInput("nRows", "Número de filas:", min = 1, max = 200, value = 9),
      
      actionButton("crearTabla", "Crear tabla"),
      hr(),
      
      DTOutput("editTable"),
      
      footer = tagList(
        actionButton("saveManual", "Guardar", class = "btn btn-success"),
        modalButton("Cerrar")
      ),
      size = "l"
    ))
  })
  
  # Crear tabla manual
  observeEvent(input$crearTabla, {
    req(input$colNames)
    req(input$nRows)
    
    cols <- trimws(unlist(strsplit(input$colNames, ",")))
    if (length(cols) == 0) {
      showNotification("Introduce al menos 1 nombre de columna.", type = "error")
      return()
    }
    
    nuevaTabla <- as.data.frame(matrix(
      NA,
      nrow = input$nRows,
      ncol = length(cols)
    ), stringsAsFactors = FALSE)
    colnames(nuevaTabla) <- cols
    
    datos(nuevaTabla)
  })
  
  # Mostrar tabla editable dentro del modal
  output$editTable <- renderDT({
    datatable(datos(), editable = TRUE, options = list(dom = 't', paging = FALSE))
  })
  
  # Guardar edición de celdas en vivo para tabla del modal
  observeEvent(input$editTable_cell_edit, {
    info <- input$editTable_cell_edit
    df <- datos()
    
    row <- info$row
    col <- info$col
    value <- info$value
    
    num <- suppressWarnings(as.numeric(value))
    if (!is.na(num) && value != "") {
      df[row, col] <- num
    } else if (value == "") {
      df[row, col] <- NA
    } else {
      df[row, col] <- value
    }
    
    datos(df)
  })
  
  # Cerrar modal
  observeEvent(input$saveManual, {
    removeModal()
  })
  
  # ============================
  #  FILTRO DE TABLA SEGÚN VARIABLES SELECCIONADAS
  # ============================
  
  tabla_filtrada <- reactive({
    req(nrow(datos()) > 0)
    df <- datos()
    
    # Regresión simple
    if (input$modelType == "simple" &&
        !is.null(input$yvar) &&
        !is.null(input$xvar)) {
      
      return(df[, c(input$xvar, input$yvar), drop = FALSE])
    }
    
    # Regresión múltiple
    if (input$modelType == "multiple" &&
        length(input$xvars) > 0 &&
        !is.null(input$yvar)) {
      
      columnas <- c(input$yvar, input$xvars)
      return(df[, columnas, drop = FALSE])
    }
    
    # Si no hay selección, mostrar todo
    return(df)
  })
  
  # Mostrar tabla filtrada
  output$tabla <- renderDT({
    validate(need(nrow(datos()) > 0, "Cargue o ingrese datos primero."))
    
    datatable(
      tabla_filtrada(),
      options = list(pageLength = 10),
      editable = TRUE
    )
  })
  
  # Editar valores en la tabla principal (respeta el filtrado)
  observeEvent(input$tabla_cell_edit, {
    info <- input$tabla_cell_edit
    df <- datos()
    
    # Mapeo Columna filtrada -> Columna original
    colname <- colnames(tabla_filtrada())[info$col]
    
    # Mapeo Fila filtrada -> Fila original
    rowname <- rownames(tabla_filtrada())[info$row]
    
    value <- info$value
    
    num <- suppressWarnings(as.numeric(value))
    if (!is.na(num) && value != "") {
      df[rowname, colname] <- num
    } else if (value == "") {
      df[rowname, colname] <- NA
    } else {
      df[rowname, colname] <- value
    }
    
    datos(df)
  })
  
  # UI de selección de variables
  output$varSelectUI <- renderUI({
    validate(need(nrow(datos()) > 0, "Cargue o ingrese datos primero."))
    
    vars <- names(datos())
    tagList(
      selectInput("yvar", "Variable dependiente (Y):", choices = vars),
      
      conditionalPanel(
        condition = "input.modelType != 'multiple'",
        selectInput("xvar", "Variable independiente (X):", choices = vars)
      ),
      
      conditionalPanel(
        condition = "input.modelType == 'multiple'",
        checkboxGroupInput("xvars", "Predictores múltiples:", choices = vars)
      )
    )
  })
  
  # ============================
  # PREPARAR VARIABLES (num / factor / char)
  # ============================
  
  prepare_variable <- function(vec) {
    if (is.numeric(vec)) return(vec)
    if (is.factor(vec)) return(vec)
    if (is.character(vec)) {
      num <- suppressWarnings(as.numeric(vec))
      if (!all(is.na(num[!is.na(vec)]))) {
        return(num)
      }
      return(as.factor(vec))
    }
    return(vec)
  }
  
  # ============================
  # GENERAR MODELO
  # ============================
  
  modelo <- eventReactive(input$runModel, {
    validate(need(nrow(datos()) > 0, "Cargue o ingrese datos primero."))
    df <- datos()
    
    # Evitar X = Y
    if (!is.null(input$yvar) && !is.null(input$xvar) &&
        input$modelType == "simple" &&
        input$yvar == input$xvar) {
      validate(need(FALSE, "Seleccione variables diferentes para X y Y."))
    }
    
    # Lineal simple
    if (input$modelType == "simple") {
      y <- prepare_variable(df[[input$yvar]])
      x <- prepare_variable(df[[input$xvar]])
      
      if (!is.numeric(y)) validate(need(FALSE, "Y debe ser numérica."))
      if (all(is.na(x))) validate(need(FALSE, "X contiene solo NA."))
      
      use_df <- na.omit(data.frame(y = y, x = x))
      validate(need(nrow(use_df) >= 2, "Datos insuficientes."))
      
      return(lm(y ~ x, data = use_df))
    }
    
    # Lineal múltiple
    if (input$modelType == "multiple") {
      y <- prepare_variable(df[[input$yvar]])
      if (!is.numeric(y)) validate(need(FALSE, "Y debe ser numérica."))
      
      pred_names <- input$xvars
      preds <- lapply(pred_names, function(nm) prepare_variable(df[[nm]]))
      
      full_df <- data.frame(y = y, as.data.frame(preds))
      colnames(full_df) <- c("y", pred_names)
      full_df <- na.omit(full_df)
      validate(need(nrow(full_df) >= length(pred_names) + 1, 
                    "Datos insuficientes para regresión múltiple."))
      
      form <- as.formula(paste("y ~", paste(pred_names, collapse = "+")))
      return(lm(form, data = full_df))
    }
  })
  
  # Resultado del modelo
  output$resultado <- renderPrint({
    req(modelo())
    print(summary(modelo()))
  })
  
  # Gráfico
  output$grafico <- renderPlot({
    req(modelo())
    
    if (input$modelType == "simple") {
      mf <- model.frame(modelo())
      yname <- names(mf)[1]
      xname <- names(mf)[2]
      ggplot(mf, aes_string(x = xname, y = yname)) +
        geom_point(size = 3, color = "steelblue") +
        geom_smooth(method = "lm", se = TRUE, color = "red") +
        theme_minimal() +
        labs(title = "Regresión Lineal Simple",
             x = input$xvar,
             y = input$yvar)
    } else {
      par(mfrow = c(2, 2))
      plot(modelo())
    }
  })
  
  # Interpretación
  output$interpretacion <- renderPrint({
    req(modelo())
    cat("=== INTERPRETACIÓN DEL MODELO ===\n\n")
    
    cat("Coeficientes:\n")
    print(coef(modelo()))
    
    if (input$modelType == "simple") {
      coefs <- coef(modelo())
      cat(sprintf("\nModelo: %s = %.4f + %.4f * %s\n",
                  input$yvar, coefs[1], coefs[2], input$xvar))
      r2 <- summary(modelo())$r.squared
      cat(sprintf("\nR² = %.4f\n", r2))
    }
    
    if (input$modelType == "multiple") {
      coefs <- coef(modelo())
      cat(sprintf("\n%s = %.4f", input$yvar, coefs[1]))
      for (i in 2:length(coefs)) {
        cat(sprintf(" + %.4f * %s", coefs[i], names(coefs)[i]))
      }
      cat("\n")
      r2 <- summary(modelo())$r.squared
      adj <- summary(modelo())$adj.r.squared
      cat(sprintf("\nR² = %.4f\nR² ajustado = %.4f\n", r2, adj))
    }
  })
  
}

