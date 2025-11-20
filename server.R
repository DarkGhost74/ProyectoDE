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
  
  # Guardar edición de celdas en vivo
  observeEvent(input$editTable_cell_edit, {
    info <- input$editTable_cell_edit
    df <- datos()
    
    row <- info$row
    col <- info$col
    value <- info$value
    
    # Intentar convertir a número si aplica
    num <- suppressWarnings(as.numeric(value))
    if (!is.na(num) && value != "") {
      df[row, col] <- num
    } else if (value == "") {
      df[row, col] <- NA
    } else {
      # dejar texto si no es convertible
      df[row, col] <- value
    }
    
    datos(df)
  })
  
  # Cerrar modal al presionar guardar
  observeEvent(input$saveManual, {
    removeModal()
  })
  
  # Mostrar tabla cargada o manual
  output$tabla <- renderDT({
    validate(need(nrow(datos()) > 0, "Cargue o ingrese datos primero."))
    datatable(datos(), options = list(pageLength = 10), editable = TRUE)
  })
  
  # Actualizar ediciones en la tabla principal también
  observeEvent(input$tabla_cell_edit, {
    info <- input$tabla_cell_edit
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
  
  # UI de selección de variables
  output$varSelectUI <- renderUI({
    validate(need(nrow(datos()) > 0, "Cargue o ingrese datos primero."))
    
    vars <- names(datos())
    tagList(
      selectInput("yvar", "Variable dependiente (Y):", choices = vars),
      
      # Solo mostrar X para simple y nls, NO para múltiple
      conditionalPanel(
        condition = "input.modelType != 'multiple'",
        selectInput("xvar", "Variable independiente (X):", choices = vars)
      ),
      
      # Mostrar predictores múltiples solo para regresión múltiple
      conditionalPanel(
        condition = "input.modelType == 'multiple'",
        checkboxGroupInput("xvars", "Predictores múltiples:", choices = vars)
      )
    )
  })
  
  # Opciones de modelos no lineales
  output$modelOptionsUI <- renderUI({
    if (input$modelType == "nls") {
      selectInput("nlsType", "Modelo no lineal:",
                  choices = c(
                    "Exponencial: y = a * exp(b*x)" = "exp",
                    "Michaelis-Menten: y = (a*x)/(b+x)" = "mm",
                    "Polinomial no lineal (a + b*x + c*x^2)" = "poly2"
                  ))
    }
  })
  
  # Función auxiliar: preparar variable (permite numéricas Y categóricas)
  prepare_variable <- function(vec) {
    # Si ya es numérica, devolverla tal cual
    if (is.numeric(vec)) return(vec)
    
    # Si es factor, devolverlo tal cual (R lo maneja automáticamente)
    if (is.factor(vec)) return(vec)
    
    # Si es character, intentar convertir a numérico
    if (is.character(vec)) {
      num <- suppressWarnings(as.numeric(vec))
      # Si la conversión funciona para todos los valores no-NA, usar numérico
      if (!all(is.na(num[!is.na(vec)]))) {
        return(num)
      }
      # Si no es convertible, devolver como factor (variable categórica)
      return(as.factor(vec))
    }
    
    return(vec)
  }
  
  # Ejecutar modelo (con validaciones)
  modelo <- eventReactive(input$runModel, {
    validate(need(nrow(datos()) > 0, "Cargue o ingrese datos primero."))
    df <- datos()
    
    # Validar que no se elijan la misma variable para X y Y
    if (!is.null(input$yvar) && !is.null(input$xvar) && input$yvar == input$xvar) {
      validate(need(FALSE, "Seleccione variables diferentes para X y Y."))
    }
    
    # Simple
    if (input$modelType == "simple") {
      validate(need(input$yvar, "Selecciona la variable Y"))
      validate(need(input$xvar, "Selecciona la variable X"))
      
      y <- prepare_variable(df[[input$yvar]])
      x <- prepare_variable(df[[input$xvar]])
      
      # Y debe ser numérica siempre
      if (!is.numeric(y) || all(is.na(y))) {
        validate(need(FALSE, "La variable Y debe ser numérica y contener valores válidos."))
      }
      
      # X puede ser numérica o categórica (factor)
      if (all(is.na(x))) {
        validate(need(FALSE, "La variable X contiene solo valores NA."))
      }
      
      # construir el data frame limpio
      use_df <- data.frame(y = y, x = x)
      # eliminar filas con NA en cualquiera
      use_df <- na.omit(use_df)
      validate(need(nrow(use_df) >= 2, "No hay suficientes observaciones válidas para ajustar el modelo."))
      
      # ajustar y devolver lm
      return(lm(y ~ x, data = use_df))
    }
    
    # Multiple
    if (input$modelType == "multiple") {
      validate(need(input$yvar, "Selecciona la variable Y"))
      validate(need(input$xvars, "Selecciona predictores."))
      
      y <- prepare_variable(df[[input$yvar]])
      if (!is.numeric(y) || all(is.na(y))) {
        validate(need(FALSE, "La variable Y debe ser numérica y contener valores válidos."))
      }
      
      # construir data frame para formula - permite categóricas
      pred_names <- input$xvars
      # convertir cada predictor (puede ser numérico o categórico)
      preds <- lapply(pred_names, function(nm) prepare_variable(df[[nm]]))
      
      full_df <- data.frame(y = y, as.data.frame(preds))
      colnames(full_df) <- c("y", pred_names)
      full_df <- na.omit(full_df)
      validate(need(nrow(full_df) >= (length(pred_names) + 1), "No hay suficientes observaciones válidas para ajustar el modelo múltiple."))
      
      form <- as.formula(paste("y ~", paste(pred_names, collapse = "+")))
      return(lm(form, data = full_df))
    }
    
    # NLS (solo acepta variables numéricas)
    if (input$modelType == "nls") {
      validate(need(input$yvar, "Selecciona la variable Y"))
      validate(need(input$xvar, "Selecciona la variable X"))
      validate(need(input$nlsType, "Selecciona un tipo de NLS"))
      
      y <- prepare_variable(df[[input$yvar]])
      x <- prepare_variable(df[[input$xvar]])
      
      # Para NLS, ambas variables DEBEN ser numéricas
      if (!is.numeric(y) || all(is.na(y))) {
        validate(need(FALSE, "Para regresión no lineal, Y debe ser numérica."))
      }
      if (!is.numeric(x) || all(is.na(x))) {
        validate(need(FALSE, "Para regresión no lineal, X debe ser numérica."))
      }
      
      use_df <- data.frame(x = x, y = y)
      use_df <- na.omit(use_df)
      validate(need(nrow(use_df) >= 3, "No hay suficientes observaciones válidas para ajuste no lineal."))
      
      tryCatch({
        if (input$nlsType == "exp") {
          return(nls(y ~ a * exp(b * x), data = use_df, start = list(a = max(use_df$y), b = 0.05)))
        }
        if (input$nlsType == "mm") {
          return(nls(y ~ (a * x) / (b + x), data = use_df, start = list(a = max(use_df$y), b = mean(use_df$x))))
        }
        if (input$nlsType == "poly2") {
          return(nls(y ~ a + b * x + c * I(x^2), data = use_df, start = list(a = mean(use_df$y), b = 1, c = 0.01)))
        }
      }, error = function(e) {
        validate(need(FALSE, paste("Error en ajuste no lineal:", e$message, 
                                    "\nIntenta ajustar los valores iniciales o verifica tus datos.")))
      })
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
    df_app <- datos()
    
    # Si es lineal simple: graficar puntos + recta
    if (input$modelType == "simple") {
      # reconstruir x,y usados en ajuste (modelo usa nombres y,x)
      mf <- model.frame(modelo())
      # model.frame devuelve y e x con nombres originales
      yname <- names(mf)[1]
      xname <- names(mf)[2]
      ggplot(mf, aes_string(x = xname, y = yname)) +
        geom_point(size = 3, color = "steelblue") +
        geom_smooth(method = "lm", se = TRUE, color = "red") +
        theme_minimal() +
        labs(title = "Regresión Lineal Simple",
             x = input$xvar,
             y = input$yvar)
    } else if (input$modelType == "nls") {
      # Para modelos no lineales
      mf <- model.frame(modelo())
      x_vals <- mf[[2]]
      y_vals <- mf[[1]]
      
      # Crear secuencia para predicciones
      x_seq <- seq(min(x_vals), max(x_vals), length.out = 100)
      y_pred <- predict(modelo(), newdata = data.frame(x = x_seq))
      
      plot_df <- data.frame(x = x_vals, y = y_vals)
      pred_df <- data.frame(x = x_seq, y = y_pred)
      
      ggplot() +
        geom_point(data = plot_df, aes(x = x, y = y), size = 3, color = "steelblue") +
        geom_line(data = pred_df, aes(x = x, y = y), color = "red", size = 1) +
        theme_minimal() +
        labs(title = paste("Regresión No Lineal:", input$nlsType),
             x = input$xvar,
             y = input$yvar)
    } else {
      # para regresión múltiple, usar gráficos de diagnóstico
      par(mfrow = c(2, 2))
      plot(modelo())
    }
  })
  
  # Interpretación
  output$interpretacion <- renderPrint({
    req(modelo())
    cat("=== INTERPRETACIÓN DEL MODELO ===\n\n")
    
    cat("Coeficientes estimados:\n")
    print(coef(modelo()))
    
    cat("\n\nEcuación del modelo:\n")
    
    if (input$modelType == "simple") {
      coefs <- coef(modelo())
      cat(sprintf("%s = %.4f + %.4f * %s\n", 
                  input$yvar, coefs[1], coefs[2], input$xvar))
      
      if (class(modelo())[1] == "lm") {
        r2 <- summary(modelo())$r.squared
        cat(sprintf("\nR² = %.4f (el modelo explica el %.2f%% de la variabilidad)\n", 
                    r2, r2 * 100))
      }
    } else if (input$modelType == "multiple") {
      cat(sprintf("%s = ", input$yvar))
      coefs <- coef(modelo())
      cat(sprintf("%.4f", coefs[1]))
      for (i in 2:length(coefs)) {
        cat(sprintf(" + %.4f * %s", coefs[i], names(coefs)[i]))
      }
      cat("\n")
      
      r2 <- summary(modelo())$r.squared
      adj_r2 <- summary(modelo())$adj.r.squared
      cat(sprintf("\nR² = %.4f\nR² ajustado = %.4f\n", r2, adj_r2))
    } else if (input$modelType == "nls") {
      coefs <- coef(modelo())
      if (input$nlsType == "exp") {
        cat(sprintf("%s = %.4f * exp(%.4f * %s)\n", 
                    input$yvar, coefs["a"], coefs["b"], input$xvar))
      } else if (input$nlsType == "mm") {
        cat(sprintf("%s = (%.4f * %s) / (%.4f + %s)\n", 
                    input$yvar, coefs["a"], input$xvar, coefs["b"], input$xvar))
      } else if (input$nlsType == "poly2") {
        cat(sprintf("%s = %.4f + %.4f * %s + %.4f * %s²\n", 
                    input$yvar, coefs["a"], coefs["b"], input$xvar, coefs["c"], input$xvar))
      }
    }
  })
  
}