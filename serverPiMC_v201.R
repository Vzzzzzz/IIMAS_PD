# Script: funciones utilizadas en el program de determinación de distribución
# de incertidumbre PDI Versión 2.0.0. Jueves 3 de abril de 2025.
library(magrittr)
library(shiny)
library(bslib)
library(tidyr)
library(ggplot2)
library(patchwork)
library(EnvStats)
library(rlang) # Errores
library(reshape2)
source("contenidoPiMC_v201.R")

###

server <- function(input, output, session) {
  # Número de variables de entrada.
  nV <- reactive({
    if (input$navegacion == "PiMC-e") {return(1)}
    if (input$navegacion == "PiMC-mv") {as.numeric(input$nVariables)}
  })
  
  ##### S E L E C C I O N   D I S T R I B U C I O N   X_i. #####
  output$distribucionVariables <- renderUI({
    req(input$navegacion, nV())
    lapply(1:nV(), function(i) funcion_distribucion_X(i, input))
  })

  ##### V A L I D A C I Ó N  D E  P A R Á M E T R O S.  #######################
  observe({
    req(nV())
    n <- nV()
    invalido <- FALSE
    error <- NULL
    for (i in 1:n) {
      id_distribucion <- input[[paste0("distribucionX", i)]]
      req(id_distribucion)
      parametros_distribucion <- diccionario_parametros[[id_distribucion]]
      if (id_distribucion == "unif1") {
        parametros_distribucion[[1]]$rango[[2]] <- input[[paste0("max", i)]]
        parametros_distribucion[[2]]$rango[[1]] <- input[[paste0("min", i)]]
      }
      if (id_distribucion == "tri1"){
        parametros_distribucion[[1]]$rango[[2]] <- input[[paste0("max_ta", i)]]
        parametros_distribucion[[2]]$rango[[1]] <- input[[paste0("min_ta", i)]]
        parametros_distribucion[[3]]$rango[[2]] <- input[[paste0("max_ta", i)]]
        parametros_distribucion[[3]]$rango[[1]] <- input[[paste0("min_ta", i)]]
      }
      for (sublista in parametros_distribucion) {
        parametro_i <- input[[paste0(sublista$parametro, i)]] 
        # Verificación del rango
        if (is.na(parametro_i) || 
            (!is.na(sublista$rango[1]) && parametro_i <= sublista$rango[1]) ||
            (!is.na(sublista$rango[2]) && parametro_i >= sublista$rango[2])) 
          {invalido <- TRUE
          error <- sublista$mensaje
          break
          }
      }
      if (invalido) { break }
    }
    # Desactivación del botón de inicio y mensaje de error condicionado
    updateActionButton(session = session, 
                       inputId = "inicio", 
                       disabled = invalido)
    if (invalido) {
      showNotification(ui = error,
                       session = session,
                       duration = 7,
                       closeButton = TRUE,
                       type = "error")
    }
  })
  
  ##### V A L I D A C I Ó N  D E  T R A N S F O R M A C I Ó N  X.  ############
  observe({
    req(input$navegacion == 'PiMC-e', input$distribucionX1)
    updateSelectizeInput(session, "transformacion_e",
                         choices = funciones_disponibles[[input$distribucionX1]],
                         selected = NULL)
  })
  
  ##### V A L I D A C I Ó N  D E  T R A N S F O R M A C I Ó N  X_i.  ##########
  observe({
    req(input$navegacion == 'PiMC-mv', input$transformacion_mv, nV())
    n <- nV()
    fx <- input$transformacion_mv
    # Entorno necesario para poder leer la sintaxis de la transformación.
    entorno <- new.env()
    for (i in 1:n) {
      assign(paste0("x_", i), 1, envir = entorno)
    }
    invalido <- tryCatch({
      eval(parse(text = fx), envir = entorno)
      FALSE
      }, 
    error = function(e) { TRUE }
    )
    # Desactivación del botón de inicio y mensaje de error condicionado
    updateActionButton(session = session,
                       inputId = "inicio",
                       disabled = invalido)
    if (invalido) {
      showNotification(
        ui = "La transformación tiene errores de sintaxis o variables no definidas.",
        duration = 7,
        closeButton = TRUE,
        type = "error")
      }
  })

#####  S I M U L A C I Ó N. ###################################################
  pagina <- reactiveVal(NULL)
  observeEvent(input$inicio, {pagina(input$navegacion)})

  SnY <- eventReactive(input$inicio, {
    # Semilla.
    if (!is.na(input$semilla)) {
      set.seed(input$semilla)
    }
    n <- nV()
    # Simulación e.
    if (pagina() == "PiMC-e"){
      X <- lapply(1:n, function(i) funcion_simulacion_X(i, input))
      # Selección de transformación.
      fx <- switch(input$transformacion_e,
                   "lin" = function(x) x * input$pendiente + input$ordenada,
                   "pot" = function(x) x^input$elevar,
                   "log" = function(x) log(x),
                   "expo" = function(x) exp(x),
                   "raiz" = function(x) x^(1 / input$bajar),
                   "logit" = function(x) log(x / (1 - x))
      )
      # Inversa de transformación (Utilizada para el gráfico combinado)
      gx <- switch(input$transformacion_e,
                   "lin" = function(x) (x - input$ordenada) / input$pendiente,
                   "pot" = function(x) x^(1 / input$elevar),
                   "log" = function(x) exp(x),
                   "expo" = function(x) log(x),
                   "raiz" = function(x) x^(input$bajar),
                   "logit" = function(x) exp(x) / (1 + exp(x))
      )
      # Media teórica distribución de entrada X.
      media_x <- switch(input$distribucionX1,
                        "norm" = input$mu1,
                        "unif1" = (input$min1 + input$max1) / 2,
                        "unif2" = input$mu_u1,
                        "tri2" = input$mu_t1,
                        "tri1" = (input$min_ta1 + input$max_ta1 + input$moda_ta1)/3,
                        "exp" = 1 / input$lambda1,
                        "gamm" = input$alpha1 * input$betav1,
                        "beta" = input$forma11 / (input$forma11 + input$forma21),
                        "chi" = input$gdl_chi1,
                        "ts" = 0,
                        "cau" = median(X[[1]]),
                        "ber" = input$theta1
      )
      # Aplicación de transformación a X.
      Y <- fx(X[[1]])
      Sn <- data.frame(Y, X[[1]])
      names(Sn) <- c("Y", "X")
      return(list("Sn" = Sn, "fx" = fx, "gx" = gx, "media_x" = media_x))
    }
    # Simulación mv.
    if (pagina() == "PiMC-mv") {
      Sn <- lapply(1:n, function(i) funcion_simulacion_X(i, input))
      req(input$transformacion_mv)
      fx <- input$transformacion_mv
      # Creación data frame.
      names(Sn) <- paste0("x_", 1:n)
      Sn <- data.frame(Sn)
      # Aplicación de transformación a X.
      Y <- tryCatch({eval(parse(text = fx), envir = Sn)},
                    error = function(e) { rep(NA, input$muestra) })
      Sn <- cbind(Y = Y, Sn)
      return(Sn)
    }
  })
  
  ##### E S T A D Í S T I C A S.  #############################################
  # Cálculo de media de Y.
  output$mediaY_e <- renderText({
    req(pagina() == 'PiMC-e')
    round(x = mean(SnY()$Sn$Y), digits = 2)
  })
  output$mediaY_mv <- renderText({
    req(pagina() == 'PiMC-mv')
    round(x = mean(SnY()$Y), digits = 2)
  })
  # Cálculo desviacion estandar de Y.
  output$desv_estY_e <- renderText({
    req(pagina() == 'PiMC-e')
    round(x = sd(SnY()$Sn$Y), digits = 2)
  })
  output$desv_estY_mv <- renderText({
    req(pagina() =='PiMC-mv')
    round(x = sd(SnY()$Y), digits = 2)
  })
  # Cálculo de media de X.
  output$mediaX <- renderText({
    req(pagina() == 'PiMC-e')
    round(x = mean(SnY()$Sn$X), digits = 2)
  })
  # Cálculo desviacion estandar de X.
  output$desv_estX <- renderText({
    req(pagina() == 'PiMC-e')
    round(x = sd(SnY()$Sn$X), digits = 2)
  })
  # Bondad de ajuste Kolmogorov-Smirnov de Y respecto Normal X.
  output$bondad_ajuste_e <- renderUI({
    req(pagina() == 'PiMC-e',SnY())
    df <- SnY()$Sn
    p_value <- round(x = as.numeric(ks.test(x = df$Y, 
                                            "pnorm", 
                                            mean(df$Y), 
                                            var(df$Y))$p.value), digits = 4
                     )
    resultado_pvalue(p_value)
  })
  # Bondad de ajuste Kolmogorov-Smirnov de Y respecto Normal X_i.
  output$bondad_ajuste_mv <- renderUI({
    req(pagina() == 'PiMC-mv',SnY())
    df <- SnY()
    p_value <- round(x = as.numeric(ks.test(x = df$Y, 
                                            "pnorm", 
                                            mean(df$Y), 
                                            var(df$Y))$p.value), digits = 4
    )
    resultado_pvalue(p_value)
  })
  # "Bondad de ajuste" de X respecto Normal.
  output$normalX <- renderUI({
    req(pagina() == 'PiMC-e')
    # Mensaje de resultado positivo.
    if (input$distribucionX1 == "norm") {
      value_box(title = "¿Se distribuye normal?",
                value = "Sí",
                max_height = "115px",
                min_height = "90px",
                height = "100px",
                theme = "secondary",
                class = "text-success")
    }
    else {
      value_box(title = "¿Se distribuye normal?",
                value = "No",
                max_height = "115px",
                min_height = "90px",
                height = "100px",
                theme = "secondary",
                class = "text-danger")
    }
  })
  # Tabla de sumario de estadísticas X.
  output$tabla_sumario_e <- renderTable(
    striped = TRUE,
    digits = 4,
    align = "c",
    spacing = "xs",
    class = "primary",
    {
      req(pagina() == 'PiMC-e',SnY())
      df <- SnY()$Sn
      estadisticas_general(df)
    })
  # Tabla de sumario de estadísticas X_i.
  output$tabla_sumario_mv <- renderTable(
    striped = TRUE,
    digits = 4,
    align = "c",
    spacing = "xs",
    class = "primary",
    {
      req(pagina() == 'PiMC-mv',SnY())
      df <- SnY()
      estadisticas_general(df)
    })
  # Tabla intervalos de cobertura X_i.
  output$tabla_cobertura_mv <- renderTable(
    striped = TRUE,
    digits = 2,
    align = "c",
    spacing = "xs",{
      req(pagina() == 'PiMC-mv', SnY())
      df <- SnY() 
      return(tabla_cobertura_general(df))
    }
  )
  # Tabla intervalos de cobertura X.
  output$tabla_cobertura_e <- renderTable(
    striped = TRUE,
    digits = 2,
    align = "c",
    spacing = "xs",{
      req(pagina() == 'PiMC-e', SnY())
      df <- SnY()$Sn 
      return(tabla_cobertura_general(df))
    }
  )
  # Tablita dinámica intervalos cobertura slider.
  output$tabla_intervalos <- renderTable(
    striped = TRUE,
    bordered = TRUE,
    digits = 2,
    align = "c",
    spacing = "xs",{
      req(pagina() == 'PiMC-e', SnY())
      df <- SnY()$Sn
      intervaloSY <- round(quantile(df$Y, 
                                    probs = c((1-input$coberturaS)/2, 1-(1-input$coberturaS)/2)),
                           digits = 2)
      intervaloSX <- round(quantile(df$X, 
                                    probs = c((1-input$coberturaS)/2, 1-(1-input$coberturaS)/2)),
                           digits = 2)
      data.frame("Intervalo_Y" = paste0("( ", intervaloSY[1], " , ", intervaloSY[2], " )"),
                 "Intervalo_X" = paste0("( ", intervaloSX[1], " , ", intervaloSX[2], " )"))
    }
  )
  # Tabla de correlación Pearson.
  output$correlacion_pearson <- renderTable(
    striped = TRUE,
    bordered = TRUE,
    digits = 2,
    align = "c",
    spacing = "xs",{
      req(pagina() == "PiMC-mv", SnY())
      df_corr_pearson <- data.frame(cor(x = SnY(), method = "pearson"))
    })
  # Tabla de correlación Spearman.
  output$correlacion_spearman <- renderTable(
    striped = TRUE,
    bordered = TRUE, 
    digits = 2,
    align = "c",
    spacing = "xs",{
      req(pagina() == "PiMC-mv", SnY())
      df_corr_pearson <- data.frame(cor(x = SnY(), method = "spearman"))
    })
  
  ##### G R A F I C A   C O M B I N A D A   X.  ###############################
  output$grafica_combinada_e <- renderPlot({
    req(pagina() == 'PiMC-e', SnY())
    # transformación e inversa de transformación.
    fx <- as.function(SnY()$fx)
    gx <- as.function(SnY()$gx) 
    df <- SnY()$Sn
    # medias.
    media_x <- as.numeric(SnY()$media_x)
    media_y <- as.numeric(mean(df$Y))
    # Límites de escala de gráficas.
    limiteX <- range(df$X)
    limiteY <- range(df$Y)
    # Puntos mínimos y máximos para segmentos del gráfico.
    minimoX <- as.numeric(min(df$X))
    minimoY <- as.numeric(min(df$Y))
    maximoX <- as.numeric(max(df$X))
    maximoY <- as.numeric(max(df$Y))
    # Intervalos de cobertura interactivos.
    interX <- round(
      quantile(df$X, probs = c((1-input$coberturaS)/2, 1-(1-input$coberturaS)/2)),
      digits = 2
    )
    interY <- round(
      quantile(df$Y, probs = c((1-input$coberturaS)/2, 1-(1-input$coberturaS)/2)),
      digits = 2
    )
    # Gráfica f(X).
    plot_fx <- ggplot(df, aes(x = X, y = Y)) +
      geom_point(alpha = 0) +
      stat_function(fun = fx, 
                    color = "#3459E6", 
                    linewidth = 1) +
      # Media e intervalos de cobertura X.
      annotate("segment", 
               x = media_x, 
               y = minimoY, 
               xend = media_x, 
               yend = fx(media_x), 
               color = "#ff5097",
               linetype = "dashed",
               linewidth = 1.5) +
      annotate("segment", 
               x = interX[1], 
               y = minimoY, 
               xend = interX[1], 
               yend = fx(interX[1]), 
               color = "#9994ca",
               linetype = "twodash",
               linewidth = 1.5) +
      annotate("segment", 
               x = interX[2], 
               y = minimoY, 
               xend = interX[2], 
               yend = fx(interX[2]), 
               color = "#9994ca",
               linetype = "twodash",
               linewidth = 1.5) +
      # Media e intervalos de cobertura Y.
      annotate("segment", 
               x = gx(media_y), 
               y = media_y, 
               xend = minimoX, 
               yend = media_y, 
               color = "#ff8d67",
               linetype = "dashed",
               linewidth = 1.5) +
      annotate("segment", 
               x = gx(interY[1]), 
               y = interY[1], 
               xend = minimoX, 
               yend = interY[1], 
               color = "#ffc79d",
               linetype = "twodash",
               linewidth = 1.5) +
      annotate("segment", 
               x = gx(interY[2]), 
               y = interY[2], 
               xend = minimoX, 
               yend = interY[2], 
               color = "#ffc79d",
               linetype = "twodash",
               linewidth = 1.5) +
      # Puntos.
      annotate("point", 
               x = media_x, 
               y = fx(media_x), 
               color = "#ff5097", 
               size = 4) +
      annotate("point", 
               x = interX[1], 
               y = fx(interX[1]), 
               color = "#9994ca", 
               size = 4) +
      annotate("point", 
               x = interX[2], 
               y = fx(interX[2]), 
               color = "#9994ca", 
               size = 4) +
      annotate("point", 
               x = gx(media_y), 
               y = media_y, 
               color = "#ff8d67", 
               size = 4) +
      annotate("point", 
               x = gx(interY[1]), 
               y = interY[1], 
               color = "#ffc79d", 
               size = 4) +
      annotate("point", 
               x = gx(interY[2]), 
               y = interY[2], 
               color = "#ffc79d", 
               size = 4) +
      labs(title = "f(X)") + coord_cartesian(xlim = limiteX, ylim = limiteY) +
      theme_minimal() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank()
      )
    # Histograma X.
    histograma_X <- ggplot(df[2], aes(x = X)) +
      geom_histogram(bins = 30, 
                     fill = "#3459E6",
                     alpha = 0.8,
                     color = "#3459E6") +
      geom_vline(xintercept = media_x, 
                 color = "#ff5097", 
                 linetype = "dashed", 
                 linewidth = 1.5) +
      geom_vline(xintercept = interX[1], 
                 linetype = "twodash", 
                 color = "#9994ca", 
                 linewidth = 1.5) +
      geom_vline(xintercept = interX[2], 
                 linetype = "twodash", 
                 color = "#9994ca", 
                 linewidth = 1.5) +
      labs(title = "X") + coord_cartesian(xlim = limiteX) +
      theme_minimal() +
      theme(axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.ticks.x = element_blank()
      )
    # Histograma de Y 
    histograma_Y <- ggplot(df[1], aes(y = Y)) +
      geom_histogram(bins = 30, 
                     fill = "#3459E6",
                     alpha = 0.8,
                     color = "#3459E6") +
      geom_hline(yintercept = media_y, 
                 color = "#ff8d67", 
                 linetype = "dashed", 
                 linewidth = 1.5) +
      geom_hline(yintercept = interY[1], 
                 linetype = "twodash", 
                 color = "#ffc79d", 
                 linewidth = 1.5) +
      geom_hline(yintercept = interY[2], 
                 linetype = "twodash", 
                 color = "#ffc79d", 
                 linewidth = 1.5) +
      labs(title = "Y") + coord_cartesian(ylim = limiteY) +
      theme_minimal() + scale_x_reverse() +
      theme(axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()
      )
    # Etiquetas.
    plot_etiquetas <- ggplot() + 
      annotate("rect", xmin = 0, xmax = 0.1, ymin = 0.8, ymax = 1.2, fill = "#ffc79d", alpha = 1) +
      annotate("rect", xmin = 0, xmax = 0.1, ymin = 1.3, ymax = 1.7, fill = "#ff8d67", alpha = 1) +
      annotate("rect", xmin = 0, xmax = 0.1, ymin = 1.8, ymax = 2.2, fill = "#9994ca", alpha = 1) +
      annotate("rect", xmin = 0, xmax = 0.1, ymin = 2.3, ymax = 2.7, fill = "#ff5097", alpha = 1) +
      annotate("text", x = c(0.05, 0.05, 0.05, 0.05), y = c(1, 1.5, 2, 2.5), size = 6,
               label = c("I.C.Y", "MediaY", "I.C.X", "MediaX"), color = "white") +
      theme_void()
    # Arreglo.
    plot_final <- histograma_Y + plot_fx + plot_etiquetas + histograma_X + 
      plot_layout(ncol = 2, nrow = 2, widths = c(2, 3), 
                  heights = c(3, 1.8), guides = "collect")
    return(plot_final)
  })
  
  ##### G R A F I C A   C O M B I N A D A   X_i.  #####
  output$grafica_combinada_mv <- renderPlot({
    req(pagina() == 'PiMC-mv', input$nVariables, SnY())
    n <- as.numeric(input$nVariables)
    df <- SnY()
    columnas <- paste0("x_", 1:n)
    df_histogramaX <- df[-c(1)] |>
      pivot_longer(cols = columnas, names_to = "variable", values_to = "valor")
    
    # Histogramas variables de entrada X_i.
    histogramaXi <- ggplot(df_histogramaX, aes(x = valor)) +
      geom_histogram(bins = 30, fill = "#3459E6", color = "#3459E6", alpha = 0.4)  +
      facet_wrap(~ variable, scales = "free", ncol = 2) +
      theme_minimal() +
      theme(axis.title.y = element_blank(),
            axis.title.x = element_blank())
    # Histograma variable de salida Y.
    df_histogramaY <- data.frame(Y = df$Y)
    histogramaY <- ggplot(df_histogramaY, aes(x = Y)) +
      geom_histogram(bins = 30, fill = "#3459E6", color = "#3459E6", alpha = 0.8) +
      theme_minimal() + labs(title = "Y") +
      theme(axis.title.y = element_blank(),
            axis.title.x = element_blank())
    # histograma final.
    plot_final <- histogramaY + histogramaXi +
      plot_layout(ncol = 2, widths = c(3,2))
    
    return(plot_final)
  })
  
  ##### H E A T   M A P S   X_i.  #####
  # Heat Map correlación Pearson.
  output$heatmap_pearson <- renderPlot({
    req(pagina() == 'PiMC-mv', SnY())
    df <- melt(cor(SnY(), method = "pearson"))
    hm_plot <- ggplot(data = df, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile(color = "white", lwd = 1) + theme_minimal() + coord_fixed() + 
      labs(fill = "Correlación") +
      scale_fill_gradient2(high = "#3459E6", low = "#D5392E", limits = c(-1, 1)) +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_text(size = 14, face = "bold"),
            axis.title.y = element_blank(),
            axis.text.y = element_text(size = 14, face = "bold")
            )
    return(hm_plot)
  })
  # Heat Mao correlación Spearman.
  output$heatmap_spearman <- renderPlot({
    req(pagina() == 'PiMC-mv', SnY())
    df <- melt(cor(SnY(), method = "spearman"))
    hm_plot <- ggplot(data = df, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile(color = "white", lwd = 1) + theme_minimal() + coord_fixed() +
      labs(fill = "Correlación") +
      scale_fill_gradient2(high = "#3459E6", low = "#D5392E", limits = c(-1, 1)) +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_text(size = 14, face = "bold"),
            axis.title.y = element_blank(),
            axis.text.y = element_text(size = 14, face = "bold")
            )
    return(hm_plot)
  })
  
  
  
  
}