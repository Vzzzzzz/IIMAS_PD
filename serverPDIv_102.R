# Script: funciones utilizadas en el program de determinación de distribución
# de incertidumbre PDI Versión 1.0.0. Jueves 25 de febrero de 2025.
# Versión 1.0.1 Martes 11 de marzo de 2025.
# Versión 1.0.2 Jueves 20 de marzo de 2025.
library(magrittr)
library(shiny)
library(bslib)
library(ggplot2)
library(patchwork)
library(EnvStats)

server <- function(input, output, session) {
  
  #############################################################################
  ### F U N C I O N E S   D E   U I.
  #############################################################################
  
  #### V A L I D A C I Ó N   D E   I N P U T S.  ##############################
  observe({
    lista_inputs <- list(
      # Parámetros  
      "min" = list(rango = c(NA, input$max), 
                   mensaje = "El valor del parámetro debe ser menor al máximo del intervalo"),
      "max" = list(rango = c(input$min, NA), 
                   mensaje = "El valor del parámetro debe ser mayor al mínimo del intervalo"),
      "sigma_u" = list(rango = c(0.001, NA), 
                     mensaje = "El valor del parámetro debe ser mayor a cero"),
      "sigma" = list(rango = c(0.001, NA), 
                     mensaje = "El valor del parámetro debe ser mayor a cero"),
      "lambda" = list(rango = c(0.001, NA), 
                      mensaje = "El valor del parámetro debe ser mayor a cero"),
      "alpha" = list(rango = c(0.001, NA), 
                     mensaje = "El valor del parámetro debe ser mayor a cero"),
      "betav" = list(rango = c(0.001, NA), 
                     mensaje = "El valor del parámetro debe ser mayor a cero"),
      "forma1" = list(rango = c(0.001, NA), 
                      mensaje = "El valor del parámetro debe ser mayor a cero"),
      "forma2" = list(rango = c(0.001, NA), 
                      mensaje = "El valor del parámetro debe ser mayor a cero"),
      "gdl_chi" = list(rango = c(0.001, NA), 
                       mensaje = "Los grados de libertad deben ser un número natural mayor a cero"),
      "gdl_t" = list(rango = c(0.001, NA), 
                     mensaje = "Los grados de libertad deben ser un número natural mayor a cero"),
      "tau" = list(rango = c(0.001, NA), 
                   mensaje = "El valor del parámetro debe ser mayor a cero"),
      "theta" = list(rango = c(0.001, 0.999), 
                      mensaje = "El valor del parámetro debe ser menor al máximo del intervalo"),
      "sigma_t" = list(rango = c(0.001, NA),
                      mensaje = "El valor del parámetro debe ser mayor a cero"),
      "min_ta" = list(rango = c(NA, input$max_ta), 
                      mensaje = "El valor del parámetro debe ser menor al máximo del intervalo"),
      "max_ta" = list(rango = c(input$min_ta, NA), 
                      mensaje = "El valor del parámetro debe ser mayor al mínimo del intervalo"),
      "moda_ta" = list(rango = c(input$min_ta + 0.001, input$max_ta + 0.001),
                       mensaje = "El valor de la moda debe de estar contenido entre el mínimo y el máximo"),
      # Inputs
      "elevar" = list(rango = c(1, NA), 
                      mensaje = "El valor de la potencia debe ser mayor a 1"),
      "bajar" = list(rango = c(1, NA), 
                     mensaje = "El valor de la raíz debe ser mayor a 1"),
      "muestra" = list(rango = c(10, 100000000), 
                       mensaje = "El tamaño de muestra debe ser mayor que 10 y menor que 10e^8")
    )
    # Validación de límites de intervalos.
    invalido <- FALSE
    error <- NULL
    for (id in names(lista_inputs)) {
      rango <- lista_inputs[[id]]$rango
      if (!is.na(rango[1]) && (is.na(input[[id]]) || input[[id]] < rango[1])) {
        invalido <- TRUE
        error <- lista_inputs[[id]]$mensaje
        break  
      }
      if (!is.na(rango[2]) && (is.na(input[[id]]) || input[[id]] > rango[2])) {
        invalido <- TRUE
        error <- lista_inputs[[id]]$mensaje
        break  
      }
    }
    # Desactivación del botón de inicio y mensaje de error condicionado.
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
  
  #### M E N U   D E   T R A N S F O R M A C I O N E S.  ######################
  lista_funciones <- c("Lineal a*x + b", "Potencia x^n", "Logarítmica log(x)",
                       "Exponencial e^x", "Raíz x^(1/n)", "Logit log x/1-x"
                       )
  funciones_disponibles <- list(
    "Uniforme (mínimo, máximo)" = lista_funciones[c(1,2,4)],
    "Uniforme (media, desv. estándar)" = lista_funciones[c(1,2,4)],
    "Normal (media, desv. estándar)" = lista_funciones[c(1,2,4)],
    "Exponencial (tasa)" = lista_funciones[c(1,2,3,5)],
    "Gamma (forma, escala)" = lista_funciones[-6],
    "Beta (forma 1, forma 2)" = lista_funciones,
    "Chi-cuadrada (grados libertad)" = lista_funciones[-6],
    "T-Student (grados libertad)" = lista_funciones[c(1,2,4)],
    "Cauchy (locación, escala)" = lista_funciones[c(1,2,4)],
    "Bernoulli (probabilidad éxito)" = lista_funciones[c(1,2,4,5)],
    "Triangular (media, desv. estándar)" = lista_funciones[c(1,2,4)],
    "Triangular (mínimo, máximo, moda)" = lista_funciones[c(1,2,4)]
  )
  # Actualizar menu de funciones disponibles.
  observe({
    if (!is.null(input$distribucionX)) {
      updateSelectizeInput(session, "transformacion",
                           choices = funciones_disponibles[[input$distribucionX]],
                           selected = NULL)
    } else {
      updateSelectizeInput(session, "transformacion", choices = NULL)
    }
  })
  
  #############################################################################
  ### F U N C I O N E S   D E   S I M U L A C I Ó N.
  #############################################################################  
  
  ####  S I M U L A C I Ó N.   ################################################
  
  XY <- eventReactive(input$inicio, {
    # Semilla.
    if (!is.na(input$semilla)) {
      set.seed(input$semilla)
    }
    # Separación para gráfico.
    separacion <- 0.1
    if (input$distribucionX == "Beta (forma 1, forma 2)") {separacion <- 0.01}
    if (input$distribucionX == "Cauchy (locación, escala)") {separacion <- 500}
    # Selección de transformación.
    fx <- switch(input$transformacion,
                 "Lineal a*x + b" = function(x) x * input$pendiente + input$ordenada,
                 "Potencia x^n" = function(x) x^input$elevar,
                 "Logarítmica log(x)" = function(x) log(x),
                 "Exponencial e^x" = function(x) exp(x),
                 "Raíz x^(1/n)" = function(x) x^(1 / input$bajar),
                 "Logit log x/1-x" = function(x) log(x / (1 - x))
                 )
    # Inversa de transformación (Utilizada para el gráfico combinado)
    gx <- switch(input$transformacion,
                 "Lineal a*x + b" = function(x) (x - input$ordenada) / input$pendiente,
                 "Potencia x^n" = function(x) x^(1 / input$elevar),
                 "Logarítmica log(x)" = function(x) exp(x),
                 "Exponencial e^x" = function(x) log(x),
                 "Raíz x^(1/n)" = function(x) x^(input$bajar),
                 "Logit log 1/1-x" = function(x) exp(x) / (1 + exp(x))
                 )
    # Simulación de variable de entrada.
    X <- switch(input$distribucionX,
                "Uniforme (mínimo, máximo)" = runif(n = input$muestra,
                                   min = input$min,
                                   max = input$max),
                "Uniforme (media, desv. estándar)" = runif(
                  n = input$muestra,
                  min = input$mu_u - (sqrt(12) * input$sigma_u)/2,
                  max = input$mu_u + (sqrt(12) * input$sigma_u)/2),
                "Normal (media, desv. estándar)" = rnorm(n = input$muestra,
                                 mean = input$mu,
                                 sd = input$sigma),
                "Exponencial (tasa)" = rexp(n = input$muestra,
                                     rate = input$lambda),
                "Gamma (forma, escala)" = rgamma(n = input$muestra,
                                 shape = input$alpha,
                                 scale = input$betav),
                "Beta (forma 1, forma 2)" = rbeta(n = input$muestra,
                               shape1 = input$forma1,
                               shape2 = input$forma2),
                "Chi-cuadrada (grados libertad)" = rchisq(n = input$muestra,
                                        df = input$gdl_chi),
                "T-Student (grados libertad)" = rt(n = input$muestra,
                                 df = input$gdl_t),
                "Cauchy (locación, escala)" = rcauchy(n = input$muestra,
                                   location = input$iota,
                                   scale = input$tau),
                "Bernoulli (probabilidad éxito)" = rbinom(n = input$muestra,
                                     size = 1,
                                     prob = input$theta),
                "Triangular (media, desv. estándar)" = rtri(
                  n = input$muestra, 
                  min = input$mu_t - (sqrt(24) * input$sigma_t)/2 , 
                  max = input$mu_t + (sqrt(24) * input$sigma_t)/2, 
                  mode = input$mu_t
                  ),
                "Triangular (mínimo, máximo, moda)" = rtri(
                  n = input$muestra, 
                  min = input$min_ta, 
                  max = input$max_ta, 
                  mode = input$moda_ta
                  ),
                )
    # Media teórica distribución de entrada X.
    media_x <- switch(input$distribucionX,
                      "Uniforme (mínimo, máximo)" = (input$min + input$max) / 2,
                      "Uniforme (media, desv. estándar)" = input$mu_u,
                      "Normal (media, desv. estándar)" = input$mu,
                      "Exponencial (tasa)" = 1 / input$lambda,
                      "Gamma (forma, escala)" = input$alpha * input$betav,
                      "Beta (forma 1, forma 2)" = input$forma1 / (input$forma1 + input$forma2),
                      "Chi-cuadrada (grados libertad)" = input$gdl_chi,
                      "T-Student (grados libertad)" = 0,
                      "Cauchy (locación, escala)" = median(X),
                      "Bernoulli (probabilidad éxito)" = input$theta,
                      "Triangular (media, desv. estándar)" = input$mu_t,
                      "Triangular (mínimo, máximo, moda)" = (input$min_ta + input$max_ta + input$moda_ta)/3
                      )
    # Aplicación de transformación a X.
    Y <- fx(X)
    return(list("X" = X, "Y" = Y, "fx" = fx, "gx" = gx, "media_x" = media_x, "sep" = separacion))
    }
  )
  
  #### E S T A D Í S T I C A S.  ##############################################
  
  # Cálculo de media de Y.
  output$mediaY <- renderText({
    req(XY())
    round(x = mean(XY()$Y), digits = 2)
    })
  # Cálculo de media de X.
  output$mediaX <- renderText({
    req(XY())
    round(x = mean(XY()$X), digits = 2)
  })
  # Cálculo desviacion estandar de Y.
  output$desv_estY <- renderText({
    req(XY())
    round(x = sd(XY()$Y), digits = 2)
    })
  # Cálculo desviacion estandar de X.
  output$desv_estX <- renderText({
    req(XY())
    round(x = sd(XY()$X), digits = 2)
  })
  # Bondad de ajuste Kolmogorov-Smirnov de Y respecto Normal.
  output$bondad_ajuste <- renderUI({
    req(XY())
    p_value <- round(
      x = as.numeric(ks.test(x = XY()$Y, 
                             "pnorm", 
                             mean(XY()$Y), 
                             var(XY()$Y))$p.value), digits = 4)
    # Mensaje de resultado negativo.
    if (p_value < 0.05) {
      value_box(title = "¿Y se distribuye normal?",
                value = "No",
                max_height = "110px",
                min_height = "50px",
                theme = "danger") |>
        tooltip("Conclusión obtenida a partir de la aplicación de prueba de 
                bondad de ajuste Kolmogorov-Smirnov para normalidad con p-value 
                de 0.05.",
                placement = "bottom"
                )
    }
    # Mensaje de resultado positivo.
    else {
      value_box(title = "¿Y se distribuye normal?",
                value = "Sí",
                max_height = "110px",
                min_height = "50px",
                theme = "success") |>
        tooltip("Conclusión obtenida a partir de la aplicación de prueba de 
                bondad de ajuste Kolmogorov-Smirnov para normalidad con p-value 
                de 0.05.",
                placement = "bottom"
        )
    }
  })
  # "Bondad de ajuste" de X respecto Normal.
  output$normalX <- renderUI({
    req(XY())
    # Mensaje de resultado positivo.
    if (input$distribucionX == "Normal (media, desv. estándar)") {
      value_box(title = "¿X se distribuye normal?",
                value = "Sí",
                max_height = "110px",
                min_height = "50px",
                theme = "secondary",
                class = "text-success")
    }
    else {
      value_box(title = "¿X se distribuye normal?",
                value = "No",
                max_height = "110px",
                min_height = "50px",
                theme = "secondary",
                class = "text-danger")
    }
  })
  # Tablita dinámica intervalos cobertura slider.
  output$tabla_intervalos <- renderTable(
    striped = TRUE,
    digits = 2,
    align = "c",
    spacing = "xs",{
      req(XY())
      intervaloSY <- round(quantile(XY()$Y, 
                     probs = c((1-input$coberturaS)/2, 1-(1-input$coberturaS)/2)),
                     digits = 2)
      intervaloSX <- round(quantile(XY()$X, 
                                    probs = c((1-input$coberturaS)/2, 1-(1-input$coberturaS)/2)),
                           digits = 2)
      data.frame("Intervalo_Y" = paste0("( ", intervaloSY[1], " , ", intervaloSY[2], " )"),
                 "Intervalo_X" = paste0("( ", intervaloSX[1], " , ", intervaloSX[2], " )"))
    }
  )
  # Tabla de sumario de estadísticas.
  output$tabla_sumario <- renderTable(
    striped = TRUE,
    digits = 4,
    align = "c",
    spacing = "xs",
    class = "primary",
    {
      req(XY())
      df_sumario <- data.frame(
        "Estadística" = c("Media", "Desviación estandar", "Mediana", "MAD"),
        "Variable_Y" = c(mean(XY()$Y), sd(XY()$Y), median(XY()$Y), mad(XY()$Y)),
        "Variable_X" = c(mean(XY()$X), sd(XY()$X), median(XY()$X), mad(XY()$X))
      )
    })
  # Tabla intervalos de cobertura.
  output$tabla_cobertura <- renderTable(
    striped = TRUE,
    digits = 2,
    align = "c",
    spacing = "xs",
    {
      req(XY())
      int_minX <- round(
        quantile(XY()$X, probs = c(0.01/2, 0.05/2, 0.1/2, 0.32/2)),
        digits = 2
      )
      int_maxX <- round(
        quantile(XY()$X, probs = c(1-0.01/2, 1-0.05/2, 1-0.1/2, 1-0.32/2)),
        digits = 2
      )
      int_minY <- round(
        quantile(XY()$Y, probs = c(0.01/2, 0.05/2, 0.1/2, 0.32/2)),
        digits = 2
        )
      int_maxY <- round(
        quantile(XY()$Y, probs = c(1-0.01/2, 1-0.05/2, 1-0.1/2, 1-0.32/2)),
        digits = 2
        )
      df_cobertura <- data.frame(
        "Cobertura" = c("99%", "95%", "90%", "68%"),
        "Intervalo_Y" = paste0("( ", int_minY, " , ", int_maxY, " )"),
        "Intervalo_X" = paste0("( ", int_minX, " , ", int_maxX, " )")
      )
    })
  
  #### G R A F I C A   C O M B I N A D A.  ####################################
  
  output$grafica_combinada <- renderPlot({
    req(XY())
    # transformación e inversa de transformación.
    fx <- as.function(XY()$fx)
    gx <- as.function(XY()$gx) 
    # medias.
    media_x <- as.numeric(XY()$media_x)
    media_y <- as.numeric(mean(XY()$Y))
    # Límites de escala de gráficas.
    limiteX <- range(XY()$X)
    limiteY <- range(XY()$Y)
    # Puntos mínimos y máximos para segmentos del gráfico.
    minimoX <- as.numeric(min(XY()$X))
    minimoY <- as.numeric(min(XY()$Y))
    maximoX <- as.numeric(max(XY()$X))
    maximoY <- as.numeric(max(XY()$Y))
    # Intervalos de cobertura interactivos.
    interX <- round(
      quantile(XY()$X, probs = c((1-input$coberturaS)/2, 1-(1-input$coberturaS)/2)),
      digits = 2
      )
    interY <- round(
      quantile(XY()$Y, probs = c((1-input$coberturaS)/2, 1-(1-input$coberturaS)/2)),
      digits = 2
      )
    df <- data.frame("X" = XY()$X, "Y" = XY()$Y)
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
    histograma_X <- ggplot(df[1], aes(x = X)) +
      geom_histogram(binwidth = XY()$sep, 
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
    histograma_Y <- ggplot(df[2], aes(y = Y)) +
      geom_histogram(binwidth = XY()$sep, 
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
  
  
} # fin server.




