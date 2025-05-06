# Script: sidebars y panels utilizado en el programa de distribución de incertidumbre
# PDI Versión 2.0.0. Jueves 3 de abril de 2025.
library(shiny)
library(bslib)

###############################################################################
#### L I S T A S.
###############################################################################
# Vector con nombres de distribuciones.
lista_distribuciones <- list("Normal (media, desv. estándar)" = "norm",
                             "Uniforme (mínimo, máximo)" = "unif1",
                             "Uniforme (media, desv. estándar)" = "unif2", 
                             "Triangular (mínimo, máximo, moda)" = "tri1",
                             "Triangular (media, desv. estándar)" = "tri2", 
                             "Exponencial (tasa)" = "exp", 
                             "Gamma (forma, escala)" = "gamm", 
                             "Beta (forma 1, forma 2)" = "beta", 
                             "Chi-cuadrada (grados libertad)" = "chi", 
                             "T-Student (grados libertad)" = "ts", 
                             "Cauchy (locación, escala)" = "cau", 
                             "Bernoulli (probabilidad éxito)" = "ber"
                             )
# Vector con nombres de transformaciones.
lista_funciones <- c("Lineal a*x + b" = "lin", 
                     "Potencia x^n" = "pot", 
                     "Logarítmica log(x)" = "log",
                     "Exponencial e^x" = "expo", 
                     "Raíz x^(1/n)" = "raiz", 
                     "Logit log x/1-x" = "logit"
                     )
# Menú funciones disponibles
funciones_disponibles <- list(
  "norm" = lista_funciones[c(1,2,4)],
  "unif1" = lista_funciones[c(1,2,4)],
  "unif2" = lista_funciones[c(1,2,4)],
  "tri1" = lista_funciones[c(1,2,4)],
  "tri2" = lista_funciones[c(1,2,4)],
  "exp" = lista_funciones[c(1,2,3,5)],
  "gamm" = lista_funciones[-6],
  "beta" = lista_funciones,
  "chi" = lista_funciones[-6],
  "ts" = lista_funciones[c(1,2,4)],
  "cau" = lista_funciones[c(1,2,4)],
  "ber" = lista_funciones[c(1,2,4,5)]
  )
# Slider para determinar procentaje del intervalo de cobertura.
slider_confianza <- sliderInput(inputId = "coberturaS",
                                label = NULL,
                                min = 0.66,
                                max = 0.99,
                                value = 0.95,
                                step = 0.01,
                                ticks = FALSE)
# Diccionario parámetros.
diccionario_parametros <- list(
  "norm" = list(
    list(parametro = "mu", rango = c(NA, NA), 
         mensaje = "Introduce un valor válido"),
    list(parametro = "sigma", rango = c(0.001, NA), 
         mensaje = "El valor del parámetro debe ser mayor a cero")
  ),
  "unif1" = list(
    list(parametro = "min", rango = c(NA, NA),  
         mensaje = "El valor del parámetro debe ser menor al máximo del intervalo"),
    list(parametro = "max", rango = c(NA, NA),  
         mensaje = "El valor del parámetro debe ser mayor al mínimo del intervalo")
  ),
  "unif2" = list(
    list(parametro = "mu_u", rango = c(NA, NA), 
         mensaje = "Introduce un valor válido"),
    list(parametro = "sigma_u", rango = c(0.001, NA), 
         mensaje = "El valor del parámetro debe ser mayor a cero")
  ), 
  "tri1" = list(
    list(parametro = "min_ta", rango = c(NA, NA), 
         mensaje = "El valor del parámetro debe ser menor al máximo del intervalo"),
    list(parametro = "max_ta", rango = c(NA, NA), 
         mensaje = "El valor del parámetro debe ser mayor al mínimo del intervalo"),
    list(parametro = "moda_ta", rango = c(NA, NA),
         mensaje = "El valor de la moda debe de estar contenido entre el mínimo y el máximo")
  ),
  "tri2" = list(
    list(parametro = "mu_t", rango = c(NA, NA), 
         mensaje = "Introduce un valor válido"),
    list(parametro = "sigma_t", rango = c(0.001, NA),
         mensaje = "El valor del parámetro debe ser mayor a cero")
  ), 
  "exp" = list(
    list(parametro = "lambda", rango = c(0.001, NA), 
         mensaje = "El valor del parámetro debe ser mayor a cero")
  ), 
  "gamm" = list(
    list(parametro = "alpha", rango = c(0.001, NA), 
         mensaje = "El valor del parámetro debe ser mayor a cero"),
    list(parametro = "betav", rango = c(0.001, NA), 
         mensaje = "El valor del parámetro debe ser mayor a cero")
  ), 
  "beta" = list(
    list(parametro = "forma1", rango = c(0.001, NA), 
         mensaje = "El valor del parámetro debe ser mayor a cero"),
    list(parametro = "forma2", rango = c(0.001, NA), 
         mensaje = "El valor del parámetro debe ser mayor a cero")
  ), 
  "chi" = list(
    list(parametro = "gdl_chi", rango = c(2.999, NA), 
         mensaje = "Los grados de libertad deben ser un número natural mayor a cero")
  ), 
  "ts" = list(
    list(parametro = "gdl_t", rango = c(2.999, NA), 
         mensaje = "Los grados de libertad deben ser un número natural mayor a cero")
  ), 
  "cau" = list(
    list(parametro = "iota", rango = c(NA, NA), 
         mensaje = "Introduce un valor válido"),
    list(parametro = "tau", rango = c(0.001, NA), 
         mensaje = "El valor del parámetro debe ser mayor a cero")
  ), 
  "ber" = list(
    list(parametro = "theta", rango = c(0.001, 0.999), 
         mensaje = "El valor del parámetro debe ser menor al máximo del intervalo")
  ))
# Link github
link_github <- tags$a(shiny::icon("github"), "GitHub", href = "https://github.com/Vzzzzzz/IIMAS_PiMC", target = "_blank")
###############################################################################
#### O P C I O N E S.
###############################################################################
# Opciones generales.
opciones_generales <- tagList(
  # Selección tamaño de muestra.
  numericInput(inputId = "muestra", 
               label = "Tamaño de muestra", 
               value = 10000, 
               min = 10, 
               max = 100000000,
               step = 100) |>
    tooltip("Selecciona la cantidad de simulaciones a realizar",
            placement = "right"),
  # Selección semilla.
  numericInput(inputId = "semilla",
               label = "Semilla",
               value = NULL,
               min = 1,
               step = 1) |>
    tooltip("Introduce una semilla inicial de simulación",
            placement = "right"),
  # Inicio de simulación.
  actionButton(inputId = "inicio", 
               label = "Iniciar simulación", 
               class = 'btn btn-primary btn-lg')
  )

# Selección de número de variables de entrada.
opciones_mv1 <- tagList(
  selectizeInput(inputId = "nVariables",
                 label = "Número de variables de entrada X",
                 choices = seq(from = 1, to = 5, by = 1),
                 options = list(search = F)) |>
    tooltip("Selecciona la cantidad de variables de entrada X",
            placement = "right")
  )

# Transformación para aplicar X_i.
opciones_mv2 <- tagList(
  textAreaInput(inputId = "transformacion_mv",
                label = "Transformación",
                value = "x_1^2 + 10 * sin(x_1)",
                resize = "both") |>
    tooltip("Escribe la transformación para aplicar a las variable de entrada X_i
            como expresión de r", 
            placement = "right")
  )

# Selección transformación X.
opciones_e <- tagList(
  selectizeInput(inputId = "transformacion_e", 
                 label = "Transformación",
                 choices = NULL,
                 selected = NULL,
                 options = list(search = F)) |>
    tooltip("Selecciona la transformación a aplicar a la variable de entrada X", 
            placement = "right"),
  # Parámetros función lineal ax + b.
  conditionalPanel(condition = "input.transformacion_e == 'lin'",
                   numericInput(inputId = "pendiente",
                                label = "Pendiente: a",
                                value = 1),
                   numericInput(inputId = "ordenada",
                                label = "Ordenada al origen: b",
                                value = 0)
  ),
  # Parámetros función potencia x^n.
  conditionalPanel(condition = "input.transformacion_e == 'pot'",
                   numericInput(inputId = "elevar",
                                label = "Potencia: n",
                                value = 2,
                                min = 1,
                                step = 1)
  ),
  # Parámetros función raíz x^(1/n).
  conditionalPanel(condition = "input.transformacion_e == 'raiz'",
                   numericInput(inputId = "bajar",
                                label = "Raíz: n",
                                value = 2,
                                min = 1,
                                step = 1)
  )
  )

# Resultados de correlación.
card_correlaciones <- tagList(
  layout_columns(
    col_widths = c(6,6),
    card(
      fill = FALSE,
      card_header(class = "bg-dark", "Correlación Pearson"),
      full_screen = TRUE,
      height = "600px",
      card_body(
        plotOutput("heatmap_pearson"),
        tableOutput("correlacion_pearson")
      )
    ),
    card(
      fill = FALSE,
      card_header(class = "bg-dark", "Correlación Spearman"),
      full_screen = TRUE,
      height = "600px",
      card_body(
        plotOutput("heatmap_spearman"),
        tableOutput("correlacion_spearman")
      )
    )
  )
)

###############################################################################
#### F U N C I O N E S.
###############################################################################
# Función de opciones para selección de distribución y parámetros.
funcion_distribucion_X <- function(i, input) {
  tagList(
    if (input$navegacion == 'PiMC-e') { "Distribución" },
    if (input$navegacion == 'PiMC-mv') { paste0("Variable X_", i) },
    selectizeInput(inputId = sprintf("distribucionX%d", i), 
                   label = NULL, 
                   choices = lista_distribuciones,
                   options = list(search = F)) |>
      tooltip(paste0("Selecciona la distribución para la variable de entrada X_", i), 
              placement = "right"),
    # Parametros distribución Normal.
    conditionalPanel(
      condition = sprintf("input.distribucionX%d == 'norm'", i),
      numericInput(inputId = paste0("mu", i),
                   label = "Media",
                   value = 0,
                   step = 0.1),
      numericInput(inputId = paste0("sigma", i),
                   label = "Desviación estandar",
                   value = 1,
                   min = 0.001,
                   step = 0.1)
    ),
    # Parametros distribución Uniforme (a,b).
    conditionalPanel(
      condition = sprintf("input.distribucionX%d == 'unif1'", i),
      numericInput(inputId = paste0("min", i),
                   label = "Mínimo",
                   value = 0,
                   step = 1),
      numericInput(inputId = paste0("max", i),
                   label = "Máximo",
                   value = 1,
                   step = 1)
    ),
    # Parametros distribución Uniforme (media, desv. estándar).
    conditionalPanel(
      condition = sprintf("input.distribucionX%d == 'unif2'", i),
      numericInput(inputId = paste0("mu_u", i),
                   label = "Media",
                   value = 0,
                   step = 1),
      numericInput(inputId = paste0("sigma_u", i),
                   label = "desv. estándar",
                   value = 1,
                   min = 0.01,
                   step = 1)
    ),
    # Parametros distribución Triangular (mínimo, máximo, moda).
    conditionalPanel(
      condition = sprintf("input.distribucionX%d == 'tri1'", i),
      numericInput(inputId = paste0("min_ta", i),
                   label = "Mínimo",
                   value = 0,
                   step = 1),
      numericInput(inputId = paste0("max_ta", i),
                   label = "Máximo",
                   value = 1,
                   step = 1),
      numericInput(inputId = paste0("moda_ta", i),
                   label = "Moda",
                   value = 0.5,
                   step = 0.1) 
    ),
    # Parametros distribución Triangular (media, desv. estándar).
    conditionalPanel(
      condition = sprintf("input.distribucionX%d == 'tri2'", i),
      numericInput(inputId = paste0("mu_t", i),
                   label = "Media",
                   value = 0,
                   step = 0.1),
      numericInput(inputId = paste0("sigma_t", i),
                   label = "Desviación estándar",
                   value = 1,
                   min = 0.001,
                   step = 1) 
    ),
    # Parametros distribución Exponencial (tasa).
    conditionalPanel(
      condition = sprintf("input.distribucionX%d == 'exp'", i),
      numericInput(inputId = paste0("lambda", i),
                   label = "Tasa",
                   value = 1,
                   min = 0.001,
                   step = 0.1)
    ),
    # Parametros distribución Gamma (forma, escala).
    conditionalPanel(
      condition = sprintf("input.distribucionX%d == 'gamm'", i),
      numericInput(inputId = paste0("alpha", i),
                   label = "Forma",
                   value = 1,
                   min = 0.001,
                   step = 0.1),
      numericInput(inputId = paste0("betav", i),
                   label = "Escala",
                   value = 1,
                   min = 0.001,
                   step = 0.1)
    ),
    # Parametros distribución Beta (forma 1, forma 2).
    conditionalPanel(
      condition = sprintf("input.distribucionX%d == 'beta'", i),
      numericInput(inputId = paste0("forma1", i),
                   label = "Forma 1",
                   value = 1,
                   min = 0.001,
                   step = 0.1),
      numericInput(inputId = paste0("forma2", i),
                   label = "Forma 2",
                   value = 1,
                   min = 0.001,
                   step = 0.1)
    ),
    # Parametros distribución Chi cuadrada.
    conditionalPanel(
      condition = sprintf("input.distribucionX%d == 'chi'", i),
      numericInput(inputId = paste0("gdl_chi", i),
                   label = "Grados de libertad",
                   value = 3,
                   min = 3,
                   step = 1)
    ),
    # Parametros distribución T student.
    conditionalPanel(
      condition = sprintf("input.distribucionX%d == 'ts'", i),
      numericInput(inputId = paste0("gdl_t", i),
                   label = "Grados de libertad",
                   value = 3,
                   min = 0.001,
                   step = 1)
    ),
    # Parametros distribución Cauchy (locación, escala).
    conditionalPanel(
      condition = sprintf("input.distribucionX%d == 'cau'", i),
      numericInput(inputId = paste0("iota", i),
                   label = "Locación",
                   value = 0,
                   step = 0.1),
      numericInput(inputId = paste0("tau", i),
                   label = "Escala",
                   value = 1,
                   min = 0.001,
                   step = 0.1)
    ),
    # Parametros distribución Bernoulli (probabilidad éxito).
    conditionalPanel(
      condition = sprintf("input.distribucionX%d == 'ber'", i),
      numericInput(inputId = paste0("theta", i),
                   label = "Probabilidad",
                   value = 0.5,
                   min = 0.001,
                   max = 0.999,
                   step = 0.1) 
    ),
    tags$hr()
  )
}
# Función de simulación distribución de entrada.
funcion_simulacion_X <- function(i, input) {
  X_i <- input[[paste0("distribucionX", i)]]
  switch(X_i, 
         "norm" = rnorm(n = input$muestra,
                        mean = input[[paste0("mu", i)]],
                        sd = input[[paste0("sigma", i)]]
         ),
         "unif1" = runif(n = input$muestra,
                         min = input[[paste0("min", i)]],
                         max = input[[paste0("max", i)]]
         ),
         "unif2" = runif(n = input$muestra,
                         min = input[[paste0("mu_u", i)]] - 
                           (sqrt(12) * input[[paste0("sigma_u", i)]])/2,
                         max = input[[paste0("mu_u", i)]] + 
                           (sqrt(12) * input[[paste0("sigma_u", i)]])/2
         ),
         "tri1" = rtri(n = input$muestra, 
                       min = input[[paste0("min_ta", i)]], 
                       max = input[[paste0("max_ta", i)]], 
                       mode = input[[paste0("moda_ta", i)]]
         ),
         "tri2" = rtri(n = input$muestra, 
                       min = input[[paste0("mu_t", i)]] - 
                         (sqrt(24) * input[[paste0("sigma_t", i)]])/2 , 
                       max = input[[paste0("mu_t", i)]] + 
                         (sqrt(24) * input[[paste0("sigma_t", i)]])/2,
                       mode = input[[paste0("mu_t", i)]]
         ),
         "exp" = rexp(n = input$muestra,
                      rate = input[[paste0("lambda", i)]]
         ),
         "gamm" = rgamma(n = input$muestra,
                         shape = input[[paste0("alpha", i)]],
                         scale = input[[paste0("betav", i)]]
         ),
         "beta" = rbeta(n = input$muestra,
                        shape1 = input[[paste0("forma1", i)]],
                        shape2 = input[[paste0("forma2", i)]]
         ),
         "chi" = rchisq(n = input$muestra,
                        df = input[[paste0("gdl_chi", i)]]
         ),
         "ts" = rt(n = input$muestra,
                   df = input[[paste0("gdl_t", i)]] 
         ),
         "cau" = rcauchy(n = input$muestra,
                         location = input[[paste0("iota", i)]],
                         scale = input[[paste0("tau", i)]] 
         ),
         "ber" = rbinom(n =  input$muestra,
                        size = 1,
                        prob = input[[paste0("theta", i)]]
         )
  )
}
# Función obtención de intervlaos de cobertura de simulaciones.
tabla_cobertura_general <- function(df) {
  proba_min <- (1 - c(0.99, 0.95, 0.90, 0.68)) / 2
  proba_max <- 1 - proba_min
  df_cobertura <- data.frame(
    "Cobertura" = paste0(c(0.99, 0.95, 0.90, 0.68) * 100, "%")
  )
  for (Xi in names(df)) {
    int_min <- round(quantile(df[[Xi]], probs = proba_min), 2)
    int_max <- round(quantile(df[[Xi]], probs = proba_max), 2)
    df_cobertura[[paste0("Intervalo_", Xi)]] <- 
      paste0("( ", int_min, " , ", int_max, " )")
  }
  return(df_cobertura)
}
# Función obtención de estadísticas de simulaciones.
estadisticas_general <- function(df) {
  df_sumario <- data.frame(
    "Estadística" = c("Media", "Desviación estándar", "Mediana", "MAD")
  )
  for (Xi in names(df)) {
    df_sumario[[Xi]] <- c(mean(df[[Xi]]),
                          sd(df[[Xi]]),
                          median(df[[Xi]]),
                          mad(df[[Xi]]))
  }
  return(df_sumario)
}
# Función resultado de prueba bondad de ajuste.
resultado_pvalue <- function(p_value) {
  # Mensaje de resultado negativo.
  if (p_value < 0.05) {
    resultado <- tagList(
      value_box(title = "¿Se distribuye normal?",
                value = "No",
                max_height = "115px",
                min_height = "90px",
                theme = "danger") |>
        tooltip("Conclusión obtenida a partir de la aplicación de prueba de 
                bondad de ajuste Kolmogorov-Smirnov para normalidad con p-value 
                de 0.05.",
                placement = "bottom")
    )
  }
  # Mensaje de resultado positivo.
  else {
    resultado <- tagList(
      value_box(title = "¿Se distribuye normal?",
                value = "Sí",
                max_height = "115px",
                min_height = "90px",
                theme = "success") |>
        tooltip("Conclusión obtenida a partir de la aplicación de prueba de 
                bondad de ajuste Kolmogorov-Smirnov para normalidad con p-value 
                de 0.05.",
                placement = "bottom")
    )
  }
  return(resultado)
}

###############################################################################
####  P A N E L E S   D E   R E S U L T A D O S. 
###############################################################################
# Panel de resultados X.
panel_pagina_1 <- tagList(
  layout_columns(
    col_widths = c(7,5),
    card(fill = TRUE,
         class = "border-primary",
         card_body(
           #### G R Á F I C O   C O M B I N A D O.  ##########################
           card(full_screen = TRUE,
                fill = FALSE,
                max_height = "950px",
                card_header(class = "bg-dark", "Histogramas y función de medida"),
                card_body(plotOutput("grafica_combinada_e"))),
           
           #### I N T E R V A L O   D E   C O N F I A N Z A. #################
           card(fill = FALSE,
                height = "150px",
                class = "border-light",
                max_height = "200px",
                card_header(class = "bg-dark", "Intervalo de cobertura interactivo"),
                card_body(
                  layout_columns(
                    col_widths = c(6, 6),
                    slider_confianza,
                    tableOutput("tabla_intervalos")
                  )
                ))
         )
    ),# Fin de la card izquierda.
    card(fill = TRUE,
         class = "border-primary",
         card_body(
           layout_columns(
             col_widths = c(6,6),
             gap = "8px",
             ####  V A L U E   B O X   M E D I A.  ##########################
             value_box(title = "Media de Y",
                       value = textOutput("mediaY_e"),
                       max_height = "80px",
                       min_height = "50px",
                       theme = "primary" 
             ),
             value_box(title = "Media de X",
                       value = textOutput("mediaX"),
                       max_height = "80px",
                       min_height = "50px",
                       theme = "secondary" 
             ),
             value_box(title = "Desviación estandar de Y",
                       value = textOutput("desv_estY_e"),
                       max_height = "100px",
                       min_height = "50px",
                       theme = "primary"
             ),
             value_box(title = "Desviación estandar de X",
                       value = textOutput("desv_estX"),
                       max_height = "100px",
                       min_height = "50px",
                       theme = "secondary"
             ),
             uiOutput("bondad_ajuste_e"),
             uiOutput("normalX")
           ),
           #### T A B L A   S U M A R I O.   #################################
           tableOutput("tabla_sumario_e"),
           #### T A B L A   C O B E R T U R A.   #############################
           tableOutput("tabla_cobertura_e")
         )
    )
  )
)
# Panel de resultados X_i.
panel_pagina_2 <- tagList(
  card(fill = FALSE,
       class = "border-primary",
       card_body(
         layout_columns(
           col_widths = c(9,3),
           card(fill = FALSE,
                full_screen = TRUE,
                height = "400px",
                max_height = "950px",
                class = "border-secondary",
                card_header(class = "bg-dark", "Histogramas"),
                card_body(
                  #### G R Á F I C O   C O M B I N A D O.  ##########################
                  plotOutput("grafica_combinada_mv")
                )),
           card(fill = FALSE,
                class = "border-secondary",
                height = "400px",
                card_body(
                  #### C A R D S. ####
                  value_box(title = "Media de Y",
                            value = textOutput("mediaY_mv"),
                            max_height = "100px",
                            min_height = "80px",
                            theme = "primary" 
                  ),
                  value_box(title = "Desviación estandar de Y",
                            value = textOutput("desv_estY_mv"),
                            max_height = "115px",
                            min_height = "95px",
                            theme = "primary"
                  ),
                  uiOutput("bondad_ajuste_mv")
                ))
         ),
         #### T A B L A   S U M A R I O.   #################################
         tableOutput("tabla_sumario_mv"),
         #### T A B L A   C O B E R T U R A.   #############################
         tableOutput("tabla_cobertura_mv"),
         card_correlaciones
       ))
)













