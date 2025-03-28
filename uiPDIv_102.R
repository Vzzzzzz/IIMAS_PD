
# Script: ui utilizado en el program de distribución de incertidumbre
#  PDI Versión 1.0.0. Jueves 25 de febrero de 2025.
# Versión 1.0.2 Jueves 20 de marzo de 2025.

# install.packages("bsicons")

##############################################################################
## P A Q U E T E R Í A S   Y   O T R O S.
##############################################################################

library(shiny)
library(bslib)

# Vector con nombres de distribuciones.
lista_distribuciones <- c("Normal (media, desv. estándar)","Uniforme (mínimo, máximo)",
                          "Uniforme (media, desv. estándar)", "Triangular (mínimo, máximo, moda)",
                          "Triangular (media, desv. estándar)", "Exponencial (tasa)", 
                          "Gamma (forma, escala)", "Beta (forma 1, forma 2)", 
                          "Chi-cuadrada (grados libertad)", "T-Student (grados libertad)", 
                          "Cauchy (locación, escala)", "Bernoulli (probabilidad éxito)" 
                          )

# Slider para determinar procentaje del intervalo de cobertura.
slider_confianza <- sliderInput(inputId = "coberturaS",
                                label = NULL,
                                min = 0.66,
                                max = 0.99,
                                value = 0.95,
                                step = 0.01,
                                ticks = FALSE)

##############################################################################
## U I   D E   A P P.
##############################################################################

ui <- page_fillable(
  title = "IIMAS. App de propagación de distribución. Metrología",
  theme = bs_theme(bootswatch = "zephyr"),
  fillable_mobile = TRUE,
  withMathJax(),
  layout_sidebar(sidebar = sidebar(
    
    #### S E L E C C I O N   D I S T R I B U C I O N   X. #####################
    selectizeInput(inputId = "distribucionX", 
                   label = "Distribución", 
                   choices = lista_distribuciones,
                   options = list(search = F)) |>
      tooltip("Selecciona la distribución para la variable de entrada X", 
              placement = "right"),
    # Parametros distribución Uniforme (a,b).
    conditionalPanel(condition = "input.distribucionX == 'Uniforme (mínimo, máximo)'",
                     numericInput(inputId = "min",
                                  label = "Mínimo",
                                  value = 0,
                                  step = 1),
                     numericInput(inputId = "max",
                                  label = "Máximo",
                                  value = 1,
                                  step = 1)
    ),
    # Parametros distribución Uniforme (media, desv. estándar).
    conditionalPanel(condition = "input.distribucionX == 'Uniforme (media, desv. estándar)'",
                     numericInput(inputId = "mu_u",
                                  label = "Media",
                                  value = 0,
                                  step = 1),
                     numericInput(inputId = "sigma_u",
                                  label = "desv. estándar",
                                  value = 1,
                                  min = 0.01,
                                  step = 1)
    ),
    # Parametros distribución Normal.
    conditionalPanel(condition = "input.distribucionX == 'Normal (media, desv. estándar)'",
                     numericInput(inputId = "mu",
                                  label = "Media",
                                  value = 0,
                                  step = 0.1),
                     numericInput(inputId = "sigma",
                                  label = "Desviación estandar",
                                  value = 1,
                                  min = 0.001,
                                  step = 0.1)
    ),
    # Parametros distribución Exponencial (tasa).
    conditionalPanel(condition = "input.distribucionX == 'Exponencial (tasa)'",
                     numericInput(inputId = "lambda",
                                  label = "Tasa",
                                  value = 1,
                                  min = 0.001,
                                  step = 0.1)
    ),
    # Parametros distribución Gamma (forma, escala).
    conditionalPanel(condition = "input.distribucionX == 'Gamma (forma, escala)'",
                     numericInput(inputId = "alpha",
                                  label = "Forma",
                                  value = 1,
                                  min = 0.001,
                                  step = 0.1),
                     numericInput(inputId = "betav",
                                  label = "Escala",
                                  value = 1,
                                  min = 0.001,
                                  step = 0.1)
    ),
    # Parametros distribución Beta (forma 1, forma 2).
    conditionalPanel(condition = "input.distribucionX == 'Beta (forma 1, forma 2)'",
                     numericInput(inputId = "forma1",
                                  label = "Forma 1",
                                  value = 1,
                                  min = 0.001,
                                  step = 0.1),
                     numericInput(inputId = "forma2",
                                  label = "Forma 2",
                                  value = 1,
                                  min = 0.001,
                                  step = 0.1)
    ),
    # Parametros distribución Chi cuadrada.
    conditionalPanel(condition = "input.distribucionX == 'Chi-cuadrada (grados libertad)'",
                     numericInput(inputId = "gdl_chi",
                                  label = "Grados de libertad",
                                  value = 3,
                                  min = 3,
                                  step = 1)
    ),
    # Parametros distribución T student.
    conditionalPanel(condition = "input.distribucionX == 'T-Student (grados libertad)'",
                     numericInput(inputId = "gdl_t",
                                  label = "Grados de libertad",
                                  value = 3,
                                  min = 3,
                                  step = 1)
    ),
    # Parametros distribución Cauchy (locación, escala).
    conditionalPanel(condition = "input.distribucionX == 'Cauchy (locación, escala)'",
                     numericInput(inputId = "iota",
                                  label = "Locación",
                                  value = 0,
                                  step = 0.1),
                     numericInput(inputId = "tau",
                                  label = "Escala",
                                  value = 1,
                                  min = 0.001,
                                  step = 0.1)
    ),
    # Parametros distribución Bernoulli (probabilidad éxito).
    conditionalPanel(condition = "input.distribucionX == 'Bernoulli (probabilidad éxito)'",
                     numericInput(inputId = "theta",
                                  label = "Probabilidad",
                                  value = 0.5,
                                  min = 0.001,
                                  max = 0.999,
                                  step = 0.1) 
    ),
    # Parametros distribución Triangular (media, desv. estándar).
    conditionalPanel(condition = "input.distribucionX == 'Triangular (media, desv. estándar)'",
                     numericInput(inputId = "mu_t",
                                  label = "Media",
                                  value = 0,
                                  step = 0.1),
                     numericInput(inputId = "sigma_t",
                                  label = "Desviación estándar",
                                  value = 1,
                                  min = 0.001,
                                  step = 1) 
    ),
    # Parametros distribución Triangular (mínimo, máximo, moda).
    conditionalPanel(condition = "input.distribucionX == 'Triangular (mínimo, máximo, moda)'",
                     numericInput(inputId = "min_ta",
                                  label = "Mínimo",
                                  value = 0,
                                  step = 1),
                     numericInput(inputId = "max_ta",
                                  label = "Máximo",
                                  value = 1,
                                  step = 1),
                     numericInput(inputId = "moda_ta",
                                  label = "Moda",
                                  value = 0.5,
                                  step = 0.1) 
    ),
    
    
    #### S E L E C C I O N   F U N C I Ó N   M E D I C I Ó N. #################
    selectizeInput(inputId = "transformacion", 
                   label = "Transformación",
                   choices = NULL,
                   selected = NULL,
                   options = list(search = F)) |>
      tooltip("Selecciona la transformación a aplicar a la variable de entrada X", 
              placement = "right"),
    # Parámetros función lineal ax + b.
    conditionalPanel(condition = "input.transformacion == 'Lineal a*x + b'",
                     numericInput(inputId = "pendiente",
                                  label = "Pendiente: \\(a\\)",
                                  value = 1),
                     numericInput(inputId = "ordenada",
                                  label = "Ordenada al origen: \\(b\\)",
                                  value = 0)
    ),
    # Parámetros función potencia x^n.
    conditionalPanel(condition = "input.transformacion == 'Potencia x^n'",
                     numericInput(inputId = "elevar",
                                  label = "Potencia: \\(n\\)",
                                  value = 2,
                                  min = 1,
                                  step = 1)
    ),
    # Parámetros función raíz x^(1/n).
    conditionalPanel(condition = "input.transformacion == 'Raíz x^(1/n)'",
                     numericInput(inputId = "bajar",
                                  label = "Raíz: \\(n\\)",
                                  value = 2,
                                  min = 1,
                                  step = 1)
    ),
    
    #### S E L E C C I O N   T A M A Ñ O   M U E S T R A. #####################
    numericInput(inputId = "muestra", 
                 label = "Tamaño de muestra", 
                 value = 10000, 
                 min = 10, 
                 max = 100000000,
                 step = 100) |>
      tooltip("Selecciona la cantidad de simulaciones a realizar",
              placement = "right"),
    
    #### S E L E C C I O N   S E M I L L A. ###################################
    numericInput(inputId = "semilla",
                 label = "Semilla",
                 value = NULL,
                 min = 1,
                 step = 1) |>
      tooltip("Introduce una semilla inicial de simulación",
              placement = "right"),
    
    #### I N I C I O   D E   S I M U L A C I Ó N.  ############################
    actionButton(inputId = "inicio", 
                 label = "Iniciar simulación", 
                 class = 'btn btn-primary btn-lg'),
    "IIMAS. App de propagación de distribución. Metrología v1.0.2"
  ),#termina sidebar.
  layout_columns(
    col_widths = c(7,5),
    card(fill = TRUE,
         class = "border-primary",
         card_body(
           #### G R Á F I C O   C O M B I N A D O.  ###########################
           card(full_screen = TRUE,
                fill = FALSE,
                max_height = "950px",
                card_header(class = "bg-dark", "Histogramas y función de medida"),
                card_body(plotOutput("grafica_combinada"))),
           
           #### I N T E R V A L O   D E   C O N F I A N Z A. ##################
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
             ####  V A L U E   B O X   M E D I A.  ########################
             value_box(title = "Media de Y",
                       value = textOutput("mediaY"),
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
             #### V A L U E   B O X   D E S V. E S T A N D A R.  ################
             value_box(title = "Desviación estandar de Y",
                       value = textOutput("desv_estY"),
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
             #### B O N D A D   A J U S T E.   ##################################
             uiOutput("bondad_ajuste"),
             uiOutput("normalX")
             ),
           #### T A B L A   S U M A R I O.   ##################################
           tableOutput("tabla_sumario"),
           #### T A B L A   C O B E R T U R A.   ##############################
           tableOutput("tabla_cobertura")
         )
    )
  )
  )
)




