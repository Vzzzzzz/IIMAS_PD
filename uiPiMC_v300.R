library(shiny)
library(bslib)
library(ggplot2)
source("contenidoPiMC_v300.R")
source("serverPiMC_v300.R")

### 


ui <- page_navbar(
  title = "PiMC v3.0.1",
  id = "navegacion",
  theme = bs_theme(bootswatch = "zephyr"),
  navbar_options = navbar_options(theme = "dark", collapsible = TRUE),
  sidebar = sidebar(
    conditionalPanel(condition = "input.navegacion == 'PiMC-mv'", opciones_mv_1),
    conditionalPanel(condition = "input.navegacion == 'PiMC-mv2'", opciones_mv2_1),
    uiOutput("distribucionVariables"),
    conditionalPanel(condition = "input.navegacion == 'PiMC-e'", opciones_e),
    conditionalPanel(condition = "input.navegacion == 'PiMC-mv'", opciones_mv_2),
    conditionalPanel(condition = "input.navegacion == 'PiMC-mv2'", opciones_mv2_2),
    opciones_generales
  ),
  nav_panel(title = "PiMC-e", panel_pagina_1),
  nav_panel(title = "PiMC-mv", panel_pagina_2),
  nav_panel(title = "PiMC-mv2", panel_pagina_3),
  nav_spacer(),
  nav_menu(title = "Script", nav_item(link_github))
)

