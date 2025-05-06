# Script: ui utilizado en el program de distribución de incertidumbre
#  PDI Versión 2.0.0. Jueves 3 de abril de 2025.
#  PiMC Versión 2.0.1 Miércoles 23 de abril de 2025.
library(shiny)
library(bslib)
source("contenidoPiMC_v201.R")

ui <- page_navbar(
  title = "PiMC v2",
  id = "navegacion",
  theme = bs_theme(bootswatch = "zephyr"),
  navbar_options = navbar_options(theme = "dark", collapsible = TRUE),
  sidebar = sidebar(
    conditionalPanel(condition = "input.navegacion == 'PiMC-mv'", opciones_mv1),
    uiOutput("distribucionVariables"),
    conditionalPanel(condition = "input.navegacion == 'PiMC-e'", opciones_e),
    conditionalPanel(condition = "input.navegacion == 'PiMC-mv'", opciones_mv2),
    opciones_generales
  ),
  nav_panel(title = "PiMC-e", panel_pagina_1),
  nav_panel(title = "PiMC-mv", panel_pagina_2),
  nav_spacer(),
  nav_menu(title = "Script", nav_item(link_github))
)