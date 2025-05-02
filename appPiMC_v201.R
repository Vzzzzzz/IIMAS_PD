library(shiny)
library(bslib)
library(ggplot2)

###

source("serverPiMC_v201.R")
source("uiPiMC_v201.R")

shinyApp(ui = ui, server = server)