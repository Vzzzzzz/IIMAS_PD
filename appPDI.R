library(shiny)
library(bslib)
library(ggplot2)

### Instalador de paqueterías utilizadas. (Quitar comentarios para poder instalar)


# librerias_usadas <- c("shiny", "bslib", "patchwork", "EnvStats", "magrittr")
# 
# instalar <- librerias_usadas[!(librerias_usadas %in% installed.packages()[, "Package"])]
# if (length(instalar) > 0) {
#   print(paste0("Instalando paquetería:", instalar))
#   install.packages(instalar, repos = "http://lib.stat.cmu.edu/R/CRAN")
# }


###

source("uiPDIv_102.R")
source("serverPDIv_102.R")

shinyApp(ui = ui, server = server)