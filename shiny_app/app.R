# app.R
suppressPackageStartupMessages({ library(shiny); library(bslib) })
source("R/globals.R", local = TRUE)
source("R/helpers_econ.R", local = TRUE)
source("R/ui_main.R", local = TRUE)
source("R/server_main.R", local = TRUE)
shinyApp(ui = ui, server = server)

