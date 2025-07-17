# Import packages --------------------------------------------------------------
library(shiny)
library(shinydashboard)

# Import UI and server ---------------------------------------------------------
source('OICD_UI.R')
source('OICD_server.R')

# Run application --------------------------------------------------------------
shinyApp(ui = OICD_UI, server = OICD_server)