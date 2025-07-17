# Import packages --------------------------------------------------------------
library(shiny)
library(shinydashboard)

# Import UI and server ---------------------------------------------------------
source('user_interface.R')
source('server.R')

# Run application --------------------------------------------------------------
shinyApp(ui = OICD_UI, server = OICD_server)
