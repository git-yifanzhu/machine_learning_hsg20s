library(shinyWidgets)
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(data.table)
library(dplyr)
library(tidytext)
library(formattable)

source('global.R')

# Run the application 
shinyApp(ui = ui, server = server)