# Define UI for application that draws a histogram
library(shinyWidgets)
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(dplyr)


ui <- shinyUI(
  fluidPage(
    theme = shinytheme("cyborg"),
    # tags$header(tags$div(HTML(paste0('<center>',tags$img(src = "https://i.ibb.co/r5krrdz/logo.png"),'</center>')))),
    # setBackgroundImage(src = 'https://i.ibb.co/vXqDmnh/background.jpg'),
    tags$head(tags$div(class = "txt",
             tags$link(rel = "stylesheet", type = "text/css", href = "netflix_style.css", 'Movie Recommender Engine'))),
    tags$style(HTML(".tabs-above > .nav > li[class=active] > a {background-color: #FF0000; color: #FFF;}")),
    

tabsetPanel(

  
    tabPanel('Movie Recommendations KNN vs. Plot-based Algorithms',
    wellPanel(
      fluidRow(
        column(6,
               uiOutput('Input_Movie'),
               sliderInput("Recslider", label = 'Number of Recommendations', min = 0, max = 20, value = 10),),
        tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: red}")),
        column(6,tags$h6('Plot of selected Movie'),tableOutput('movie_description'))),
      
      fluidRow(column(6,
                      tags$h4('KNN Movie Recommendations'),
                      tableOutput('KNN_table') %>% withSpinner(color = getOption("spinner.color", default = "#FF0000"))),
               column(6,
                      tags$h4('Plot based Movie Recommendations'),
                      tableOutput('reco_plot') %>% withSpinner(color = getOption("spinner.color", default = "#FF0000")))))),
    
    tabPanel('Testing Collaborative Filtering vs. Logistic Regression',
           wellPanel(
             fluidRow(
               column(6,
                      numericInput("numInput", label = 'Select a User Profile (number between 1 - 564)', min = 1, max = 564, value = 564))),
             fluidRow(column(6,
                             tags$h4('Collaborative Filtering'),
                             tableOutput('Colaborative_filt') %>% withSpinner(color = getOption("spinner.color", default = "#FF0000"))),
                      column(6,
                             tags$h4('Logistic Regression Recommendations'),
                             tableOutput('logOutput') %>% withSpinner(color = getOption("spinner.color", default = "#FF0000"))))))
    )
)
)
    

