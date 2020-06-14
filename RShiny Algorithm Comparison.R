
#laod packages
library(shinyWidgets)
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(data.table)
library(dplyr)
library(tidytext)
library(formattable)


# we can not source the whole main.R (takes to long)
# source("main.R")

source2 <- function(file, start, end, ...) {
    file.lines <- scan(file, what=character(), skip=start-1, nlines=end-start+1, sep='\n')
    file.lines.collapsed <- paste(file.lines, collapse='\n')
    source(textConnection(file.lines.collapsed), ...)
}
#only source the functions in the beginning and not the hole file
source2('main.R',1,180)


# Define server logic 
server <- function(input, output, session) {
    
    #input for movie selection
    movies <- unique(movies_data$title)
    numb_rec <- reactive(input$Recslider)
    output$Input_Movie <-  renderUI(fluidRow(column(12,selectInput("Selected_Movie",label = 'Select a Movie for Recommendations', choices = movies))))
    Movdescription <- reactive(get_movie_data(input$Selected_Movie))
    output$movie_description <- renderTable(Movdescription()$overview, colnames = FALSE)
    
    #output of knn movie rec
    knn_react <- reactive(get_recommend_kNN(input$Selected_Movie, numb_rec(), c(1, 1, 1, 1))$title)
    output$KNN_table <- renderTable(knn_react(), colnames = FALSE)
    
    #output of plot based movie rec
    reco_plot_react <- reactive(get_recommend_plot(input$Selected_Movie, numb_rec())$title)
    output$reco_plot <-renderTable(reco_plot_react(), colnames = FALSE)
    
    # #logistic regression rec
    # logistic_react <- reactive(get_recommend_logistic(input$numInput, 10)%>% select('title'))
    # output$logOutput <-renderTable(logistic_react(), colnames = FALSE)
    # 
    # #Collaborative Filtering rec
    # cola_filt<- reactive(get_recommend_user(input$numInput)%>% select('title'))
    # output$Colaborative_filt <- renderTable(cola_filt(), colnames=FALSE)
}


# Define User Interface
ui <- shinyUI(
    
    fluidPage(
        setBackgroundImage(src = 'https://i.ibb.co/vXqDmnh/background.jpg'),
        
        theme = shinytheme("cyborg"),
        # tags$header(tags$div(HTML(paste0('<center>',tags$img(src = "https://i.ibb.co/r5krrdz/logo.png"),'</center>')))),
        # tags$head(tags$div(class = "txt",
        #                    tags$link(rel = "stylesheet", type = "text/css", href = "netflix_style.css", 'Movie Recommender Engine'))),
        # tags$style(HTML(".tabs-above > .nav > li[class=active] > a {background-color: #FF0000; color: #FFF;}")),
        tags$h1('MOVIE RECOMMENDER'),
        tags$head(tags$style(HTML("@import url('https://fonts.googleapis.com/css?family=Roboto:700,900');
                                      h1 {
                                        font-weight: 5000;
                                        line-height: 5;
                                        font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
                                        line-height: 1;
                                        margin-left: auto;
                                        margin-right: auto;
                                        padding-bottom: 20px;
                                        padding-top: 20px;
                                        text-align: center;
                                        width: 80%;
                                        font-size: 5rem;
                                        color: red;}"))
        ),
        
        
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
    
    
# Run the application 
shinyApp(ui = ui, server = server)
