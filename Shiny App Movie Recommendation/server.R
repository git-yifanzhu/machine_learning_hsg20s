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
  
  #logistic regression rec
  logistic_react <- reactive(get_recommend_logistic(input$numInput, 10)%>% select('title'))
  output$logOutput <-renderTable(logistic_react(), colnames = FALSE)
  
  #Collaborative Filtering rec
  cola_filt<- reactive(get_recommend_user(input$numInput)%>% select('title'))
  output$Colaborative_filt <- renderTable(cola_filt(), colnames=FALSE)

  
}