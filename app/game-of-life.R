library(shiny)
library(d3heatmap)
library(Matrix)
source('constr.R')

# Define UI for application
ui <- shinyUI(fluidPage(
  fluidRow(
    column(width = 12, class = "well",
      selectInput("objectSelect", "Select Object",
                  choices = list("Glider" = 1, 
                                 "Light Weight Space Ship" = 2,
                                 "Pulsar" = 3, 
                                 "R-pentomino" = 4)),
      actionButton("startButton", "Start"),
      actionButton("stopButton", "Stop"),
      actionButton("stepButton", "One Step"),
      br(), br(),
      d3heatmapOutput("plot")
    )
  )
))

# Define server logic
server <- shinyServer(function(input, output) {

  ############## Core Logic ##########################
  # Constructing the transformation matrix
  constructTransformation <- function(width) {
    tt <<- Diagonal(width)
    # constructing the minor diagonals
    # TODO: Is there a more efficient way?
    md <- Diagonal(width-1)
    md <- rbind(0, cbind(md, 0))
    tt <<- tt + md + t(md)
    # make it torrodial 
    tt[1,width] <<- 1
    tt[width,1] <<- 1
  }
  
  # Calculating the next step
  calc_m <- function() {
    tm <- tt %*% m %*% tt - m
    tm[tm == 1 | tm > 3] <- 0
    tm <- tm + m
    tm[tm < 3] <- 0
    tm[tm > 0] <- 1
    m <<- tm
  }
  ####################################################
  
  # Initially set things
  object <<- 1
  width <<- 20
  constructMatrix(width, object)
  constructTransformation(width)
  
  # Event handler called when button is clicked
  observeEvent(input$stepButton, {
    # Plot the image
    output$plot <- renderD3heatmap({ 
      x <- y <- c(1:width)
      calc_m()
      # (Ab)use d3heatmap for convenience
      d3heatmap(m, dendrogram = "none", 
                colors = "Purples",
                width = "100%",
                xaxis_font_size = "0pt", yaxis_font_size = "0pt",
                xaxis_height = 1, yaxis_width = 1)
    })
  })
  
  # Event handler called when button is clicked
  observeEvent(input$startButton, {
    # Plot the image
    object <<- input$objectSelect
    constructMatrix(width, object)
    constructTransformation(width)
    
    x <- y <- c(1:width)
    output$plot <- renderD3heatmap({ 
      invalidateLater(200)
      calc_m()
      d3heatmap(m, dendrogram = "none", 
                colors = "Purples",
                width = "100%",
                xaxis_font_size = "0pt", yaxis_font_size = "0pt",
                xaxis_height = 1, yaxis_width = 1)
    })
  })
  
  # Event handler called when button is clicked
  observeEvent(input$stopButton, {
    # Plot the image
    output$plot <- renderD3heatmap({ 
      x <- y <- c(1:width)
      calc_m()
      # (Ab)use d3heatmap for convenience
      d3heatmap(m, dendrogram = "none", 
                colors = "Purples",
                width = "100%",
                xaxis_font_size = "0pt", yaxis_font_size = "0pt",
                xaxis_height = 1, yaxis_width = 1)
    })
  })
})


# Run the application 
shinyApp(ui = ui, server = server)
