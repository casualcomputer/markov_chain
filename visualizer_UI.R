library(shiny)
library(shinyMatrix)
library(visNetwork)

# Define UI
ui <- fluidPage(
  titlePanel("Dynamic Markov Chain Model with Validation"),
  sidebarLayout(
    sidebarPanel(
      fileInput("fileUpload", "Upload Q Matrix (as .RData file)", accept = ".RData"),
      numericInput(
        "matrixSize",
        "Define matrix size:",
        4,
        min = 2,
        max = 10
      ),
      actionButton("generate", "Generate Matrix"),
      matrixInput(
        "transitionMatrix",
        label = "Transition Matrix:",
        value = as.matrix(rbind(
          c(0, 0.25, 0, 0.25),
          c(0.166, 0, 0.166, 0.166),
          c(0, 0.25, 0, 0.25),
          c(0, 0, 0, 0)
        )),
        rows = list(extend = FALSE, names = TRUE),
        cols = list(extend = FALSE, names = TRUE)
      ),
      textOutput("matrixValidation"),
      actionButton("updateGraph", "Update Graph")
    ),
    mainPanel(
      sliderInput(
        "slider",
        label = "Time elapsed",
        min = min(mydata$a),
        max = max(mydata$a),
        value = min(mydata$a),
        step = 1,
        animate =
          animationOptions(interval = 200, loop = TRUE)
      ),
      textOutput("timeStepsDisplay"),
      visNetworkOutput("markovGraph")
    )
  )
)

server <- function(input, output, session) {
  source("visualize.R")
  source("helpers.R")

  
  Q_matrix <- reactiveVal(as.matrix(rbind(
    c(0, 0.25, 0, 0.25),
    c(0.166, 0, 0.166, 0.166),
    c(0, 0.25, 0, 0.25),
    c(0, 0, 0, 0)
  )))  # Initialize as reactive value
  Q_msm <- reactiveVal() 
 
  
  observeEvent(input$generate, {
    new_matrix <- matrix(0, input$matrixSize, input$matrixSize, 
                         dimnames = list(1:input$matrixSize, 1:input$matrixSize))
    Q_matrix(new_matrix)
  })
  
  observeEvent(input$fileUpload, {
    req(input$fileUpload)
    load(input$fileUpload$datapath)
    if (exists("Q_matrix")) {
      Q_matrix(get("Q_matrix"))  # Load Q_matrix into reactive variable
      cav.msm <- msm(state ~ years, subject = PTNUM, data = cav, qmatrix = Q_matrix, deathexact = 4)
      Q_msm(cav.msm) 
    } else {
      showNotification("No 'Q_matrix' found in the uploaded file.", type = "error")
    }
  })
  
  observeEvent(input$updateGraph, {
    Q <- Q_matrix()  # Access the reactive matrix
    if (is.null(Q)) {
      showNotification("Q_matrix is not defined.", type = "error")
      return()
    }
    state_names <- colnames(Q)
    output$markovGraph <- renderVisNetwork({
      create_and_plot_interactive_graph(Q, state_names)
    })
  })
  
  observeEvent(input$slider, {
    Q <- Q_matrix()  # Ensure Q_matrix is accessed correctly
    if (!is.null(Q)) {
      Q_star <- pmatrix.msm(cav.msm, t=input$slider) #transition probability at time t
      output$markovGraph <- renderVisNetwork({
        create_and_plot_interactive_graph(Q_star, colnames(Q_star))
      })
    }
  })
  
  output$timeStepsDisplay <- renderText({
    paste("Slider Value:", input$slider)
  })
}


# Run the application
shinyApp(ui = ui, server = server)
