# install.packages("shiny")
# install.packages("shinyMatrix")
# install.packages("visNetwork")
# install.packages("msm")
# install.packages("igraph")

library(shiny)
library(shinyMatrix)
library(visNetwork)
library(msm)
library(igraph)

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
        min = 0,
        max = 30,
        value = 1,
        step = 1,
        animate =
          animationOptions(interval = 500, loop = TRUE)
      ),
      textOutput("timeStepsDisplay"),
      visNetworkOutput("markovGraph")
    )
  )
)

server <- function(input, output, session) {
  source("helpers.R")
  
  Q_matrix <- reactiveVal(as.matrix(rbind(
    c(0, 0.25, 0, 0.25),
    c(0.166, 0, 0.166, 0.166),
    c(0, 0.25, 0, 0.25),
    c(0, 0, 0, 0)
  )))  # Initialize as reactive value
  
  graph_initialized <- reactiveVal(FALSE)
  
  observeEvent(input$generate, {
    new_matrix <- matrix(
      0,
      input$matrixSize,
      input$matrixSize,
      dimnames = list(1:input$matrixSize, 1:input$matrixSize)
    )
    Q_matrix(new_matrix)
  })
  
  observeEvent(input$fileUpload, {
    req(input$fileUpload)
    load(input$fileUpload$datapath)
    if (exists("Q_matrix")) {
      Q_matrix(get("Q_matrix"))  # Load Q_matrix into reactive variable
      # print("Uploaded Q_matrix:")
      # print(Q_matrix)
    } else {
      showNotification("No 'Q_matrix' found in the uploaded file.", type = "error")
    }
  })
  
  observeEvent(input$updateGraph, {
    if (is.null(Q_matrix())) {
      showNotification("Q_matrix is not defined.", type = "error")
      return()
    }
    state_names <- colnames(Q_matrix())
    output$markovGraph <- renderVisNetwork({
      create_and_plot_interactive_graph(Q_matrix(), state_names)
    })
    graph_initialized(TRUE)
  })
  
  observeEvent(input$slider, {
    Sys.sleep(0.5)  # Introduce a delay of 0.5 seconds
    # print("current Q_matrix before plots:")
    # print(Q_matrix())
    if (!is.null(Q_matrix()) && graph_initialized()) {
      cav.msm <- msm(
        state ~ years,
        subject = PTNUM,
        data = cav,
        qmatrix = Q_matrix(),
        deathexact = 4
      )
      
      Q_star <- pmatrix.msm(cav.msm, t = input$slider) #transition probability at time t
      output$markovGraph <- renderVisNetwork({
        create_and_plot_interactive_graph(Q_star, colnames(Q_star))
      })
    }
  })
  
  output$timeStepsDisplay <- renderText({
    paste("Time Step =", input$slider)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
