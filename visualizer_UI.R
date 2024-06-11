library(shiny)
library(shinyMatrix)
library(visNetwork)

# Define UI
ui <- fluidPage(
  titlePanel("Dynamic Markov Chain Model with Validation"),
  sidebarLayout(
    sidebarPanel(
      numericInput("matrixSize", "Define matrix size:", 4, min = 2, max = 10),
      actionButton("generate", "Generate Matrix"),
      fileInput("fileUpload", "Upload Q Matrix (as .RData file)", accept = ".RData"),
      matrixInput("transitionMatrix", 
                  label = "Transition Matrix:",
                  value = matrix(0, 4, 4, dimnames = list(1:4, 1:4)),
                  rows = list(extend = FALSE, names = TRUE),
                  cols = list(extend = FALSE, names = TRUE)),
      textOutput("matrixValidation"),
      actionButton("updateGraph", "Update Graph")
    ),
    mainPanel(
      visNetworkOutput("markovGraph")
    )
  )
)

server <- function(input, output, session) {
  source("visualize")
  source("helpers.R")
  observeEvent(input$generate, {
    updateMatrixInput(session, "transitionMatrix", 
                      value = matrix(0, input$matrixSize, input$matrixSize, 
                                     dimnames = list(1:input$matrixSize, 1:input$matrixSize)))
  })
  
  observeEvent(input$fileUpload, {
    req(input$fileUpload)  # Ensure a file is uploaded
    # Load the uploaded RData file
    load(input$fileUpload$datapath)
    # Assume 'Q_matrix' is the name of the loaded matrix
    if (exists("Q_matrix")) {
      updateMatrixInput(session, "transitionMatrix", value = Q_matrix)
      print(paste("names: ", names(Q_matrix)))
    } else {
      showNotification("No 'Q_matrix' found in the uploaded file.", type = "error")
    }
  })
  
  observeEvent(input$updateGraph, {
    Q <- as.matrix(sapply(input$transitionMatrix, as.numeric))  # Convert matrix input to numeric
    Q <- reshape_vector_to_matrix(Q, nrow = input$matrixSize, ncol = input$matrixSize)
    print(Q)
    state_names <- colnames(Q)
    print(paste("state name",state_names))
    output$markovGraph <- renderVisNetwork({
      create_and_plot_interactive_graph(Q, state_names)
    })
  })
  
  # Validate the transition matrix
  output$matrixValidation <- renderText({
    Q <- as.matrix(sapply(input$transitionMatrix, as.numeric))  # Convert matrix input to numeric
    if (is.null(Q)) {
      return("Matrix is not defined.")
    }
    
    # Check if all rows sum to 1
    row_sums <- rowSums(Q)
    if (all(row_sums == 1)) {
      return("The matrix is valid: All rows sum to 1.")
    } else {
      return("The matrix is invalid: Not all rows sum to 1.")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
