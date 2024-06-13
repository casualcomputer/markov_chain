
# Dynamic Markov Chain Model with Validation

This project provides a Shiny application designed to visualize the dynamics of Markov Chains. It allows users to interactively modify the transition matrix of a Markov chain and observe the effects on the state transitions over time, visualized through a network graph.

[Launch web app](https://creative-analytics.shinyapps.io/markov_chain/)

## Features

- **Interactive Transition Matrix**: Users can input and modify the transition matrix that defines the Markov chain.
- **File Upload Capability**: Users can upload their own matrix in RData format to set the transition matrix.
- **Dynamic Visualization**: The network graph updates dynamically based on changes to the transition matrix or the time steps controlled by a slider.
- **Validation of Transition Matrix**: The application checks if the transition matrix is valid (i.e., rows sum to 1).
- **Time Evolution**: Users can simulate the Markov chain's evolution over time using a slider that adjusts the elapsed time, influencing the transition probabilities.

## Installation

To run this Shiny app on your local machine, you need to have R and Shiny installed. Follow these steps:

1. **Install R**: Download and install R from [CRAN](https://cran.r-project.org/).
2. **Install Shiny**: Run the following command in R to install the Shiny package:

   ```R
   install.packages("shiny")
   ```

3. **Additional Packages**: Install other required packages:

   ```R
   install.packages(c("shinyMatrix", "visNetwork", "expm"))
   ```

4. **Clone the Repository**: Clone this repository to your local machine or download the files directly.

   ```bash
   git clone https://github.com/casualcomputer/markov_chain
   ```

5. **Run the App**: Open R and set your working directory to the project folder, then run:

   ```R
   shiny::runApp()
   ```

## Usage

After launching the app, you will be greeted with a user interface consisting of:

- **Sidebar**:
  - Upload your `.RData` file containing the `Q_matrix`.
  - Adjust the size of the matrix and manually input or modify values in the transition matrix.
  - Click "Generate Matrix" to initialize a new matrix of the specified size with zero values.
  - Click "Update Graph" to visualize changes.
  - The transition matrix's validity is displayed below the matrix input.

- **Main Panel**:
  - Use the slider to adjust the time steps and watch the Markov chain evolve.
  - The network graph displays the current state of the Markov chain.
  - Text output shows the current setting of the time slider.

## Contributing

Contributions to this project are welcome! Please fork the repository and submit a pull request with your enhancements, or open an issue if you find bugs or have suggestions.

---

Enjoy simulating and visualizing Markov chains dynamically!
