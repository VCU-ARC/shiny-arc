# Load required libraries
library(shiny)
library(DT) # For DataTables

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Gene Information Viewer"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      # Slider input for association strength
      sliderInput("association_strength", "Association Strength", min = 0, max = 1, value = c(0, 1), step = 0.01),
      # Slider input for padj
      sliderInput("padj", "Adjusted p-value", min = 0, max = 0.05, value = c(0, 1), step = 0.01),
      # Slider input for number of evidences
      sliderInput("number_of_evidences", "Number of Evidences", min = 0, max = 12, value = c(0, 12), step = 1),
      # Select input for category
      selectInput("category", "Category", choices = c("All", "Excellent", "Good", "Moderate"))
    ),
    # Show output
    mainPanel(
      DT::dataTableOutput("gene_table")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Read data
  data <- read.csv("data/demo.csv")
  
  # Filter data based on user inputs
  filtered_data <- reactive({
    req(input$association_strength, input$padj, input$number_of_evidences, input$category)
    
    filtered <- data
    
    # Filter based on association strength
    filtered <- filtered[filtered$association_strength >= input$association_strength[1], ]
    
    # Filter based on padj
    filtered <- filtered[filtered$padj <= input$padj[2], ]
    
    # Filter based on number of evidences
    filtered <- filtered[filtered$number_of_evidences >= input$number_of_evidences[1], ]
    
    # Filter based on category
    if (input$category != "All") {
      filtered <- filtered[filtered$category == input$category, ]
    }
    
    filtered
  })
  
  # Show filtered data in table
  output$gene_table <- DT::renderDataTable({
    DT::datatable(filtered_data(), options = list(pageLength = 10))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
