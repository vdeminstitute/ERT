library(shiny)
library(shinyWidgets)
#library(tidyverse)

# Define UI for app that plots the ERT episodes ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Episodes of Regime Transformations (ERT)"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs, essentially all ERT parameters ----
    sidebarPanel(
      
      # Input: Text field for country names
      # taken from V-Dem data package (maybe use ERT package) ----
      selectInput("country_name", "Select country name", sort(vdemdata::vdem$country_name),
                  selected = "Sweden")
    ,
      # Enter time frame (need shinyWidgets for that) 
     numericRangeInput('years', 'Enter a time frame',
                        value = c(1900, 2020),
                        min = 1900, max = 2020)
    ,
      # Add sliders for all input parameters   
    sliderInput(inputId = "start_incl",
                label = "Start inclusion (minimum annual change)",
                min = 0.001,
                max = 0.5,
                value = 0.01)
    ,
    sliderInput(inputId = "cum_incl",
                label = "Cumulative inclusion (minimum total change)",
                min = 0.001,
                max = 0.5,
                value = 0.1)
    ,
    sliderInput(inputId = "year_turn",
                label = "Annual turn for termination",
                min = 0.001,
                max = 0.5,
                value = 0.03)
    ,
    sliderInput(inputId = "cum_turn",
                label = "Cumulative turn for termination",
                min = 0.001,
                max = 0.5,
                value = 0.1)
    ,
    sliderInput(inputId = "tolerance",
                label = "Tolerance (years)",
                min = 1,
                max = 10,
                value = 5)
    
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Episodes plot ----
      plotOutput(outputId = "ERTplot")
      
    )
  )
)

# Define server logic required to plot episodes ----
server <- function(input, output) {
  
  # Plot episodes as given by the ERT data set ----
  # depending on parameter choice
  output$ERTplot <- renderPlot({
   
  # Run the underlying plot_episodes function from ERT package 
ERT::plot_episodes(country = input$country_name,
                   years = input$years,
                   start_incl = input$start_incl,
                   cum_incl= input$cum_incl,
                   year_turn = input$year_turn,
                   cum_turn = input$cum_turn,
                   tolerance = input$tolerance)
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)