# Alfredo Rojas
# BIOS 611: Project 2
# Shiny App

library(tidyverse)
library(shiny)
library(shinydashboard)
library(rsconnect)
source("helper_functions.R")

# read in data
data_food = load_data("data/UMD_Services_Provided_20190719.tsv")

# User interface
ui = dashboardPage(
  
  # Header w/ title ---
  dashboardHeader(title = "Urban Ministries of Durham: Food Data",
                  titleWidth = 400),
  
  # remove sidebar for this app
  dashboardSidebar(disable = TRUE),
  
  # plot in main body ---
  dashboardBody(
    
    # row-based layout for plot ---
    fluidRow(
      
      # first box with plots ---
      box(title = "Food Barplot", status = "primary", plotOutput("plot1", height = 250)),
      
      # Tab box with plots 2 and 3 line graphs ---
      tabBox(
        tabPanel(title = "Visitors Line Graph", status = "primary", plotOutput("plot2", height = 250)),
        tabPanel(title = "Sum of People Receiving Food", status = "primary", plotOutput("plot3", height = 250))
      )
    ),
    
    fluidRow(

      # Third box for select input ---
      box(
        title = "Select Year", status = "warning",
        # "You can select which year you want to see",
        selectInput(inputId = "select", label = "Choose Year for Monthly Data", 
                    choices = 2000:2019, selectize = FALSE)
      ),
      
      # Fourth box for description of data ---
      box(
        title = "Description of Data", width = 6, background = "light-blue",
        "UMD has collected data on food distribution over the years. 
        Food pounds have been recorded, as well as amount of people for whom food was provided.
        Choose a year to see monthly observations. Some years do not contain data. 
        What trends can you notice over the years?"
      )
    )
  )
)

# server function
server = function(input, output) {
  
  # reactive data based on year selection ---
  react_data = reactive({
    select_year = as.numeric(input$select)
    return(data_food[data_food$Year == select_year, ])
  })
  
  output$plot1 = renderPlot({
    
    # used data based on selection from reactive ---
    current_data = react_data()
    
    # use food_plot() from helper_functions.R ---
    food_plot(current_data, input$select)

  })
  
  output$plot2 = renderPlot({
    
    # used data based on selection from reactive --
    current_data = react_data()
    
    # use visits_plot() from helper_functions.R---
    visits_plot(current_data, input$select)
  })
  
  output$plot3 = renderPlot({
    
    # used data based on selection from reactive --
    current_data = react_data()
    
    # use food_provided() from helper_functions.R---
    food_provided(current_data, input$select)
  })
  
}

shinyApp(ui = ui, server = server)
