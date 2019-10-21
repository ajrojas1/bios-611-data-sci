# Alfredo Rojas
# BIOS 611: Project 2
# Shiny App

library(tidyverse)
library(shiny)
library(shinydashboard)
source("helper_functions.R")

data_food = load_data("data/UMD_Services_Provided_20190719.tsv")

# Some ideas: Maybe at the very least, have two options on a side bar for two plots (depending on
# different data.frames. . . For each plot, have the user select variables to visualize)

ui = dashboardPage(
  
  dashboardHeader(title = "Urban Ministries of Durham Data Exploration"),
  dashboardSidebar(),
  
  # plot in main body ---
  dashboardBody(
    
    # row-based layout for plot ---
    fluidRow(
      
      # first box with plot ---
      box(title = "Monthly Pounds of Food", status = "primary", plotOutput("plot1", height = 250)),
      
      # Second box for select input ---
      box(
        title = "Select", status = "warning", 
        "You can select which year", br(), "you want to see", 
        selectInput(inputId = "select", label = "Choose Year", choices = 2000:2019)
      )
    )
  )
)

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
  
}

shinyApp(ui = ui, server = server)
