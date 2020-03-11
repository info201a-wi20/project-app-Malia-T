library(shiny)
library(wbstats)
library(tidyr)
library(ggplot2)
library(maps)

source("project.R")

ui <- fluidPage (
  titlePanel("Study on Economic Status and Education Rates by Country"),
  navbarPage(
    title = "",
    tabPanel(title = "Home"),
    tabPanel(title = "Econ. Status & Grad Rates"),
    tabPanel(title = "Education & Econ. Status Ranked"),
    tabPanel(title = "US Correlation & US Events"),
    tabPanel(title = "Worldwide GDP & Graduation Rate")
  ),
  
  h2("Data Introduction")
)

server <- function(input, output) {
  
#UI layouts (allows for nesting!)
#static content
#control widgets
#reactive outputs
}

shinyApp(ui = ui, server = server)
