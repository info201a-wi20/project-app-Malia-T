library(shiny)
library(wbstats)
library(tidyr)
library(ggplot2)
library(maps)

source("project.R")

ui <- fluidPage (
  titlePanel("Study on Economic Status and Education Rates by Country"),
  
  h2("Data Introduction"),
  p()
)

server <- function(input, output) {
  
#UI layouts (allows for nesting!)
#static content
#control widgets
#reactive outputs
}

shinyApp(ui = ui, server = server)
