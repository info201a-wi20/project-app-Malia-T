library(shiny)
library(wbstats)
library(tidyr)
library(ggplot2)
library(maps)

source("project.R")

ui <- fluidPage (
  h1("Study on Economic Status and Education Rates by Country"),
  
  h2("Data Introduction"),
  p()
)

server <- function(input, output) {
  
}

shinyApp(ui = ui, server = server)
