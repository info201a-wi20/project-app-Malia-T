library(shiny)
library(wbstats)
library(tidyr)
library(ggplot2)
library(maps)

source("project.R")

#UI layouts (allows for nesting!)
#static content
#control widgets
#reactive outputs

ui <- fluidPage(
  titlePanel("Economy Growth Rates of Countries versus Graudation Rates of Higher Education")
  
)

server <- function(input, output) {
  # use values from `input` list
  # assign values to `output` list
  # we'll fill this in soon
}

shinyApp(ui = ui, server = server)
