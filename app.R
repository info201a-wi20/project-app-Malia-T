library(shiny)
library(wbstats)
library(tidyr)
library(ggplot2)
library(maps)

source("project.R")

ui <- fluidPage (
  titlePanel("Study on Economic Status and Education Rates by Country"),
  h2("Data Introduction"),
  p(),
  show_edu_input
)

show_edu_input <- checkboxInput(
  inputId = "edu_graph",
  label = "Show Education Graph",
  value = TRUE
)

server <- function(input, output) {
  if(input_list$edu_graph == TRUE){
    the_plot <- the_plot + geom_smooth(mapping = aes_string(
      x = input_list$feature_choice,
      y = "price",
      color = "cut"
    ),se = FALSE)
  }
}

shinyApp(ui = ui, server = server)
