library(shiny)
library(wbstats)
library(tidyr)
library(ggplot2)
library(maps)

source("project.R")
home <- tabPanel(
  "Home",
  titlePanel("Introduction")
)

#correlation
#select by year
#select by country
q1 <- tabPanel(
  "Econ. Status & Grad Rates"
)
q2 <- tabPanel(
  "Education & Econ. Status Ranked"
)
q3 <- tabPanel(
  "US Correlation & US Events"
)

#worldmap
#by year
#two maps to choose if want to see both
q4 <- tabPanel(
  "Worldwide GDP & Graduation Rate",
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "username", label = "What is your name?")
    ),
    mainPanel(
      h3("Primary Content"),
      p("Plots, data tables, etc. would go here")
    )
  )
)

ui <- fluidPage (
  titlePanel("Study on Economic Status and Education Rates by Country"),
  navbarPage(
    title = "Info 201, AH Team 1",
    home,
    q1,
    q2,
    q3,
    q4
  )
)



server <- function(input, output) {
  
}

shinyApp(ui = ui, server = server)
