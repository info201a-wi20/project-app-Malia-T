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
  "Econ. Status & Grad Rates",
  titlePanel("Is there a relationship between economic 
             status and graduation rates?"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "year_slider", label = "Pick a Year",
                  min = 2005, max = 2017, value = 2005)
    ),
    mainPanel(
      h3("Relationship Between Economic Status and Graduation Rates"),
      p(
        plotOutput(outputId = "plot_1_output")
      )
    )
  )
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
  output$plot_1_output <- renderPlot({
    ggplot(mean_data,aes(x = mean_gdp,y = mean_grad_rate ))+
      
      geom_point(color = "red")+
      labs(title = "Economy and Rates of Education Change", x = "GDP in USD", y = "Graduation Rate %")+
      geom_smooth(method=lm, se = FALSE)+
      theme_minimal()+
      theme(axis.line = element_line(size = 0.5, linetype = "solid",colour = "black"))
  })
}

shinyApp(ui = ui, server = server)
