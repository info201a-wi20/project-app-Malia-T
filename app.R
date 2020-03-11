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
      #select year#
      #############
      year_input <- selectInput(
        inputId = "year_map",
        label = "Year",
        choices = c(2005,2010,2011,2012,2013,2014,2015,2016,2017),
        selected = 2005)
    ),
    mainPanel(
      tabsetPanel(
      tabPanel("Education", plotOutput(outputId = "edu_map_plot")),
      tabPanel("Economy", plotOutput(outputId = "eco_map_plot")))
    )
))

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
  
  #edu map#
  #########
  output$edu_map_plot <- renderPlot({
    year_input<- input$year_map
    edu_input <- df %>% 
      filter(Year == as.integer(year_input))
    where_to_cut <- c(-Inf, 0, 10, 20, 30, 40, 50, 60, Inf)
    label <- c("0%","0% to 10%", "10% to 20%", "20% to 30%", "30% to 40%", "40% to 50%", "50% to 60%","> 60%")
    edu_df <- mutate(edu_input, change_labels = cut(edu_input$Education, breaks = where_to_cut, labels = label))
    grad_map_df <- left_join(world_map, edu_df, by = "iso3c")
    grad_map_df$change_labels <- factor(grad_map_df$change_labels, levels = rev(levels(grad_map_df$change_labels)))
    q4_edu_map <- ggplot(data = grad_map_df, mapping = aes(x = long, y = lat)) +
      geom_polygon(aes(group = group, fill = change_labels)) +
      scale_fill_brewer(palette = "BuPu",direction = -1) +
      coord_quickmap() +
      labs(title = "Worldwide Graduation Rate") +
      theme_void()+
      guides(fill = guide_legend(title = "Graduation Rate"))
    return(q4_edu_map)
  })
 
  #eco map#
  ######### 
  output$eco_map_plot <- renderPlot({
    year_input<- input$year_map
    eco_input <- df %>% 
      filter(Year == as.integer(year_input))
    where_to_cut <- c(-Inf,0 , 10000, 25000, 40000, 55000, 70000, 85000, Inf)
    label <- c("< 10000%","$10,000 to $25,000", "$25,000 to $40,0000", "$40,000 to $55,000", "$55,000 to $70,000", "$70,000 to $85,000", "$85,000 to $100,000","> $100,000")
    edu_df <- mutate(eco_input, change_labels = cut(eco_input$Economy, breaks = where_to_cut, labels = label))
    eco_map_df <- left_join(world_map, edu_df, by = "iso3c")
    eco_map_df$change_labels <- factor(eco_map_df$change_labels, levels = rev(levels(eco_map_df$change_labels)))
    q4_eco_map <- ggplot(data = eco_map_df, mapping = aes(x = long, y = lat)) +
      geom_polygon(aes(group = group, fill = change_labels)) +
      scale_fill_brewer(palette = "OrRd",direction = -1) +
      coord_quickmap() +
      labs(title = "Worldwide Economy GDP Rate") +
      theme_void()+
      guides(fill = guide_legend(title = "GDP Rate"))
    return(q4_eco_map)
  })
}

shinyApp(ui = ui, server = server)
