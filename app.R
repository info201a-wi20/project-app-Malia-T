library(shiny)
library(wbstats)
library(tidyr)
library(ggplot2)
library(maps)

source("project.R")


home <- tabPanel(
  "Home",
  titlePanel("Introduction"),
   p("Through this project, we are attempting to draw relationships and comparisons between these two variables; economy rates of countries and higher rates of education in different countries.
   Some of the questions that will facilitate in drawing these comparisons between the two variables inlcude:
   Is there a relationship between economic status of a country and their graduation rates?
   This question is aimed to help us finally analyze and answer if there is a relationship between the two factors; if there is a correlation between the two, and if so, the type of correlation (positive, negative).
   Rank the education rate from highest to lowest among countries worldwide, and also show their economic trend.
   This question will help us understand which country has the highest rates of highest education and which has the lowest rates.
   How does the correlation of US higher education rates vs. economy look like with respect to US events?
   This question will help us understand how these values compare to how well the country is faring.
   Which regions tend to have a higher GDP? Higher graduation rates? What could these results entail?
   This will help us understand if there is a certain region where more affluent individuals use overseas bank accounts to store money, for example. These findings could prompt many more questions."),
   
   p("We chose this topic, as we believe that education is a very essential tool to a person's overall success in life irrespective of age. Just like every human being requires oxygen to survive, 
   education too is very essential to survive in this world. Education provides people the knowledge and skills they need in order to survive. By analyzing these questions relating to education and how the economy rates of countries affect education - we hope to be able to better understand which countries are doing well in terms of economy and higher rates of education, and which countries are not doing well in these aspects. By understanding these aspects, we as students can work on spreading awareness regarding countries that are low on higher education rates and work in small ways that impact those countries and improve the overall higher educational rates. Thus, we believe that the topic chosen is important. Data Description
   Our data is from the World Bank and the Organisation for Economic Co-Operation and Development. Our World Bank data shows countries' GDP per capita and the OECD data shows education rates for different countries. OECD has columns of education rates for upper-secondary education, post-secondary education, short-cycle tertiary education, as well as college degrees such as a bachelor's or equivalent. We are focusing on people who obtain bachelor's degrees the first time they enter university.
   The data was collected and distributed by the respective organizations."),
  
   p("World Bank Data: https://data.worldbank.org/indicator/NY.GDP.PCAP.CD?end=2018&start=2018&view=bar"),
  
   p("OECD Data: https://stats.oecd.org/Index.aspx?datasetcode=EAG_GRAD_ENTR_RATES")
)

q1 <- tabPanel(
  "Econ. Status & Grad Rates",
  titlePanel("Is there a relationship between economic 
             status and graduation rates?"),
  sidebarLayout(
    sidebarPanel(
      plot1_input_world <- selectInput(inputId = "year_select_plot1", label = "Year",
                                 choices = c(2005, 2010, 2011, 2012, 2013, 2014, 
                                             2015, 2016, 2017), 
                                 selected = 2005),
      
      plot1_input_country <- selectInput(inputId = "country_select_plot1", label = "Country",
                                           choices = mean_data$Country, 
                                           selected = "Argentina")

    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Worldwide", plotOutput(outputId = "plot_1_output_worldwide")),
        tabPanel("By Country", 
                 p(plotOutput(outputId = "eco_trend", height = 500, width = 700)),
                 p(plotOutput(outputId = "edu_trend", height = 500, width = 700)))
        
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


q4 <- tabPanel(
  "Worldwide GDP & Graduation Rate",
      #select year#
      #############
      year_input <- selectInput(
        inputId = "year_map",
        label = "Year",
        choices = c(2005,2010,2011,2012,2013,2014,2015,2016,2017),
        selected = 2005),
    mainPanel(
      tabsetPanel(
      tabPanel("Education", plotOutput(outputId = "edu_map_plot")),
      tabPanel("Economy", plotOutput(outputId = "eco_map_plot")))
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
  
  output$plot_1_output_worldwide <- renderPlot({
    # used for plotting worldwide over years
    plot1_input_world <- input$year_select_plot1
    plot_1_year_input <- filter(df, Year == as.integer(plot1_input_world))
    
    plot1_input_df <- left_join(plot_1_year_input, mean_data, by = "Country") 
    
    q1_plot_world <- ggplot(data = plot_1_input_df, aes(x = mean_data$mean_gdp, y = mean_data$mean_grad_rate ))+
      
      geom_point(color = "red")+
      labs(title = "Economy and Rates of Education Change", x = "GDP in USD", y = "Graduation Rate %")+
      geom_smooth(method=lm, se = FALSE)+
      theme_minimal()+
      theme(axis.line = element_line(size = 0.5, linetype = "solid",colour = "black"))
    return(q1_plot_world)
  })
  
  # Used for country output: gdp
  output$eco_trend <- renderPlot({
    plot1_input_country <- input$country_select_plot1
    plot1_countries <- df %>% 
      filter(Country == plot1_input_country)
    
    q1_plot_eco <- ggplot(data = plot1_countries, aes(x = Year, y = Economy)) +
      geom_point(color = "red") +
      labs(title = paste("Economy for", plot1_input_country, "Over Time"), x = "Years", y = "GDP in USD") +
      xlim(2005, 2017) +
      geom_smooth(method = lm, se = FALSE) +
      theme_minimal() +
      theme(axis.line = element_line(size = 0.5, linetype = "solid", colour = "black"))
    return(q1_plot_eco)
  })
  
  # Used for country output: edu
  output$edu_trend <- renderPlot({
    plot1_input_country <- input$country_select_plot1
    plot1_countries <- df %>% 
      filter(Country == plot1_input_country)
    
    q1_plot_edu <- ggplot(data = plot1_countries, aes(x = Year, y = Education)) +
      geom_point(color = "red") +
      labs(title = paste("Education for", plot1_input_country, "Over Time"), x = "Years", y = "Education Rate") +
      xlim(2005, 2017) +
      geom_smooth(method = lm, se = FALSE) +
      theme_minimal() +
      theme(axis.line = element_line(size = 0.5, linetype = "solid", colour = "black"))
    return(q1_plot_edu)
  })
  
}

View(plot_1_year_input)

shinyApp(ui = ui, server = server)

