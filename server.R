server <- function(input, output) {
  
  #edu map#
  #########
  
  output$edu_map_plot <- renderPlot({
    year_input <- input$year_map
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
    year_input <- input$year_map
    eco_input <- df %>% 
      filter(Year == as.integer(year_input))
    where_to_cut <- c(-Inf,0 , 10000, 25000, 40000, 55000, 70000, 85000, Inf)
    label <- c("< 10000%","$10,000 to $25,000", "$25,000 to $40,0000", "$40,000 to $55,000", "$55,000 to $70,000", "$70,000 to $85,000", "$85,000 to $100,000","> $100,000")
    eco_df <- mutate(eco_input, change_labels = cut(eco_input$Economy, breaks = where_to_cut, labels = label))
    eco_map_df <- left_join(world_map, eco_df, by = "iso3c")
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
  
  
  #eco & edu relationship trend#
  ##############################   
  output$plot_1_output_worldwide <- renderPlot({
    # used for plotting worldwide over years
    plot1_input_world <- input$year_select_plot1
    plot_1_year_input <- filter(df, Year == as.integer(plot1_input_world))
    
    q1_plot_world <- ggplot(data = plot_1_year_input, aes(x = Economy, y = Education ))+
      
      geom_point(color = "red")+
      labs(title = "Economy and Rates of Education Change", x = "GDP in USD", y = "Graduation Rate %")+
      geom_smooth(method=lm, se = FALSE)+
      theme_minimal()+
      theme(axis.line = element_line(size = 0.5, linetype = "solid",colour = "black"))
    return(q1_plot_world)
  })
  
  
  #eco bar chart Question 2#
  ########################## 
  output$eco_bar_plot <- renderPlot({
    mean_data %>%
      arrange(-mean_data$mean_gdp) %>%
      head(input$country_slider) %>%
      ggplot(aes(x = reorder(Country, -mean_gdp), y = mean_gdp))+
      geom_col(fill = "orange")+
      labs(y = "GDP in US Dollars",
           title = "Ranking of Average Education Rate and GDP Between Countries"
      )+
      theme_minimal()+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.title.y = element_text(size = 12),
            axis.text.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 2)),
            axis.line.y = element_line(size = 0.5, linetype = "solid",
                                       colour = "black")
      )
    
  })
  
  #edu bar chart Question 2#
  ##########################   
  output$edu_bar_plot <- renderPlot({
    mean_data %>%
      arrange(-mean_data$mean_gdp) %>%
      head(input$country_slider) %>%
      ggplot(aes(x = reorder(Country, -mean_gdp), y = mean_grad_rate))+
      geom_col(fill = "pink")+
      labs( x = "Country", 
            y = "Graduation Rate")+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 90, size = 12,hjust = 1, vjust = 0.25),
            axis.text.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 18)),
            legend.position = "none",
            axis.title = element_text(size = 12),
            axis.line = element_line(size = 0.5, linetype = "solid",
                                     colour = "black")
            
      )    
    
  })
  
  
  #Mean Data Table for Question 2#
  ################################
  output$mean_data <- renderTable({
    mean_data %>%
      arrange(-mean_data$mean_gdp) %>%
      head(input$country_slider)
    
  })
  
  #event gdp chart for Question 3#
  ################################
  output$event_gdp <- renderPlot({
    usa %>%
      filter(Events == input$events_select | (input$events_select == "All Events")) %>%
      ggplot(aes(x = Year,y = Economy)) +
      geom_point(size = 3)+
      geom_line()+
      labs(y = "GDP in US Dollars")+
      scale_x_continuous(breaks=seq(2010, 2017, 1))+
      theme_minimal()+
      theme(axis.title = element_text(size = 12),
            axis.text.x = element_text( size = 12),
            axis.text.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 18)),
            axis.line = element_line(size = 0.5, linetype = "solid",
                                     colour = "black")
      )
  })
  
  output$event_gdp_tab3 <- renderPlot({
    usa %>%
      filter(Events == input$events_select | (input$events_select == "All Events")) %>%
      ggplot(aes(x = Year,y = Economy)) +
      geom_point(size = 3)+
      geom_line()+
      labs(y = "GDP in US Dollars")+
      scale_x_continuous(breaks=seq(2010, 2017, 1))+
      theme_minimal()+
      theme(axis.title = element_text(size = 12),
            axis.text.x = element_text( size = 12),
            axis.text.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 18)),
            axis.line = element_line(size = 0.5, linetype = "solid",
                                     colour = "black")
      )
  })
  
  #event grad chart for Question 3#
  #################################
  output$event_grad <- renderPlot({
    usa %>%
      filter(Events == input$events_select | (input$events_select == "All Events")) %>%
      ggplot(aes(x = Year,y = Education)) +
      geom_point(size = 3)+
      geom_line()+
      labs(y = "Graduation Rate %")+
      scale_x_continuous(breaks=seq(2010, 2017, 1))+
      theme_minimal()+
      theme(axis.title = element_text(size = 12),
            axis.text.x = element_text( size = 12),
            axis.text.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 18)),
            axis.line = element_line(size = 0.5, linetype = "solid",
                                     colour = "black")
      )
  })
  
  output$event_grad_tab3 <- renderPlot({
    usa %>%
      filter(Events == input$events_select | (input$events_select == "All Events")) %>%
      ggplot(aes(x = Year,y = Education)) +
      geom_point(size = 3)+
      geom_line()+
      labs(y = "Graduation Rate %")+
      scale_x_continuous(breaks=seq(2010, 2017, 1))+
      theme_minimal()+
      theme(axis.title = element_text(size = 12),
            axis.text.x = element_text( size = 12),
            axis.text.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 18)),
            axis.line = element_line(size = 0.5, linetype = "solid",
                                     colour = "black")
      )
  })
  
  #Dynamic usa Table for Question 3#
  ########################################
  output$usa <- renderTable({
    usa %>%
      filter(Events == input$events_select | (input$events_select == "All Events"))
  })
  
  #table for q4 education
  output$mean_edu_data <- renderTable({
    df %>%
      filter(Year == input$year_map) %>%
      select(Country,Education) %>% 
      arrange(-Education)
    
  })
  
  #table for q4 economy
  output$mean_eco_data <- renderTable({
    df %>%
      filter(Year == input$year_map) %>%
      select(Country,Economy) %>% 
      arrange(-Economy)
    
  })
  
  #text analysis for q4 edu
  output$mean_world_edu <- renderText({
    mean_grad <- world_mean %>% 
      filter(Year == input$year_map) %>% 
      pull(mean_grad_rate)
    paste("The world average graduation rate in",input$year_map,"is",round(mean_grad, digits = 2),"%.")
  })
  
  #text analysis for q4 eco
  output$mean_world_eco <- renderText({
    mean_gdp <- world_mean %>% 
      filter(Year == input$year_map) %>% 
      pull(mean_gdp)
    paste("The world average GDP (in US Dollars) in",input$year_map,"is <b>$",round(mean_gdp, digits = 2),"</b>.")
  })
  
  # mean plot for q1
  output$plot_1_mean <- renderPlot({
    mean_plot <- ggplot(mean_data,aes(x = mean_gdp,y = mean_grad_rate ))+
      geom_point(color = "red")+
      labs(title = "Economy and Rates of Education Change", x = "GDP in USD", y = "Graduation Rate %")+
      geom_smooth(method=lm, se = FALSE)+
      theme_minimal()+
      theme(axis.line = element_line(size = 0.5, linetype = "solid",colour = "black"))
    
    return(mean_plot)
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