# Group Name: Team 1
# Members: Serah Prakkat, Mei Zhao, Malia Cortez, Ryan Ros
# Assignment: Group Project


#procedure for filtering data
# Class: INFO 201
#
# The questions we are answering and analyzing are as follows:
#
# Is there a relationship between economic status of a country and their graduation rates? If so, what is the relationship?
# How does the correlation of U.S higher education rates vs. economy look like with respect to U.S events?
# Rank the education rate from highest to lowest among countries worldwide, and also show their economic trend.  
# Which regions tend to have a higher GDP? Higher graduation rates? What could these results entail?

library(wbstats)
library(ggplot2)
library(dplyr)
library(tidyr)
library(maps)

##################
###Extract Data###
##################

eco <- wb(country = "countries_only",indicator = c("NY.GDP.PCAP.CD"), mrv = 50)
edu <- read.csv("education_data.csv", stringsAsFactors = FALSE)

##education dataset##
#####################
#filter for world data
edu <- edu %>% 
  filter(substring(edu$International.students.exclusion,0,5) == "World",)

#filter for only bachelors, all ages, all sex, removed entries with NA values
#only include data of country code, country name, year and value
edu <- filter(edu, substring(edu$Education.level.and.programe.orientation.,0,8) == "Bachelor",)
edu <- filter(edu, substring(edu$Age,0,5) == "Total",)
edu <- filter(edu, SEX == "T",) %>% 
  filter(!is.na(Value)) %>% 
  select(ï..COUNTRY, Country, Year, Value )
  
#change column name
new_col_names <- c("Country_code", "Country","Year", "grad_rate")
colnames(edu) <- new_col_names

##economy dataset##
###################
#filter economy data to only include country code, year and GDP value
eco <- eco %>% 
  select(1:3)

#rename columns
new_col_names <- c("Country_code", "Year", "GDP")
colnames(eco) <- new_col_names

#change data types to math with each other
edu$Country_code <- as.character(edu$Country_code)
edu$Country <- as.character(edu$Country)
eco$Year <- as.integer(eco$Year)

#merge/combine the two data sets
##after filtering, we get data for most countries from 2005 to 2017
df <- inner_join(edu, eco, by = c("Year","Country_code"))


####################
###Summarize Data###
####################

#extract all the countries (name only appear once)
country_names <- distinct(edu, Country_code, Country)

#mean dataframe that has the mean graduation rate and mean gdp rate for
#each countries.
#the graduation rate and gdp was divided by the total number of years recorded in dataset
mean_data <- group_by(df,Country_code) %>% 
  summarise(mean_grad_rate = mean(grad_rate), mean_gdp = mean(GDP)) %>%  
  left_join(country_names, by = "Country_code")

#the world mean is the mean grad rate and mean gdp rate of all countries from 2005 to 2017
world_mean <- group_by(df, Year) %>% 
  summarise(mean_grad_rate = mean(grad_rate),mean_gdp = mean(GDP))

#Section 2.2, q2 distribution and trend#
########################################

#histogram to show the mean grad rate and gdp rate for the countries using mean_data
#grad_rate
world_mean_grad <- ggplot(mean_data, aes(x = mean_grad_rate)) + 
  geom_histogram( bins = 15, color="darkblue", fill="lightblue")+
  labs(title = "Distribution of Mean Graduation Rate Worldwide Over Years",
       x = "Mean Graduation Rate %",
       y = "Frequency")+
  scale_x_log10(breaks=c(10,20,30,40,50,60,70)) 
#gdp
world_mean_gdp <- ggplot(mean_data, aes(x = mean_gdp)) + 
  geom_histogram( bins = 15, color="red", fill="pink")+
  labs(title = "Distribution of Mean GDP Worldwide Over Years",
       x = "Mean GDP in US Dollars",
       y = "Frequency")

#graph the mean trend of grad rate and gdp for each country
#grad_rate
country_mean_grad <- ggplot(mean_data, aes(x = reorder(Country,-mean_grad_rate), y = mean_grad_rate))+
  geom_col(color = "red", fill = "pink")+
  theme(axis.text.x = element_text(angle = 90, size = 8,hjust = 1),legend.position = "none")+
  labs(title = "Average Graduation Rate % for Each Country", x = "Country", y = "Average Graduation Rate %")
#gdp
country_mean_gdp <- ggplot(mean_data, aes(x = reorder(Country,-mean_gdp), y = mean_gdp))+
  geom_col( color = "blue",fill = "lightblue")+
  theme(axis.text.x = element_text(angle = 90, size = 8,hjust = 1),legend.position = "none")+
  labs(title = "Average Economic GDP for Each Country",x = "Country", y = "GDP in US Dollars")

#############################
###Answering Our Questions###
#############################

#Question 1
#- Is there a relationship between economic status of a country 
#  and their graduation rates? If so, what is the relationship?
q1 <- ggplot(mean_data,aes(x = mean_gdp,y = mean_grad_rate ))+

  geom_point(color = "red")+
  labs(title = "Economy and Rates of Education Change", x = "GDP in USD", y = "Graduation Rate %")+
  geom_smooth(method=lm, se = FALSE)+
  theme_minimal()+
  theme(axis.line = element_line(size = 0.5, linetype = "solid",colour = "black"))
q1

#Question 2
#- Rank the education rate from highest to lowest among countries 
#  worldwide, and also show their economic trend.  
eco_plot <- ggplot(mean_data, aes(x = reorder(Country, -mean_gdp), y = mean_gdp))+
  geom_col(fill = "orange")+
  labs(y = "GDP in US Dollars",
       title = "Ranking of Average Education Rate and GDP Between Countries"
  )+
  theme_minimal()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 2)),
        axis.line.y = element_line(size = 0.5, linetype = "solid",
                                 colour = "black")
  )

edu_plot <- ggplot(mean_data, aes(x = reorder(Country, -mean_gdp), y = mean_grad_rate))+
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

eco_plot
edu_plot

#Question 3
#- How does the correlation of U.S higher education rates vs. economy look like with respect to U.S events?
#events from 
#https://www.thebalance.com/us-gdp-by-year-3305543#citation-4

events <- c("Patient Protection and Affordable Care Act, Dodd-Frank Wall Street Reform and Consumer Protection Act",
            "Japan Tohoku earthquake and tsunami", 
            "U.S. Fiscal cliff",
            "Budget sequestration",
            "Quantitative easing (QE) ends (aka large-scale asset purchases)", 
            "Trans-Pacific Partnership, Joint Comprehensive Plan of Action (aka Iran nuclear deal)", 
            "Presidential race",
            "Trump Tax Act (Tax Cuts and Jobs Act)")

#only use data from USA
usa <- df %>% 
  filter(Country_code == "USA")
#removed the outlier of data from 2005
usa <- usa[-c(1),]
#select only the year, grad_Rate, gdp and events
usa <- mutate(usa, Events = events) %>% 
  select(Year, grad_rate, GDP, Events)

#change to long format
usa_long <- usa %>% 
  gather(key = Group, value = Value, -Year, -Events)

#graph a line plot for grad_rate and gdp
event_gdp <- ggplot(data = usa,aes(x = Year,y = GDP)) +
    geom_point()+
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

event_grad <- ggplot(data = usa,aes(x = Year,y = grad_rate)) +
  geom_point()+
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

#table showing the events of each year
event_table <- select(usa, Year, Events)

event_gdp
event_grad

#Question 4
#- Which regions tend to have a higher GDP? Higher graduation rates? 
#  What could these results entail?

#get world map data
world_map <- map_data("world")
iso <- iso.alpha(world_map$region, n = 3)
world_map <- mutate(world_map, iso3c = iso)

#change current datafram column name to match the world map dataframe
new_col_names <- c("iso3c", "mean_grad_rate","mean_gdp", "Country")
colnames(mean_data) <- new_col_names

##education map##
#################

#devide the mean_grad_rate dataset into categories
where_to_cut <- c(-Inf, 0, 10, 20, 30, 40, 50, 60, Inf)
label <- c("0%","0% to 10%", "10% to 20%", "20% to 30%", "30% to 40%", "40% to 50%", "50% to 60%","> 60%")
mean_data_trimmed <- mutate(mean_data, change_labels = cut(mean_data$mean_grad_rate, breaks = where_to_cut, labels = label))

#join them
grad_map_df <- left_join(world_map, mean_data_trimmed, by = "iso3c")

#make the label in the reverse order
grad_map_df$change_labels <- factor(grad_map_df$change_labels, levels = rev(levels(grad_map_df$change_labels)))

#plot in a map
q4_edu_map <- ggplot(data = grad_map_df, mapping = aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = change_labels)) +
  scale_fill_brewer(palette = "BuPu",direction = -1) +
  coord_quickmap() +
  labs(title = "Worldwide Average Graduation Rate") +
  theme_void()+
  guides(fill = guide_legend(title = "Graduation Rate"))
q4_edu_map

##gdp map##
###########

#devide the mean_grad_rate dataset into categories
where_to_cut <- c(-Inf,0 , 10000, 25000, 40000, 55000, 70000, 85000, Inf)
label <- c("< 10000%","$10,000 to $25,000", "$25,000 to $40,0000", "$40,000 to $55,000", "$55,000 to $70,000", "$70,000 to $85,000", "$85,000 to $100,000","> $100,000")
mean_data_trimmed <- mutate(mean_data, change_labels = cut(mean_data$mean_gdp, breaks = where_to_cut, labels = label))

#join them
grad_map_df <- left_join(world_map, mean_data_trimmed, by = "iso3c")

#make the label in the reverse order
grad_map_df$change_labels <- factor(grad_map_df$change_labels, levels = rev(levels(grad_map_df$change_labels)))

#plot in a map
q4_gdp_map <- ggplot(data = grad_map_df, mapping = aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = change_labels)) +
  scale_fill_brewer(palette = "OrRd",direction = -1) +
  coord_quickmap() +
  labs(title = "Worldwide Average GDP") +
  theme_void()+
  guides(fill = guide_legend(title = "GDP in US Dollars"))
q4_gdp_map
