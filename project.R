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
colnames(df)<- c("iso3c", "Country","Year","Education","Economy")
####################
###Summarize Data###
####################


#extract all the countries (name only appear once)
country_names <- distinct(edu, Country_code, Country)
colnames(country_names)<- c("iso3c", "Country")

#mean dataframe that has the mean graduation rate and mean gdp rate for
#each countries.
#the graduation rate and gdp was divided by the total number of years recorded in dataset
mean_data <- group_by(df,iso3c) %>% 
  summarise(mean_grad_rate = mean(Education), mean_gdp = mean(Economy)) %>% 
  left_join(country_names, by = "iso3c")



#the world mean is the mean grad rate and mean gdp rate of all countries from 2005 to 2017
world_mean <- group_by(df, Year) %>% 
  summarise(mean_grad_rate = mean(Education),mean_gdp = mean(Economy)) 



#Question 3 
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
  filter(iso3c == "USA")
#removed the outlier of data from 2005
usa <- usa[-c(1),]
#select only the year, grad_Rate, gdp and events
usa <- mutate(usa, Events = events) %>% 
  select(Year, Education, Economy, Events)

#Question 4
#get world map data
world_map <- map_data("world")
iso <- iso.alpha(world_map$region, n = 3)
world_map <- mutate(world_map, iso3c = iso)



