library(shiny)
library(wbstats)
library(tidyr)
library(ggplot2)
library(maps)

source("project.R")
#Data introduction

#Through this project, we are attempting to draw relationships and comparisons between these two variables; economy rates of countries and higher rates of education in different countries.

###Some of the questions that will facilitate in drawing these comparisons between the two variables inlcude:
#Is there a relationship between economic status of a country and their graduation rates?
 #This question is aimed to help us finally analyze and answer if there is a relationship between the two factors; if there is a correlation between the two, and if so, the type of correlation (positive, negative).
#Rank the education rate from highest to lowest among countries worldwide, and also show their economic trend.
 #This question will help us understand which country has the highest rates of highest education and which has the lowest rates.
#How does the correlation of US higher education rates vs. economy look like with respect to US events?
 #This question will help us understand how these values compare to how well the country is faring.
#Which regions tend to have a higher GDP? Higher graduation rates? What could these results entail?
 #This will help us understand if there is a certain region where more affluent individuals use overseas bank accounts to store money, for example. These findings could prompt many more questions.
#We chose this topic, as we believe that education is a very essential tool to a person's overall success in life irrespective of age. Just like every human being requires oxygen to survive, education too is very essential to survive in this world. Education provides people the knowledge and skills they need in order to survive. By analyzing these questions relating to education and how the economy rates of countries affect education - we hope to be able to better understand which countries are doing well in terms of economy and higher rates of education, and which countries are not doing well in these aspects. By understanding these aspects, we as students can work on spreading awareness regarding countries that are low on higher education rates and work in small ways that impact those countries and improve the overall higher educational rates. Thus, we believe that the topic chosen is important.

#Data Description
#Our data is from the World Bank and the Organisation for Economic Co-Operation and Development. Our World Bank data shows countries' GDP **per capita** and the OECD data shows education rates for different countries. OECD has columns of education rates for upper-secondary education, post-secondary education, short-cycle tertiary education, as well as college degrees such as a bachelor's or equivalent. We are focusing on people who obtain bachelor's degrees the first time they enter university.
#The data was collected and distributed by the respective organizations.
#**World Bank Data: https://data.worldbank.org/indicator/NY.GDP.PCAP.CD?end=2018&start=2018&view=bar**
#**OECD Data: https://stats.oecd.org/Index.aspx?datasetcode=EAG_GRAD_ENTR_RATES**
  