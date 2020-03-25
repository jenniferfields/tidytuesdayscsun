#First tidytuesday!
#Jenn Fields
#8/27/2019

#load libraries
library(tidyverse)
library(wordcloud2) 


#to revert click on square and then settings button and click revert

#####READ DATA##########
simpsons <- readr::read_delim("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-27/simpsons-guests.csv", delim = "|", quote = "")
#uploaded data from rdatascience/tidytuesday github repo

simpsons$guest_star<-as.factor(simpsons$guest_star) #change guest star to factor

Sdata<- simpsons %>% #create new data frame for word cloud
  group_by(guest_star) %>% 
  tally()
View(Sdata)  
#group by guest star and tally by frequency in data and create new dataframe


# star wordcloud with freq of guest stars on Simpsons over all seasons
wordcloud2(data=Sdata, size = 2.5, shape = 'star')