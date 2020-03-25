#Tidydors 3.24.2020

rm(list=ls())
#load libs
library(tidyverse)
library(viridis)
library(hrbrthemes)
library(PNWColors)
library(calecopal)
# Get the Data

tbi_age <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_age.csv')
tbi_year <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_year.csv')
tbi_military <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_military.csv')

tbi_age$age_group<-as.factor(tbi_age$age_group)
AG <- factor(c("0-4","15-24", "25-34", "35-44","45-54", "5-14","55-64", "65-74", "75+", "Total"), levels = c("0-4","5-14","15-24", "25-34", "35-44","45-54", "55-64", "65-74", "75+", "Total"))

colors<-pnw_palette(name="Starfish",n=7,type="discrete")
BrainDeaths<-tbi_age %>%
  filter(age_group != "0-17") %>%
  filter(type == "Deaths") %>%
  mutate(AG = fct_relevel(age_group,"0-4","5-14","15-24", "25-34", "35-44","45-54", "55-64", "65-74", "75+", "Total")) %>% #reorders factors  in graph
  ggplot(aes(fill=injury_mechanism, y=rate_est, x=AG)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values = colors) +
  labs(fill="Injury Mechanism", x ="Age Groups", y = "Rate out of 100,000") +
  theme_ipsum() +
  theme(legend.text = element_text(size=24),
        legend.title = element_text(size = 24),
        legend.spacing = unit(1, unit = "cm")) +
  theme(axis.text.x=element_text(color="black", size=24), 
        axis.text.y=element_text(color="black", size=24), 
        axis.title.x = element_text(color="black", size=24, face="bold"), 
        axis.title.y = element_text(color="black", size=24, face="bold"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  ggtitle('Brain Deaths by Mechanism and Age Groups') +
  ggsave("March242020/BrainDeathsGraph.png", width=55, height=45, unit="cm")
BrainDeaths

