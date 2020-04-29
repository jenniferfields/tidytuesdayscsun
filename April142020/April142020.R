#TidyTuesday
#April 28 2020
#Give my regards to BROADDDDWAYYYYYYYY

#clear
rm(list=ls())
#load libs
library(tidyverse)
library(devtools)
devtools::install_github("jakelawlor/PNWColors") 
library(PNWColors)
library(vegan) #spp richness
library(lubridate) #edit dates
library(ggpubr)
library(ggsidekick)
library(png)
library(ggimage)
library(cowplot)
devtools::install_github("dill/emoGG")
library(emoGG)
devtools::install_github("seananderson/ggsidekick")
library(ggsidekick)
emoji_search("microphone")

# Get the Data
grosses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/grosses.csv', guess_max = 40000)
synopses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/synopses.csv')
cpi <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/cpi.csv')
pre_1985_starts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/pre-1985-starts.csv')


grosses$week_ending <- ymd(grosses$week_ending, quiet = FALSE, tz = NULL) #convert date to date format
grosses$decade<-floor_date(grosses$week_ending, years(10)) #sort into decades


colors<-pnw_palette(name="Lake",6)
TopGrossingShows <-grosses %>%
  mutate_at(vars(decade), funs(year, month, day)) %>% #separates out date to year month and day columns
  rename(Decade = year) %>% #renames year col to Decade
  drop_na() %>% #drop rows with nas
  group_by(show,Decade) %>%
  summarise(Maxweekly=max(weekly_gross)) %>% 
  arrange(desc(Decade)) %>%
  group_by(Decade) %>%
  top_n(3) # top 3 weekly grossing shows
TopGrossingShows<-as.data.frame(TopGrossingShows)

ggplot(TopGrossingShows,aes(x = Decade, y=Maxweekly, fill = show)) +
  geom_emoji(emoji = "1f3a4") +
  labs(x="Decade", 
       y="Max Weekly Gross (USD dollars)", 
       fill="Broadway Show") + #labels the x and y axes
  scale_size(range = c(.1, 15), name="Max Weekly Gross (USD dollars)") +
  labs(title="Top three Broadway shows with highest weekly box office by decade",
          subtitle = "by Jenn Fields of the Tidydors",
          caption ="Data courtesy of Playbill & The Broadway League") +
  geom_text_repel(aes(label = rownames(df)),
            size = 3.5)
  theme_sleek() +
  theme(plot.title = element_text(size = 15, face = "bold"), #increase size and bold title
    plot.subtitle = element_text(),
    plot.caption = element_text(hjust = 0, size= 10,face = "italic"),
    axis.title.x = element_text(color="black", size=12, face="bold"), 
    axis.title.y = element_text(color="black", size=12, face="bold"))# italic caption
  





  
  
