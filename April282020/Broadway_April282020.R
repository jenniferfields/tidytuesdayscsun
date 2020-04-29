#TidyTuesday
#April 28 2020
#Give my regards to BROADDDDWAYYYYYYYY

#clear
rm(list=ls())
#load libs
library(tidyverse)
library(devtools)
library(lubridate) #edit dates
devtools::install_github("jakelawlor/PNWColors") 
library(PNWColors)
devtools::install_github("dill/emoGG")
library(emoGG) #for emojis!!!
devtools::install_github("seananderson/ggsidekick")
library(ggsidekick) #for theme sleek
library(ggrepel) # for text labels


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
  group_by(Decade) %>%
  top_n(3) # top 3 weekly grossing shows
TopGrossingShows<-as.data.frame(TopGrossingShows)

colors<-pnw_palette(name="Starfish",6)
ggplot(TopGrossingShows,aes(x = Decade, y=Maxweekly, label =show)) +
  geom_emoji(emoji = "1f3a4") +
  labs(x="Decade", 
       y="Max Weekly Gross (USD dollars)", 
       fill="Broadway Show") + #labels the x and y axes
  scale_size(range = c(.1, 15), name="Max Weekly Gross (US Dollars)") +
  labs(title="Top three Broadway shows with highest weekly box office by decade",
          subtitle = "by Jenn Fields of the Tidydors",
          caption ="Data courtesy of Playbill & The Broadway League") +
  geom_text_repel(aes(color = show),
                  size = 5,
                  force = 1,
                  box.padding = .75,
                  point.padding = 1,
                  fontface = "bold") +
  scale_colour_manual(values = colors) +
  theme_sleek() +
  theme(plot.title = element_text(size = 15, face = "bold"), #increase size and bold title
    plot.subtitle = element_text(),
    plot.caption = element_text(hjust = 0, size= 10,face = "italic"),# italic caption
    axis.title.x = element_text(color="black", size=14, face="bold"), 
    axis.title.y = element_text(color="black", size=14, face="bold"),
    axis.text.x=element_text(size=12), 
    axis.text.y=element_text(size =12)) +
  theme(legend.position = "none") +
  ggsave("April282020/TopWeeklyGrossbyDecade.png", width=25, height=22,dpi=300, unit="cm")
  





  
  
