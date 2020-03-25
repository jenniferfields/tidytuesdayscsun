#I am the Lorax and I speak for the trees!
#TidyTuesday 1.28.2020
#Jenn Fields
#Tidydors

rm(list=ls()) #Clears the environment


#load libs
library(tidyverse)
library(devtools)
devtools::install_github("jakelawlor/PNWColors") 
library(PNWColors)
library(vegan) #spp richness
library(lubridate) #edit dates
library(ggpubr)
library(png)
library(ggimage)
library(cowplot)

# Get the Data

sf_trees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-28/sf_trees.csv')
sf_trees$date <- ymd(sf_trees$date, quiet = FALSE, tz = NULL) #convert date to date format
sf_trees$date<-floor_date(sf_trees$date, years(10)) #short into decades

sf_trees<- sf_trees %>%
  mutate_at(vars(date), funs(year, month, day)) %>% #separates out date to year month and day columns
  rename(Decade = year) %>% #renames year col to Decade
  mutate_if(is.character,as.factor) %>% #convert to factor
  drop_na() %>% #drop rows with nas
  filter(species != "Tree(s) ::") %>% #get rid of unknown trees
  separate(col = species, into = c("ScientificName", "CommonName"), sep = "::") %>%
  filter(ScientificName!= "") %>%
  filter(dbh > 0) %>%
  group_by(ScientificName,Decade) %>%
  summarise(Maxdbh=max(dbh)) %>%
  top_n(1) %>%
  arrange(desc(Decade)) %>%
  filter(Decade != 2020)



#change to lollipop(any way you can make lollipop truffulas? ah)


# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 0
to_add <- data.frame( matrix(NA, empty_bar*nlevels(sf_trees$Decade), ncol(sf_trees)) )
colnames(to_add) <- colnames(sf_trees)
to_add$group <- rep(levels(sf_trees$Decade), each=empty_bar)
sf_trees <- rbind(sf_trees, to_add)
sf_trees <- sf_trees %>% dplyr::arrange(Decade)
sf_trees$id <- seq(1, nrow(sf_trees))

# Get the name and the y position of each label
label_data <- sf_trees
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)


#Adding Lorax
Lorax <- readPNG("TidyTuesday1.28/Lorax.png") #read png
Lorax<- grid::rasterGrob(Lorax,width = 0.2, height = 0.3)

# Make the plot
#pnw colors
sf_trees$Decade<-as.factor((sf_trees$Decade))
colors<-pnw_palette(name="Lake",7)
Treeplot <- ggplot(sf_trees, aes(x=as.factor(id), y=richness, fill=Decade)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(stat="identity", alpha=0.5) +
  scale_fill_manual(values = colors) +
  ylim(-300,270) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-0.5,4), "cm")) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=richness, label=richness, hjust=hjust), color="black", fontface="bold",
           size=5, angle= label_data$angle, inherit.aes = FALSE ) 
Treeplot

TreeandLorax<- ggdraw() +
  draw_plot(Treeplot) +
  draw_plot(Lorax, x = -.07, y = 0.1) +
  annotate(geom="text", x=.27, y=0.05, label="Species Richness of San Francisco Trees Over Time",
         color="black",fontface = "bold") +
  #ggsave....

