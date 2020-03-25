#Jenn Fields
#09032019

library(tidyverse)
library(hrbrthemes)
library(viridis)
cpu <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-03/cpu.csv")

gpu <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-03/gpu.csv")

ram <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-03/ram.csv")

View(cpu)
View(gpu)
View(ram)

RamData<- ram %>%
  select(manufacturer_s, transistor_count, date_of_introduction)
RamData
#drop_na drops nas

CpuData1<- cpu %>%
  select(designer, transistor_count, date_of_introduction)
CpuData1
CpuData1$manufacturer_s<-CpuData1$designer
CpuData<-CpuData1[-1]
View(CpuData)

GpuData<- gpu %>%
  select(manufacturer_s, transistor_count, date_of_introduction)
GpuData

RGdata<-left_join(RamData,GpuData)

RGCdata<-left_join(RGdata,CpuData)
View(RGCdata)

NumManufacturer<- RGCdata %>%
  group_by(manufacturer_s) %>%
  tally()



ggplot(RGCdata) +
  geom_point(aes(x= date_of_introduction, y = transistor_count)) +
  #scale_y_continuous(trans = 'exponential') +
  xlab('Year') +
  ylab('transistor')

ggplot() +
  ggplot(RGCdata, aes(x=date_of_introduction, y=transistor_count) + 
  geom_point(size=6) +
  theme_ipsum()
