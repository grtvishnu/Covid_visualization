# Load libraries
# install.packages("tidyverse")
library(tidyverse)
library(scales)
library(lubridate)
library(gganimate)
library(gifski)
covid <- read_csv("time-series-19-covid-combined.csv")

# Remove unnecessary Columns and Rename
covid <- covid %>% 
  select(Date,country=`Country/Region`, Confirmed, Recovered,Deaths)

# Create a useful data frame
covid_stat<- covid %>% 
  group_by(country) %>% 
  summarise(Death=max(Deaths),Confirmed =max(Confirmed), Recovered=max(Recovered)) %>% 
  mutate(Active_case = Confirmed - Recovered)

#top 10 Confirmed
covid_stat %>% 
  arrange(desc(Confirmed)) %>% 
  top_n(10) %>% 
  ggplot(aes(x=reorder(country,-Confirmed), y= Confirmed, fill=country))+
  geom_bar(stat = "identity")+
  scale_y_continuous(labels = comma)+
  ggtitle("Top 10 Total Confirmed")

# Top 10 Deaths
covid_stat %>% 
  arrange(desc(Death)) %>% 
  top_n(10) %>% 
  ggplot(aes(x=reorder(country,-Death), y= Death, fill=country))+
  geom_bar(stat = "identity")+
  scale_y_continuous(labels = comma)+
  ggtitle("Top 10 Total Deaths")

# Top 10 Recovered
covid_stat %>% 
  arrange(desc(Recovered)) %>% 
  top_n(10) %>% 
  ggplot(aes(x=reorder(country,-Recovered), y= Recovered, fill=country))+
  geom_bar(stat = "identity")+
  scale_y_continuous(labels = comma)+
  ggtitle("Top 10 Total Recovered")

# Top 10 Active Cases
covid_stat %>% 
  top_n(10) %>% 
  ggplot(aes(x=reorder(country,Active_case), y= Active_case, fill=country))+
  geom_bar(stat = "identity")+
  coord_flip()+
  scale_y_continuous(labels = comma)+
  ggtitle("Top 10 Total Active Case")

# animated graph Total death India (Line Graph)
p1<- covid %>% 
  group_by(country,Date) %>% 
  summarise(Death=max(Deaths),Confirmed =max(Confirmed), Recovered=max(Recovered)) %>% 
  arrange(desc(Death)) %>%
  filter(country=="India") %>% 
  ggplot(aes(Date,Death))+
  ggtitle("Total Death In India")+
  geom_line(color ='blue')+
  scale_y_continuous(labels = comma)+
  geom_point(size =1.5)+
  transition_reveal(Death)
animate(p1, height=600, width=800, fps = 30,duration = 10, end_pause = 60, res=100)
anim_save("india.gif")

#Animated Graph Multiple Countries (Line Graph)
p2<- covid %>% 
  group_by(country,Date) %>% 
  summarise(Death=max(Deaths),Confirmed =max(Confirmed), Recovered=max(Recovered)) %>% 
  arrange(desc(Confirmed)) %>%
  filter(country=="India"|
           country=="US"|
           country=="Brazil"|
           country=="Russia") %>% 
  ggplot(aes(Date,Confirmed, color =country))+
  ggtitle("Confirmed Cases")+
  geom_line()+
  scale_y_continuous(labels = comma)+
  geom_point(size =1.5)+
  transition_reveal(Confirmed)
animate(p2, height=600, width=800, fps = 30,duration = 10, end_pause = 60, res=100)  
anim_save("all confirmed.gif ")

# Animated Graph total death multiple Countries (Bar plot)
p3<- covid %>% 
  group_by(country,Date) %>% 
  summarise(Death=max(Deaths),Confirmed =max(Confirmed), Recovered=max(Recovered)) %>% 
  arrange(desc(Death)) %>% 
  filter(country=="India"|
           country=="US"|
           country=="Brazil"|
           country=="Russia") %>% 
  ggplot(aes(x=reorder(country,-Death), y= Death, fill=country))+
  geom_bar(stat = "identity")+
  ggtitle("Total Death")+
  scale_y_continuous(labels = comma)+
  transition_time(Date)
animate(p3, height=600, width=800, fps = 30,duration = 10, end_pause = 60, res=100)  
anim_save("all dead.gif ")
