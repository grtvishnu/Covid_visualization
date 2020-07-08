library(tidyverse)
library(scales)
library(lubridate)
library(gganimate)
library(gifski)
cov_nes <- read_csv("time-series-19-covid-combined.csv")


cov_nes <- cov_nes %>% 
  select(Date,country=`Country/Region`, Confirmed, Recovered,Deaths)
rm(cov_latest_all)


cov_stat<- cov_nes %>% 
  group_by(country) %>% 
  summarise(Death=max(Deaths),Confirmed =max(Confirmed), Recovered=max(Recovered)) %>% 
  mutate(Active_case = Confirmed- Recovered)


cov_nes %>% 
  group_by(country) %>% 
  summarise(Death=max(Deaths),Confirmed =max(Confirmed), Recovered=max(Recovered)) %>% 
  arrange(desc(Death)) %>% 
  top_n(10) %>% 
  ggplot(aes(x=reorder(country,-Death), y= Death, fill=country))+
  geom_bar(stat = "identity")+
  scale_y_continuous(labels = comma)


cov_nes %>% 
  group_by(country) %>% 
  summarise(Death=max(Deaths),Confirmed =max(Confirmed), Recovered=max(Recovered)) %>% 
  arrange(desc(Confirmed)) %>% 
  top_n(10) %>% 
  ggplot(aes(x=reorder(country,-Confirmed), y= Confirmed, fill=country))+
  geom_bar(stat = "identity")+
  scale_y_continuous(labels = comma)

cov_nes %>% 
  group_by(country) %>% 
  summarise(Death=max(Deaths),Confirmed =max(Confirmed), Recovered=max(Recovered)) %>% 
  arrange(desc(Recovered)) %>% 
  top_n(10) %>% 
  ggplot(aes(x=reorder(country,-Recovered), y= Recovered, fill=country))+
  geom_bar(stat = "identity")+
  scale_y_continuous(labels = comma)

cov_nes %>% 
  group_by(country) %>% 
  summarise(Death=max(Deaths),Confirmed =max(Confirmed), Recovered=max(Recovered)) %>% 
  mutate(Active_case = Confirmed - Recovered) %>% 
  top_n(10) %>% 
  ggplot(aes(x=reorder(country,Active_case), y= Active_case, fill=country))+
  geom_bar(stat = "identity")+
  coord_flip()+
  scale_y_continuous(labels = comma)

cov_nes<- cov_nes %>% 
mutate(year = lubridate::year(Date), 
       month = lubridate::month(Date), 
       day = lubridate::day(Date))


ani1<- cov_nes %>% 
  group_by(Date,country) %>% 
  summarise(Death=max(Deaths),Confirmed =max(Confirmed), Recovered=max(Recovered)) %>% 
  arrange(desc(Death)) %>%
  filter(country=="India"|
         country=="US"|
         country=="Brazil"|
         country=="Russia") %>% 
  ggplot(aes(Date,Death, color =country))+
  ggtitle("Total Death")+
  geom_line()+
  scale_y_continuous(labels = comma)+
  geom_point(size =1.5)+
  transition_reveal(Death)
animate(ani1, height=600, width=800, fps = 30,duration = 10, end_pause = 60, res=100)  


ani1<- cov_nes %>% 
  group_by(Date,country) %>% 
  summarise(Death=sum(Deaths),Confirmed =max(Confirmed), Recovered=max(Recovered)) %>% 
  arrange(desc(Death)) %>%
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
animate(ani1, height=600, width=800, fps = 30,duration = 10, end_pause = 60, res=100)  


ani1<- cov_nes %>% 
  group_by(Date,country) %>% 
  summarise(Death=max(Deaths),Confirmed =max(Confirmed), Recovered=max(Recovered)) %>% 
  arrange(desc(Recovered)) %>%
  filter(country=="India"|
           country=="US"|
           country=="Brazil"|
           country=="Russia") %>% 
  ggplot(aes(Date,Recovered, color =country))+
  ggtitle("Total Recovered Cases")+
  geom_line()+
  scale_y_continuous(labels = comma)+
  geom_point(size =1.5)+
  transition_reveal(Confirmed)
animate(ani1, height=600, width=800, fps = 30,duration = 10, end_pause = 60, res=100)  



p<- cov_nes %>% 
  group_by(Date,country) %>% 
  summarise(Death=sum(Deaths),Confirmed =sum(Confirmed), Recovered=sum(Recovered)) %>% 
  arrange(desc(Death)) %>% 
  filter(country=="India"|
           country=="US"|
           country=="Brazil"|
           country=="Russia") %>% 
  ggplot(aes(x=reorder(country,-Death), y= Death, fill=country))+
  geom_bar(stat = "identity")+
  scale_y_continuous(labels = comma)+
  transition_time(Date)
animate(p, height=600, width=800, fps = 30,duration = 10, end_pause = 60, res=100)  
