library(tidyverse)
library(scales)
cov_latest_all <- read_csv("time_series_19_covid_combined.csv")


cov_nes <- cov_latest_all %>% 
  select(Date,country=`Country/Region`, Confirmed, Recovered,Deaths)



cov_in <- cov_nes %>% 
  filter(country=="India")


cov_nes %>% 
  group_by(country) %>% 
  count(Deaths)




cov_nes$country <- as.factor(cov_nes$country)

table(cov_nes$country)

 cov_nes %>%
  group_by(country) %>%
  summarize(Deaths = count(Deaths))



cov_stat<- cov_nes %>% 
  group_by(country) %>% 
  summarise(Death=sum(Deaths),Confirmed =sum(Confirmed), Recovered=sum(Recovered)) %>% 
  mutate(Active_case = Confirmed- Recovered)


cov_nes %>% 
  group_by(country) %>% 
  summarise(Death=sum(Deaths),Confirmed =sum(Confirmed), Recovered=sum(Recovered)) %>% 
  arrange(desc(Death)) %>% 
  top_n(10) %>% 
  ggplot(aes(x=reorder(country,-Death), y= Death, fill=country))+
  geom_bar(stat = "identity")+
  scale_y_continuous(labels = comma)


cov_nes %>% 
  group_by(country) %>% 
  summarise(Death=sum(Deaths),Confirmed =sum(Confirmed), Recovered=sum(Recovered)) %>% 
  arrange(desc(Confirmed)) %>% 
  top_n(10) %>% 
  ggplot(aes(x=reorder(country,-Confirmed), y= Confirmed, fill=country))+
  geom_bar(stat = "identity")+
  scale_y_continuous(labels = comma)

cov_nes %>% 
  group_by(country) %>% 
  summarise(Death=sum(Deaths),Confirmed =sum(Confirmed), Recovered=sum(Recovered)) %>% 
  arrange(desc(Recovered)) %>% 
  top_n(10) %>% 
  ggplot(aes(x=reorder(country,-Recovered), y= Recovered, fill=country))+
  geom_bar(stat = "identity")+
  scale_y_continuous(labels = comma)

cov_nes %>% 
  group_by(country) %>% 
  summarise(Death=sum(Deaths),Confirmed =sum(Confirmed), Recovered=sum(Recovered)) %>% 
  mutate(Active_case = Confirmed - Recovered) %>% 
  top_n(10) %>% 
  ggplot(aes(x=reorder(country,Active_case), y= Active_case, fill=country))+
  geom_bar(stat = "identity")+
  coord_flip()+
  scale_y_continuous(labels = comma)







