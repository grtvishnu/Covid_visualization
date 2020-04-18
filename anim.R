library(tidyverse)
library(lubridate)
library(gganimate)

data <- read.csv("latestdata.csv") 
head(data$date_confirmation)

datanew <- data %>% 
        mutate(date_confirmation = dmy(date_confirmation))
head(datanew$date_confirmation)


datanew %>% 
        group_by(date_confirmation) %>% 
        summarise(count = n()) %>% 
        mutate(cuml = cumsum(count)) %>% 
        ggplot(aes(x=date_confirmation, y =cuml))+
        geom_line(color = 'red')+
        geom_point(size =1.5)+
        geom_area(fill ='red')+
        theme_bw()+
        ggtitle("Daily Cumulative Cases")+
        transition_reveal(cuml)
anim_save("Total case")

datanew$month <- as.integer(datanew$month)

datanew$day <- day(datanew$date_confirmation)
datanew$month <- month(datanew$date_confirmation)

new <- datanew %>% 
        filter(month== 3) %>%
        group_by(day, country) %>% 
        summarise(count =n())
new <- data.frame(complete(new, day, country,
                           fill = list(count =0)))

new %>% filter(country == "United States" | country == "France" | country == "United Kingdom" |
                       country == "Germany") %>% 
        ggplot(aes(x =day, y= count,
                   group = country,
                   color = country))+
        geom_line()+
        geom_point()+
        theme_bw()+
        ggtitle("Animated Daily Plot") +
        transition_reveal(day)
anim_save("dailyplot.gif")
