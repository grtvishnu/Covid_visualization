library(rjson)
library(data.table)

result <- fromJSON(file="https://api.covid19india.org/state_district_wise.json")

class(result)
result[["statewise"]][[1]][["active"]]
# 
# state<- result[["statewise"]]
# df<- do.call(rbind.data.frame, state)
# 
# time_series <- result[["cases_time_series"]]
# ts <-do.call(rbind.data.frame, time_series)
# 
# tested <- result[["tested"]]
# tested_df <- do.call(rbind.data.frame, tested)

kerala <- result[["Kerala"]][["districtData"]]
df <- ldply (kerala, data.frame)

df<- df %>% 
        select(dist=.id,3:6)

df <- as_tibble(df)
df$dist <- as.factor(df$dist)

df %>% 
        ggplot(aes(reorder(dist,active),active, fill=dist))+
        geom_bar(stat = "identity")+
        coord_flip()+
        theme_fivethirtyeight()+
        geom_text(aes(label=active), vjust=1.2,hjust=0, color="black", alpha=0.7,size=3.5)




library(tidyverse)
library(sf)
library(rayshader)

s.sf <- st_read("final_combined.shp")

kk=ggplot(s.sf)+
        geom_sf(aes(fill = Active))+
        scale_fill_gradient(low = "#f5af19", high = "#f12711")+
        ggtitle("Active Cases")

df<- df %>% 
        filter(active > 0)


s.sf$Active<- replace(s.sf$Active,values = df$active)
s.sf$Totalcase<- replace(s.sf$Totalcase,values = df$confirmed)
s.sf$Deaths<- replace(s.sf$Deaths,values = df$deceased)
s.sf$Recovered<- replace(s.sf$Recovered,values = df$recovered)





