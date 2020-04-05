cov <- read_csv("cov.csv")


tail(cov)


cov_1 <- cov %>% 
  select(Country_Region, Last_Update, Confirmed, Deaths)
plot(cov_1)


table(is.na(cov_1))

us_cov<- cov_1 %>% 
  select(Confirmed,Country_Region, Deaths) %>% 
  filter(Country_Region == "US")

sum(us_cov$Confirmed)
sum(us_cov$Deaths)

df <- data.frame("US", "2020-04-04 23:34:21", 308850, 8407 )
cov_1 <- rbind(cov_1, df)
names(df) <- c("Country_Region", "Last_Update", "Confirmed", "Deaths")


cov_1<- cov_1 %>% 
  filter(Country_Region !='US')


cov_1 <- cov_1 %>% 
  arrange(desc(Confirmed))

cov_vis<- head(cov_1,10)

df2 <- tidyr::pivot_longer(cov_vis, cols=c('Confirmed', 'Deaths'), names_to='variable', 
                           values_to="value")

ggplot(df2, aes(x=reorder(Country_Region, -value), y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge')

