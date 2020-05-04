install.packages("covid19.analytics")
library(covid19.analytics)

cov <- covid19.data(case = "aggregated")
tscov <- covid19.data(case = "ts-confirmed")
tscall <- covid19.data(case = "ts-ALL")

report.summary(Nentries = 10,
               graphical.output = T)
tots.per.location(tscov, geo.loc = c("US", "India"))

growth.rate(tscov, geo.loc = "US")

totals.plt(tscall, c("India"))

live.map(tscall)
