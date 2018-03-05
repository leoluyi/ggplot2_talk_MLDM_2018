library(wbstats)
library(data.table)
library(googleVis)
library(magrittr)

# Indicator    	  | Key
# ----------------|--------------
# fertility rate  | SP.DYN.TFRT.IN
# life expectancy |	SP.DYN.LE00.IN
# population	    | SP.POP.TOTL
# GDP per capita (current US$) |	NY.GDP.PCAP.CD

dt <- wb(indicator = c("SP.POP.TOTL","SP.DYN.LE00.IN", "SP.DYN.TFRT.IN", "NY.GDP.PCAP.CD"), 
         mrv = 60) %>% 
  data.table

dt_countries <- wbcountries() %>% data.table
dt_merge <- dt_countries[dt, .(country, region, date, value, indicator), on = .(iso2c)][
  ! region %in% "Aggregates"]

# long to wide by `indicator`
d <- dt_merge %>% 
  dcast(date + country + region ~ indicator, value.var = "value")
d[, `:=`(date = as.integer(date))]

d %>% setnames(old = c("date",
                       "country",
                       "region",
                       "Fertility rate, total (births per woman)",
                       "GDP per capita (current US$)",
                       "Life expectancy at birth, total (years)",
                       "Population, total"),
               new = c("Year", 
                       "Country",
                       "Region",
                       "Fertility", 
                       "GDP",
                       "LifeExpectancy",
                       "Population"))
d %>% fwrite("data/wb.csv")

p <- gvisMotionChart(d, 
                     idvar = "Country",
                     timevar = "Year",
                     xvar = "LifeExpectancy",
                     yvar = "Fertility",
                     sizevar = "Population",
                     colorvar = "Region")

# Ensure Flash player is available an enabled
plot(p)
