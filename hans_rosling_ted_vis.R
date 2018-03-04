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

dt <- wb(indicator = c("SP.POP.TOTL","SP.DYN.LE00.IN", "SP.DYN.TFRT.IN"), 
         mrv = 60) %>% 
  data.table

dt_countries <- wbcountries() %>% data.table
dt_merge <- dt[dt_countries, .(country, region, date, value, indicator), on = .(iso2c)][
  ! region %in% "Aggregates"]

# long to wide by `indicator`
d <- dt_merge %>% 
  dcast(date + country + region ~ indicator, value.var = "value")

d[, `:=`(date = as.integer(date), "NA" = NULL)]

d %>% setnames(old = c("date",
                       "country",
                       "region",
                       "Fertility rate, total (births per woman)",
                       "Life expectancy at birth, total (years)",
                       "Population, total"),
               new = c("Year", 
                       "Country",
                       "Region",
                       "Fertility", 
                       "LifeExpectancy",
                       "Population"))

p <- gvisMotionChart(d, 
                     idvar = "Country",
                     timevar = "Year",
                     xvar = "LifeExpectancy",
                     yvar = "Fertility",
                     sizevar = "Population",
                     colorvar = "Region")

# Ensure Flash player is available an enabled
plot(p)
