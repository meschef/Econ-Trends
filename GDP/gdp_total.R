# set FRED API key
API = Sys.getenv("API_key")
fredr_set_key(API)
# load required packages
library(data.table)
library(dygraphs)
library(dplyr)
library(readxl)
library(fredr)
library(ggplot2)
library(stringr)
library(tidyr)
library(widgetframe)
library(xts)

gdp_total_data<-fredr("GDPC1", observation_start = as.Date("1990-01-01"))
gdp_total_dt<-as.data.table(gdp_total_data[,c(1,3)])
gdp_total_dt<- as.xts.data.table(gdp_total_dt)

end_date = Sys.Date() + 1095

gdp_total<-dygraph(gdp_total_dt, xlab= "Date", ylab = "Billions of Chained 2012 Dollars") %>%
  dySeries("value", color= "#003366") %>% 
  dyOptions(fillAlpha= 0.8) %>%
  dyLegend(show= "onmouseover", width = 150) %>%
  dyShading(from= "1990-07-01", to=" 1991-03-01", color = "#cecece")%>%
  dyShading(from= "2001-03-01", to="2001-11-01", color = "#cecece") %>%
  dyShading(from = "2007-12-01", to="2009-06-01", color = "#cecece") %>%
  dyShading(from = "2020-02-01", to= "2020-04-01" ,color = "#cecece") %>%
  dyRangeSelector(dateWindow = c(as.Date("1990-01-01"), as.Date(end_date))) %>%
  dyBarChart()
gdp_total
saveWidget(gdp_total, "gdp_total_dy.html")


