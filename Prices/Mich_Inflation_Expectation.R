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

# set FRED API key
API = Sys.getenv("API_key")
fredr_set_key(API)

#University of Michigan: Inflation Expectation
MICH<-fredr("MICH", observation_start = as.Date("1990-01-01"))
MICH_dt<-as.xts.data.table(as.data.table(MICH[,c(1,3)]))

avg<-mean(MICH_dt$value) %>% round(digits=2)

end_date = Sys.Date() + 1095

mich_dy<-dygraph(MICH_dt, xlab= "Date", ylab = "Percent") %>%
  dySeries(label = "Inflation Expectation") %>%
  dyOptions(fillAlpha= 0.8, strokeWidth = 3 ,colors = "#003366") %>%
  dyLimit(limit = as.numeric(avg), color= "black", label = as.numeric(avg), labelLoc = "right", strokePattern = "solid") %>%
  dyLegend(show= "always",  width  = 150, labelsSeparateLines = TRUE) %>%
  dyShading(from= "1990-07-01", to=" 1991-03-01", color = "#cecece")%>%
  dyShading(from= "2001-03-01", to="2001-11-01", color = "#cecece") %>%
  dyShading(from = "2007-12-01", to="2009-06-01", color = "#cecece") %>%
  dyShading(from = "2020-02-01", to= "2020-04-01" ,color = "#cecece") %>%
  dyRangeSelector(dateWindow = c(as.Date("1990-01-01"), as.Date(end_date))) %>%
  dyAxis("y", valueRange = c(0,7))
mich_dy

saveWidget(mich_dy, "mich_dy.html")


