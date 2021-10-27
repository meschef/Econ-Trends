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

previous_period_pchange<-fredr("A191RL1Q225SBEA", observation_start = as.Date("1990-01-01"))
GDP_change<-as.data.table(previous_period_pchange[,c(1,3)])
negative<-GDP_change %>% filter(value <0)
positive<- GDP_change%>% filter(value>=0)
as.data.table(negative)
as.data.table(positive)
positive_data<- as.xts.data.table(positive)
negative_data<- as.xts.data.table(negative)


avg<-mean(GDP_change$value)%>% round(digits=2)

final<-merge(positive_data, negative_data)


GDP_change_dynamic<-dygraph(final, xlab= "Date", ylab = "Annualized Rate of Change from Previous Quarter") %>%
  dySeries("value", color= "#003366") %>% 
  dySeries("value.1", color = "#B22234") %>%
  dyLimit(limit = as.numeric(avg), color= "black", label =as.numeric(avg), labelLoc = "right", strokePattern = "solid") %>%
  dyOptions(fillAlpha= 0.8) %>%
  dyLegend(show= "onmouseover", width = 150) %>%
  dyShading(from= "1990-07-01", to=" 1991-03-01", color = "#cecece")%>%
  dyShading(from= "2001-03-01", to="2001-11-01", color = "#cecece") %>%
  dyShading(from = "2007-12-01", to="2009-06-01", color = "#cecece") %>%
  dyShading(from = "2020-02-01", to= "2020-04-01" ,color = "#cecece") %>%
  dyRangeSelector(dateWindow = c(as.Date("1990-01-01"), as.Date("2024-01-01"))) %>%
  dyBarChart()
GDP_change_dynamic
saveWidget(GDP_change_dynamic, "gdp_growth_dy.html")

