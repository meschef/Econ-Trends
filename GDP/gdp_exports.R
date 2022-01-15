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


A020RY2Q224SBEA<-fredr("A020RY2Q224SBEA", observation_start = as.Date("1990-01-01"))
export_dt<-as.data.table(A020RY2Q224SBEA[,c(1,3)])
negative<- export_dt %>%
  filter(value <0) 
positive<- export_dt %>%
  filter(value>=0)
as.data.table(negative)
as.data.table(positive)
positive_data<- as.xts.data.table(positive)
negative_data<- as.xts.data.table(negative)
export_data<-merge(positive_data, negative_data)
colnames(export_data)<- c("positive", "negative")

avg<-mean(export_dt$value) %>% round(digits=2)

end_date = Sys.Date() + 1095

gdp_exports<-dygraph(export_data, xlab= "Date", ylab = "Percentage Points of GDP po at Annual Rate") %>%
  dySeries("positive", color= "#003366") %>% 
  dySeries("negative", color = "#B22234") %>%
  dyLimit(limit = as.numeric(avg), color= "black", label = as.numeric(avg), labelLoc = "right", strokePattern = "solid") %>%
  dyOptions(fillAlpha= 0.8) %>%
  dyLegend(show= "always",  width  = 150, labelsSeparateLines = TRUE) %>%
  dyShading(from= "1990-07-01", to=" 1991-03-01", color = "#cecece")%>%
  dyShading(from= "2001-03-01", to="2001-11-01", color = "#cecece") %>%
  dyShading(from = "2007-12-01", to="2009-06-01", color = "#cecece") %>%
  dyShading(from = "2020-02-01", to= "2020-04-01" ,color = "#cecece") %>%
  dyRangeSelector(dateWindow = c(as.Date("1990-01-01"), as.Date(end_date))) %>%
  dyBarChart()
gdp_exports
saveWidget(gdp_exports, "gdp_exports_dy.html")





