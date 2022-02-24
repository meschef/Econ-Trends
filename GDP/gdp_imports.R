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


A021RY2Q224SBEA<-fredr("A021RY2Q224SBEA", observation_start = as.Date("1990-01-01"))
import_dt<-as.data.table(A021RY2Q224SBEA[,c(1,3)])
negative<- import_dt %>%
  filter(value <0) 
positive<- import_dt %>%
  filter(value>=0)
as.data.table(negative)
as.data.table(positive)
positive_data<- as.xts.data.table(positive)
negative_data<- as.xts.data.table(negative)
import_data<-merge(positive_data, negative_data)
colnames(import_data)<- c("positive", "negative")

avg<-mean(import_dt$value) %>% round(digits=2)

end_date = Sys.Date() + 1095

gdp_imports<-dygraph(import_data,  xlab= "Date", ylab = "Percentage Points of GDP at Annual Rate") %>%
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
  dyAxis("y", valueRange=c(-10,14))%>%
  dyBarChart()
gdp_imports
saveWidget(gdp_imports, "gdp_imports_dy.html")



