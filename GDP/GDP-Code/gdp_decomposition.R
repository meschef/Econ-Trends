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

#consumption
PCEC<-fredr("PCEC", observation_start = as.Date("1990-01-01"))
consumption<-as.xts.data.table(as.data.table(PCEC[,c(1,3)]))
colnames(consumption)<- "Consumption"

#investment
GPDIC1<-fredr("GPDIC1", observation_start = as.Date("1990-01-01"))
investment<-as.xts.data.table(as.data.table(GPDIC1[,c(1,3)]))
colnames(investment)<-"Investment"

#government
W068RCQ027SBEA<-fredr("W068RCQ027SBEA", observation_start = as.Date("1990-01-01"))
government<-as.xts.data.table(as.data.table(W068RCQ027SBEA[,c(1,3)]))
colnames(government)<- "Government"

#exports
EXPGS<-fredr("EXPGS", observation_start = as.Date("1990-01-01"))
exports<-as.xts.data.table(as.data.table(EXPGS[,c(1,3)]))
colnames(exports)<- "Exports"

#imports
IMPGSC1<-fredr("IMPGSC1", observation_start = as.Date("1990-01-01"))
imports<-(as.data.table(IMPGSC1[,c(1,3)]))
imports$value=imports$value*(-1)
imports<- as.xts.data.table(imports)
colnames(imports)<- c("Imports")


aggregate<-cbind(consumption, investment, government, imports, exports)


gdp_decomposition<-dygraph(aggregate) %>%
  dyStackedBarChart() %>%
  dyOptions(colors = c("#003f5c", "#58508d", "#bc5090", "#ff6361", "#ffa600"), fillAlpha= 0.8) %>%
  dyShading(from= "1990-07-01", to=" 1991-03-01", color = "#cecece")%>%
  dyShading(from= "2001-03-01", to="2001-11-01", color = "#cecece") %>%
  dyShading(from = "2007-12-01", to="2009-06-01", color = "#cecece") %>%
  dyShading(from = "2020-02-01", to= "2020-04-01" ,color = "#cecece") %>%
  dyLegend(show= "onmouseover", width = 900) %>%
  dyRangeSelector(dateWindow = c(as.Date("1990-01-01"), as.Date("2025-01-01"))) %>%
  dyAxis("y", valueRange = c(-2000, 30000))
gdp_decomposition

saveWidget(gdp_decomposition, "gdp_decomposition_dy.html")

