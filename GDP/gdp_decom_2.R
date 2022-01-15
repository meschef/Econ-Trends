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

#net exports
EXPGS<-fredr("NETEXP", observation_start = as.Date("1990-01-01"))
netexports<-as.xts.data.table(as.data.table(EXPGS[,c(1,3)]))
colnames(netexports)<- "Net Exports"


aggregate<-cbind(consumption, investment, government, netexports)

end_date = Sys.Date() + 1095

gdp_decomposition2<-dygraph(aggregate) %>%
  dyStackedBarChart() %>%
  dyOptions(colors = c("#003f5c", "#58508d", "#bc5090", "#ff6361", "#ffa600"), fillAlpha= 0.8) %>%
  dyShading(from= "1990-07-01", to=" 1991-03-01", color = "#cecece")%>%
  dyShading(from= "2001-03-01", to="2001-11-01", color = "#cecece") %>%
  dyShading(from = "2007-12-01", to="2009-06-01", color = "#cecece") %>%
  dyShading(from = "2020-02-01", to= "2020-04-01" ,color = "#cecece") %>%
  dyLegend(show= "onmouseover", width = 900) %>%
  dyRangeSelector(dateWindow = c(as.Date("1990-01-01"), as.Date(end_date))) %>%
  dyAxis("y", valueRange = c(-2000, 30000))
gdp_decomposition2

saveWidget(gdp_decomposition2, "gdp_decomposition2_dy.html")



