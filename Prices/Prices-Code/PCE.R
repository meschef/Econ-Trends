# load required packages
library(data.table)
library(dygraphs)
library(dplyr)
library(fredr)
library(purrr)
library(tidyr)
library(widgetframe)
library(xts)

# set FRED API key
API = Sys.getenv("API_key")
fredr_set_key(API)

# 1) Trimmed Mean PCE Inflation Rate (already as a percent change from last year)
PCETRIM12M159SFRBDAL<-fredr("PCETRIM12M159SFRBDAL", observation_start = as.Date("1990-01-01"))
trimmed_mean_change<-as.data.table(PCETRIM12M159SFRBDAL[,c(1,3)])
colnames(trimmed_mean_change)<- c("date", "Trimmed Mean")

# 2) Personal Consumption Expenditures Excluding Food and Energy (Chain-Type Price Index)
PCEPILFE<-fredr("PCEPILFE", observation_start = as.Date("1990-01-01"))
PCEPILFE_dt<-as.data.table(PCEPILFE[,c(1,3)])
PCEPILFE_change<-PCEPILFE_dt[,"Excluding Food and Energy":=  (value/shift(value, n=12, fill = NA)-1)*100]
PCEPILFE_change<-PCEPILFE_change[,c(1,3)]

# 3) Personal Consumption Expenditures: Chain-type Price Index
PCEPI<-fredr("PCEPI", observation_start = as.Date("1990-01-01"))
PCEPI_dt<-as.data.table(PCEPI[,c(1,3)])
PCEPI_change<-PCEPI_dt[,"Chain-type Price Index":=  (value/shift(value, n=12, fill = NA)-1)*100]
PCEPI_change<-PCEPI_change[,c(1,3)]

#combine
dt<-list( PCEPI_change, PCEPILFE_change, trimmed_mean_change) %>% reduce(left_join, by= "date")


#dy graph containing all three measures as different series

all_PCE<-dygraph(dt, xlab= "Date", ylab = "Percent Change from Year Ago") %>%
  dyOptions(strokeWidth = 3, colors = c("#B22234", "#003366", "green")) %>%
  dyLegend(show= "always",  width  = 300, labelsSeparateLines = TRUE) %>%
  dyShading(from= "1990-07-01", to=" 1991-03-01", color = "#cecece")%>%
  dyShading(from= "2001-03-01", to="2001-11-01", color = "#cecece") %>%
  dyShading(from = "2007-12-01", to="2009-06-01", color = "#cecece") %>%
  dyShading(from = "2020-02-01", to= "2020-04-01" ,color = "#cecece") %>%
  dyRangeSelector(dateWindow = c(as.Date("1990-01-01"), as.Date("2025-01-01")))
all_PCE
saveWidget(all_PCE, "pce_dy.html")




