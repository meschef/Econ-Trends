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

#  Average Hourly Earnings of All Employees, Total Private (CES0500000003)	
CES0500000003<-fredr("CES0500000003", observation_start = as.Date("2006-01-01"))
earnings_dt<-as.data.table(CES0500000003[,c(1,3)])

earnings_change<-earnings_dt[,"Average Hourly Earnings":=  (value/shift(value, n=12, fill = NA)-1)*100]
earnings_change<-earnings_change[,c(1,3)]


earnings_dy<-dygraph(earnings_change, xlab= "Date", ylab = "Percent Change from Year Ago") %>%
  dySeries(label = "Average Hourly Earnings") %>%
  dyOptions(fillAlpha= 0.8, strokeWidth = 3 ,colors = "#B22234") %>%
  dyLegend(show= "always",  width  = 300, labelsSeparateLines = FALSE) %>%
  dyShading(from= "1990-07-01", to=" 1991-03-01", color = "#cecece")%>%
  dyShading(from= "2001-03-01", to="2001-11-01", color = "#cecece") %>%
  dyShading(from = "2007-12-01", to="2009-06-01", color = "#cecece") %>%
  dyShading(from = "2020-02-01", to= "2020-04-01" ,color = "#cecece") %>%
  dyRangeSelector(dateWindow = c(as.Date("2006-01-01"), as.Date("2024-01-01"))) 
earnings_dy

saveWidget(earnings_dy, "earnings_dy.html")


#Average Hourly Earnings of All Employees: Total Private in South Carolina (SMU45000000500000003SA)
SMU45000000500000003SA<-fredr("SMU45000000500000003SA", observation_start = as.Date("2006-01-01"))
sc_earnings_dt<-as.data.table(SMU45000000500000003SA[,c(1,3)])
sc_earnings_change<-sc_earnings_dt[,"SC Average Hourly Earnings":=  (value/shift(value, n=12, fill = NA)-1)*100]
sc_earnings_change<-sc_earnings_change[,c(1,3)]

pchange_both<-left_join(earnings_change, sc_earnings_change,by= "date")
final<-as.xts(pchange_both)
colnames(final)<- c("National", "South Carolina")

both_earnings_dy<-dygraph(final, xlab= "Date", ylab = "Percent Change from Year Ago") %>%
  dyOptions(fillAlpha= 0.8, strokeWidth = 3 ,colors = c("#B22234", "#003366")) %>%
  dyLegend(show= "always",  width  = 300, labelsSeparateLines = FALSE) %>%
  dyShading(from= "1990-07-01", to=" 1991-03-01", color = "#cecece")%>%
  dyShading(from= "2001-03-01", to="2001-11-01", color = "#cecece") %>%
  dyShading(from = "2007-12-01", to="2009-06-01", color = "#cecece") %>%
  dyShading(from = "2020-02-01", to= "2020-04-01" ,color = "#cecece") %>%
  dyRangeSelector(dateWindow = c(as.Date("2006-01-01"), as.Date("2024-01-01"))) 
both_earnings_dy
saveWidget(both_earnings_dy, "both_earnings_dy.html")
