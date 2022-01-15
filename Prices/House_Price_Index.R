
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

#All-Transactions House Price Index for the United States
USSTHPI<-fredr("USSTHPI", observation_start = as.Date("1990-01-01"))
USSTHPI_dt<-as.data.table(USSTHPI[,c(1,3)])


# All-Transactions House Price Index for South Carolina
SCSTHPI<-fredr("SCSTHPI", observation_start = as.Date("1990-01-01"))
SCSTHPI_dt<-as.data.table(SCSTHPI[,c(1,3)])


US_per_chg<-USSTHPI_dt[,"United States":=  value/shift(value, n=4, fill = NA)-1]
SC_per_chg<-SCSTHPI_dt[,"South Carolina":=  value/shift(value, n=4, fill = NA)-1]


pchange_both<-left_join(US_per_chg, SC_per_chg,by= "date")
final<-as.xts(pchange_both[,c(1,3,5)])

end_date = Sys.Date() + 1095

HPI<-dygraph(final, xlab= "Date", ylab = "Percent Change from Year Ago") %>%
  dyOptions(fillAlpha= 0, strokeWidth = 3, colors = c("#B22234", "#003366")) %>%
  dyAxis("y", valueRange=c(-.08,0.2)) %>%
  dyLegend(show= "always",  width  = 300, labelsSeparateLines = TRUE) %>%
  dyShading(from= "1990-07-01", to=" 1991-03-01", color = "#cecece")%>%
  dyShading(from= "2001-03-01", to="2001-11-01", color = "#cecece") %>%
  dyShading(from = "2007-12-01", to="2009-06-01", color = "#cecece") %>%
  dyShading(from = "2020-02-01", to= "2020-04-01" ,color = "#cecece") %>%
  dyRangeSelector(dateWindow = c(as.Date("1990-01-01"), as.Date(end_date))) 
HPI
saveWidget(HPI, "hpi_dy.html")


