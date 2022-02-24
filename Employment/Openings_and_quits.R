# load required packages
library(data.table)
library(dygraphs)
library(dplyr)
library(fredr)
library(purrr)
library(tidyr)
library(widgetframe)
library(xts)
library(ggplot2)

# set FRED API key
API = Sys.getenv("API_key")
fredr_set_key(API)

# Total non farm quits quits 
JTSQUL <- fredr("JTSQUL", observation_start = as.Date("2001-01-01"))
quits <- as.data.table(JTS1000QUL[,c(1,3)])

#Total non farm job openings
JTSJOL <- fredr("JTSJOL", observation_start = as.Date("2001-01-01"))
openings <- as.data.table(JTS1000JOL[,c(1,3)])

#combine
dt<-list(openings, quits) %>% reduce(left_join, by= "date")

end_date = Sys.Date() + 730

openings_and_quits<- dygraph(dt, xlab="Date", ylab="Level in Thousands") %>%
  dySeries("value.x", label = "Jobs Openings", color = "#B22234") %>%
  dySeries("value.y", label = "Quits", color = "#003366") %>%
  dyAxis("y", valueRange = c(0,13000)) %>%
  dyOptions(strokeWidth = 3) %>%
  dyLegend(show= "always",  width  = 300, labelsSeparateLines = TRUE) %>%
  dyShading(from= "2001-03-01", to="2001-11-01", color = "#cecece") %>%
  dyShading(from = "2007-12-01", to="2009-06-01", color = "#cecece") %>%
  dyShading(from = "2020-02-01", to= "2020-04-01" ,color = "#cecece") %>%
  dyRangeSelector(dateWindow = c(as.Date("2000-11-01"), as.Date(end_date)))
openings_and_quits

saveWidget(openings_and_quits, "openings_and_quits.html")