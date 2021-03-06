library(dplyr)
library(dygraphs)
library(ggplot2)
library(widgetframe)
library(xts)
library(readxl)
library(tidyverse)
library(tidyr)
library(lubridate)

# USA Trade Online Harmonized System (HS) District-level Data:
# https://usatrade.census.gov/data/Perspective60/Browse/BrowseTables.aspx
# See README_trade.md for instructions on how I created the reports

# import, clean, merge
exp <- read_excel("data/Standard Report - Exports.xlsx", skip = 7, 
                  col_types = c("text", "guess", "numeric"), 
                  col_names = c("Country", "Date", "Export Value"))
imp <- read_excel("data/Standard Report - Imports.xlsx", skip = 8, 
                  col_types = c("text", "guess", "numeric"), 
                  col_names = c("Country", "Date", "Import Value"))
exp <- exp %>% fill(Country)
imp <- imp %>% fill(Country)
trade <- merge(exp, imp, by = c("Country", "Date"))

trade$Date <- as.Date(trade$Date, format = "%Y-%m-%d")

# scale trade value: dollars -> billions
trade$`Export Value` <- trade$`Export Value`/1000000000
trade$`Import Value` <- trade$`Import Value`/1000000000

end_date = "2022-04-01"

# GRAPHS

# World Total
# dynamic
tradeWorld <- trade[trade$`Country` %in% "World Total",-c(1)]
tradeWorld <- xts(tradeWorld, order.by = tradeWorld$Date)
tradeWorld <- tradeWorld[,-c(1)]
dygraph_world <- dygraph(tradeWorld, xlab = "Date", ylab = "Billions of US Dollars") %>%
  dySeries("Export Value", label = "Exports", color = "#B22234") %>%
  dySeries("Import Value", label = "Imports", color = "#003366") %>%
  dyOptions(drawPoints = TRUE, strokeWidth = 3, rightGap = TRUE) %>%
  dyLegend(width = 150, labelsSeparateLines = TRUE) %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyHighlight() %>%
  dyShading(from = "2020-02-01", to= "2020-04-01" ,color = "#cecece") %>%
  dyAxis("y", valueRange = c(70,320)) %>%
  dyRangeSelector(dateWindow = c(as.Date("2009-12-01"), as.Date(end_date)))
dygraph_world
saveWidget(dygraph_world, "world-trade.html")

# static
tradeWorld <- trade[trade$`Country` %in% "World Total",-c(1)]
tradeWorld <- tradeWorld %>%
  gather(key = "variable", value = "value", -Date)
graph_world <- ggplot(tradeWorld, aes(x = Date, y = value)) + labs(x = "Date", y = "Billions of US Dollars") +
  geom_rect(xmin=as.Date("2020-02-01"), xmax=as.Date("2020-04-01"), ymin=0, ymax=Inf, fill="#cecece", alpha=0.2) +
  geom_line(aes(color=variable), size=1) +
  scale_color_manual(values = c("#B22234", "#003366"))
graph_world


# Mexico
#dynamic
tradeMX <- trade[trade$`Country` %in% "Mexico", -c(1)]
tradeMX <- xts(tradeMX, order.by = tradeMX$Date)
tradeMX <- tradeMX[,-c(1)]
dygraph_MX <- dygraph(tradeMX, xlab = "Date", ylab = "Billions of US Dollars") %>%
  dySeries("Export Value", label = "Exports", color = "#B22234") %>%
  dySeries("Import Value", label = "Imports", color = "#003366") %>%
  dyOptions(drawPoints = TRUE, strokeWidth = 3, rightGap = TRUE) %>%
  dyLegend(width = 150, labelsSeparateLines = TRUE) %>%
  dyHighlight() %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyShading(from = "2020-02-01", to= "2020-04-01" ,color = "#cecece") %>%
  dyAxis("y", valueRange = c(8, 45)) %>%
  dyRangeSelector(dateWindow = c(as.Date("2009-12-01"), as.Date(end_date)))
dygraph_MX
saveWidget(dygraph_MX, "mexico-trade.html")

#static
tradeMX <- trade[trade$`Country` %in% "Mexico", -c(1)]
tradeMX <- tradeMX %>%
  gather(key="variable", value="value", -Date)
graph_MX <- ggplot(tradeMX, aes(x = Date, y = value)) + labs(x = "Date", y = "Billions US Dollars") +
  geom_rect(xmin=as.Date("2020-02-01"), xmax=as.Date("2020-04-01"), ymin=0, ymax=Inf, fill="#cecece", alpha=0.2) +
  geom_line(aes(color=variable), size=1) +
  scale_color_manual(values = c("#B22234", "#003366"))
graph_MX

# Canada
#dynamic
tradeCA <- trade[trade$`Country` %in% "Canada", -c(1)]
tradeCA <- xts(tradeCA, order.by = tradeCA$Date)
tradeCA <- tradeCA[,-c(1)]
dygraph_CA <- dygraph(tradeCA, xlab = "Date", ylab = "Billions of US Dollars") %>%
  dySeries("Export Value", label = "Exports", color = "#B22234") %>%
  dySeries("Import Value", label = "Imports", color = "#003366") %>%
  dyOptions(drawPoints = TRUE, strokeWidth = 3, rightGap = TRUE) %>%
  dyLegend(width = 150, labelsSeparateLines = TRUE) %>%
  dyHighlight() %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyShading(from = "2020-02-01", to= "2020-04-01" ,color = "#cecece") %>%
  dyAxis("y", valueRange = c(12, 40)) %>%
  dyRangeSelector(dateWindow = c(as.Date("2009-12-01"), as.Date(end_date)))
dygraph_CA
saveWidget(dygraph_CA, "canada-trade.html")

#static
tradeCA <- trade[trade$`Country` %in% "Canada", -c(1)]
tradeCA <- tradeCA %>%
  gather(key="variable", value="value", -Date)
graph_CA <- ggplot(tradeCA, aes(x = Date, y = value)) + labs(x = "Date", y = "US Dollars") +
  geom_rect(xmin=as.Date("2020-02-01"), xmax=as.Date("2020-04-01"), ymin=0, ymax=Inf, fill="#cecece", alpha=0.2) +
  geom_line(aes(color=variable), size=1) +
  scale_color_manual(values = c("#B22234", "#003366"))
graph_CA

# NAFTA (MX + CA)
tradeCA <- trade[trade$`Country` %in% "Canada", -c(1)]
tradeMX <- trade[trade$`Country` %in% "Mexico", -c(1)]
expNAFTA <- data.frame(tradeMX$`Export Value` + tradeCA$`Export Value`)
impNAFTA <- data.frame(tradeMX$`Import Value` + tradeCA$`Import Value`)
tradeNAFTA <- data.frame(tradeCA$Date, expNAFTA, impNAFTA)
colnames(tradeNAFTA) <- c("Date", "Export Value", "Import Value")

#dynamic
tradeNAFTA <- xts(tradeNAFTA, order.by = tradeNAFTA$Date)
tradeNAFTA <- tradeNAFTA[,-c(1)]
dygraph_NAFTA <- dygraph(tradeNAFTA, xlab = "Date", ylab = "Billions of US Dollars") %>%
  dySeries("Export Value", label = "Exports", color = "#B22234") %>%
  dySeries("Import Value", label = "Imports", color = "#003366") %>%
  dyOptions(drawPoints = TRUE, strokeWidth = 3, rightGap = TRUE) %>%
  dyLegend(width = 150, labelsSeparateLines = TRUE) %>%
  dyHighlight() %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyShading(from = "2020-02-01", to= "2020-04-01" ,color = "#cecece") %>%
  dyAxis("y", valueRange = c(20, 85)) %>%
  dyRangeSelector(dateWindow = c(as.Date("2009-12-01"), as.Date(end_date)))
dygraph_NAFTA
saveWidget(dygraph_NAFTA, "nafta-trade.html")

#static
tradeNAFTA <- tradeNAFTA %>%
  gather(key="variable", value="value", -Date)
graph_NAFTA <- ggplot(tradeNAFTA, aes(x = Date, y = value)) + labs(x = "Date", y = "US Dollars") +
  geom_rect(xmin=as.Date("2020-02-01"), xmax=as.Date("2020-04-01"), ymin=0, ymax=Inf, fill="#cecece", alpha=0.2) +
  geom_line(aes(color=variable), size=1) +
  scale_color_manual(values = c("#B22234", "#003366"))
graph_NAFTA

# China
#dynamic
tradeCH <- trade[trade$`Country` %in% "China", -c(1)]
tradeCH <- xts(tradeCH, order.by = tradeCH$Date)
tradeCH <- tradeCH[,-c(1)]
dygraph_CH <- dygraph(tradeCH, xlab = "Date", ylab = "Billions of US Dollars") %>%
  dySeries("Export Value", label = "Exports", color = "#B22234") %>%
  dySeries("Import Value", label = "Imports", color = "#003366") %>%
  dyOptions(drawPoints = TRUE, strokeWidth = 3, rightGap = TRUE) %>%
  dyLegend(width = 150, labelsSeparateLines = TRUE) %>%
  dyHighlight() %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyShading(from = "2020-02-01", to= "2020-04-01" ,color = "#cecece") %>%
  dyAxis("y", valueRange = c(0, 65)) %>%
  dyRangeSelector(dateWindow = c(as.Date("2009-12-01"), as.Date(end_date)))
dygraph_CH
saveWidget(dygraph_CH, "china-trade.html")

#static
tradeCH <- trade[trade$`Country` %in% "China", -c(1)]
tradeCH <- tradeCH %>%
  gather(key="variable", value="value", -Date)
graph_CH <- ggplot(tradeCH, aes(x = Date, y = value)) + labs(x = "Date", y = "US Dollars") + 
  geom_rect(xmin=as.Date("2020-02-01"), xmax=as.Date("2020-04-01"), ymin=0, ymax=Inf, fill="#cecece", alpha=0.2) +
  geom_line(aes(color=variable), size=1) +
  scale_color_manual(values = c("#B22234", "#003366"))
graph_CH

# Europe
#dynamic
tradeEU <- trade[trade$Country %in% "Europe", -c(1)]
tradeEU <- xts(tradeEU, order.by = tradeEU$Date)
tradeEU <- tradeEU[,-c(1)]
dygraph_EU <- dygraph(tradeEU, xlab = "Date", ylab = "Billions of US Dollars") %>%
  dySeries("Export Value", label = "Exports", color = "#B22234") %>%
  dySeries("Import Value", label = "Imports", color = "#003366") %>%
  dyOptions(drawPoints = TRUE, strokeWidth = 3, rightGap = TRUE) %>%
  dyLegend(width = 150, labelsSeparateLines = TRUE) %>%
  dyHighlight() %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyShading(from = "2020-02-01", to= "2020-04-01" ,color = "#cecece") %>%
  dyAxis("y", valueRange = c(15, 75)) %>%
  dyRangeSelector(dateWindow = c(as.Date("2009-12-01"), as.Date(end_date)))
dygraph_EU
saveWidget(dygraph_EU, "europe-trade.html")

#static
tradeEU <- trade[trade$Country %in% "Europe", -c(1)]
tradeEU <- tradeEU %>%
  gather(key="variable", value="value", -Date)
graph_EU <- ggplot(tradeEU, aes(x = Date, y = value)) + labs(x = "Date", y = "US Dollars") +
  geom_rect(xmin=as.Date("2020-02-01"), xmax=as.Date("2020-04-01"), ymin=0, ymax=Inf, fill="#cecece", alpha=0.2) +
  geom_line(aes(color=variable), size=1) +
  scale_color_manual(values = c("#B22234", "#003366"))
graph_EU

# Japan
#dynamic
tradeJP <- trade[trade$Country %in% "Japan", -c(1)]
tradeJP <- xts(tradeJP, order.by = tradeJP$Date)
tradeJP <- tradeJP[,-c(1)]
dygraph_JP <- dygraph(tradeJP, xlab = "Date", ylab = "Billions of US Dollars") %>%
  dySeries("Export Value", label = "Exports", color = "#B22234") %>%
  dySeries("Import Value", label = "Imports", color = "#003366") %>%
  dyOptions(drawPoints = TRUE, strokeWidth = 3, rightGap = TRUE) %>%
  dyLegend(width = 150, labelsSeparateLines = TRUE) %>%
  dyHighlight() %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyShading(from = "2020-02-01", to= "2020-04-01" ,color = "#cecece") %>%
  dyAxis("y", valueRange = c(3, 16)) %>%
  dyRangeSelector(dateWindow = c(as.Date("2009-12-01"), as.Date(end_date)))
dygraph_JP
saveWidget(dygraph_JP, "japan-trade.html")

#static
tradeJP <- trade[trade$Country %in% "Japan", -c(1)]
tradeJP <- tradeJP %>%
  gather(key="variable", value="value", -Date)
graph_JP <- ggplot(tradeJP, aes(x = Date, y = value)) + labs(x = "Date", y = "US Dollars") + 
  geom_rect(xmin=as.Date("2020-02-01"), xmax=as.Date("2020-04-01"), ymin=0, ymax=Inf, fill="#cecece", alpha=0.2) +
  geom_line(aes(color=variable), size=1) +
  scale_color_manual(values = c("#B22234", "#003366"))
graph_JP


# year-over-year growth rate graph (World Total)

tradeWorld <- trade[trade$`Country` %in% "World Total",-c(1)]
# want last 24 months
tradeWorld <- tradeWorld %>% tail(24)
# inputting YoY growth rates -- using lubridate package
tradeWorld <- tradeWorld %>% group_by(month=month(Date)) %>%
  arrange(month) %>%
  mutate(YoY_exp=`Export Value`/lag(`Export Value`,1)) %>%
  mutate(YoY_imp=`Import Value`/lag(`Import Value`,1)) %>%
  ungroup() %>% arrange(month)
tradeWorld <- tradeWorld[,-c(2:4)]
# dygraph
tradeWorld <- xts(tradeWorld, order.by = tradeWorld$Date)
tradeWorld <- tradeWorld[-c(1:12),-c(1)]
graph_growth <- dygraph(tradeWorld, ylab = "YoY Growth Rate", xlab = "Date")  %>%
  dySeries("YoY_exp", label= "Export Value", color = "#B22234") %>%
  dySeries("YoY_imp", label= "Import Value", color = "#003366") %>%
  dyOptions(fillAlpha = 0.8, rightGap = 15) %>%
  dyAxis("x",rangePad = 20, pixelsPerLabel = 64) %>%
  dyLegend(width = 150, labelsSeparateLines = TRUE) %>%
  dyBarChart() %>%
  dyAxis("y", valueRange = c(0.8, 1.8)) %>%
  dyRangeSelector(dateWindow = c(as.Date("2021-02-15"), as.Date("2022-02-15")))
graph_growth
saveWidget(graph_growth, "growth-rates.html")
