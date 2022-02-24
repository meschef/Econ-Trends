# load required packages
library(dplyr)
library(dygraphs)
library(fredr)
library(ggplot2)
library(tidyverse)
library(widgetframe)
library(xts)

# set FRED API key
API = Sys.getenv("API_key")
fredr_set_key("aae3658b653ba18c15305b7b43fcdb08")


# Import National Unemployment Rate data from FRED, only keep date and value columns. Import South Carolina Unemployment Rate data from FRED, only keep date and value columns.
    ## National Unemployment rate:  https://fred.stlouisfed.org/series/UNRATE
    ## South Carolina unemployment rate:  https://fred.stlouisfed.org/series/SCUR
National_unemployment_rate<-fredr(series_id= "UNRATE", observation_start = as.Date("1976-01-01"))
National_unemployment_rate<- National_unemployment_rate[,c(1,3)]
South_Carolina_unemployment_rate<-fredr(series_id= "SCUR")
South_Carolina_unemployment_rate<- South_Carolina_unemployment_rate[,c(1,3)]

# Merge data frames with National and South Carolina data by date, name columns for recall.
NSCUR<-merge(National_unemployment_rate, South_Carolina_unemployment_rate, by="date", all.x=TRUE)
colnames(NSCUR)<- c("date", "national unemprate", "south caroline unemprate")
# Convert to an XTS object as required by dyGraphs, remove date column as xts format forces the dates to be row names, if you leave this column your graph is less aesthetically pleasing.
NSCUR<- xts(NSCUR, order.by = NSCUR$date)
NSCUR<-NSCUR[,2:3]

end_date = Sys.Date() + 1095

# dyGraph-- dynamic
graphTwo<- dygraph(NSCUR, ylab = "Unemployment Rate (%)", xlab = "Date") %>%
  dyAxis("y", valueRange = c(0, 20)) %>%
  dySeries("national unemprate", label= "National", color = "#B22234") %>%
  dySeries("south caroline unemprate", label= "South Carolina", color = "#003366") %>%
  dyOptions(drawPoints = TRUE, strokeWidth = 3) %>%
  dyLegend(width = 150, labelsSeparateLines = TRUE) %>% 
  dyHighlight() %>%
  dyRangeSelector(dateWindow = c(as.Date("1975-01-01"), as.Date(end_date))) %>%
  dyShading(from= "1980-01-01", to="1980-07-01", color = "#cecece")%>%
  dyShading(from= "1981-07-01", to="1982-11-01", color = "#cecece")%>%
  dyShading(from= "1990-07-01", to=" 1991-03-01", color = "#cecece")%>%
  dyShading(from= "2001-03-01", to="2001-11-01", color = "#cecece") %>%
  dyShading(from = "2007-12-01", to="2009-06-01", color = "#cecece") %>%
  dyShading(from = "2020-02-01", to= "2020-04-01" ,color = "#cecece")
graphTwo

# Save as an html widget.
saveWidget(graphTwo, "unemployment-rate.html")

# Reimport data beginning in 2005 for static graph, only keep date and value columns, merge data frames and name columns.
national_unemp_begin2005<-fredr(series_id= "UNRATE", observation_start = as.Date("2005-01-01"))
national_unemp_begin2005<- national_unemp_begin2005[,c(1,3)]
sc_unemp_begin2005<-fredr(series_id= "SCUR", observation_start = as.Date("2005-01-01"))
sc_unemp_begin2005<- sc_unemp_begin2005[,c(1,3)]
NSCUR2005<-merge(national_unemp_begin2005, sc_unemp_begin2005, by="date", all.x=TRUE)
colnames(NSCUR2005)<- c("date", "National", "South Carolina")

# Filter data to comply with "tidy" standards as ggplot is a tidyverse package -- (https://r4ds.had.co.nz/tidy-data.html).
df <- NSCUR2005 %>%
  select(date, `National`, `South Carolina`) %>%
  gather(key = "variable", value = "value", -date)

# ggplot-- static
static_unemp_rate<-ggplot(df, aes(x = date, y = value)) + labs(x = "Date", y="Unemployment Rate") +
  geom_rect(xmin=as.Date("2007-12-01"), xmax=as.Date("2009-06-01"), ymin=0, ymax=Inf, fill="#cecece", alpha=0.2) +
  geom_rect(xmin=as.Date("2020-02-01"), xmax=as.Date("2020-04-01"), ymin=0, ymax=Inf, fill="#cecece", alpha=0.2) +
  geom_line(aes(color = variable), size=1) +
  scale_color_manual(values = c("#B22234", "#003366"))+ theme_bw() +
  theme(legend.position = c(.5, .95) ,legend.title=element_blank(), legend.background=element_rect(fill = alpha("white", 0)))
static_unemp_rate

# Save as a png object.
ggsave("static unemployment rate.png",
  plot = static_unemp_rate,
  device = "png",
  width = 10,
  height = 10,
  units= "in")

### PACKAGES
# fredr-- https://cran.r-project.org/web/packages/fredr/vignettes/fredr.html
    # API key-- https://research.stlouisfed.org/docs/api/api_key.html
# tidyverse-- https://www.tidyverse.org/packages/
    ## dplyr-- https://cran.r-project.org/web/packages/dplyr/vignettes/dplyr.html ; https://dplyr.tidyverse.org/
    ## ggplot2-- https://ggplot2.tidyverse.org/ ; https://ggplot2.tidyverse.org/reference/
# dygraphs-- https://rstudio.github.io/dygraphs/ ; https://cran.r-project.org/web/packages/dygraphs/dygraphs.pdf
# widgetframe-- https://cran.r-project.org/web/packages/widgetframe/widgetframe.pdf
# xts-- https://cran.r-project.org/web/packages/xts/xts.pdf


### HELPFUL URLs
# https://www.rstudio.com/resources/cheatsheets/