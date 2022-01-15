# load required packages
library(data.table)
library(dygraphs)
library(fredr)
library(tidyverse)
library(widgetframe)
library(xts)

# set FRED API key
API = Sys.getenv("API_key")
fredr_set_key(API)


# Import National Employment to Population Ratio data from FRED, only keep date and value columns. 
## National Employment to Population Ratio: https://fred.stlouisfed.org/series/EMRATIO
empnational<-fredr(series_id = "EMRATIO", observation_start = as.Date("1976-10-01"))
empnational<- empnational[,c(1,3)]

# Convert data frame to data.table, name columns, covert to XTS data table.
emptopop<-data.table(empnational)
colnames(emptopop)<- c("date", "National")
emp_p<- as.xts.data.table(emptopop)

end_date = Sys.Date() + 1095

# dyGraph-- dynamic
graphThree<-dygraph(emp_p, xlab = "Date", ylab = "Employment to Population Ratio") %>%
  dySeries(name= "National", color ="#B22234") %>%
  dyAxis("y", valueRange = c(50, 70)) %>%
  dyRangeSelector(dateWindow = c(as.Date("1975-01-01"), as.Date(end_date))) %>%
  dyOptions(drawPoints = TRUE, strokeWidth = 3, rightGap = 20) %>%
  dyLegend(width = 100, labelsSeparateLines = TRUE) %>%
  dyShading(from= "1980-01-01", to="1980-07-01", color = "#cecece")%>%
  dyShading(from= "1981-07-01", to="1982-11-01", color = "#cecece")%>%
  dyShading(from= "1990-07-01", to=" 1991-03-01", color = "#cecece")%>%
  dyShading(from= "2001-03-01", to="2001-11-01", color = "#cecece") %>%
  dyShading(from = "2007-12-01", to="2009-06-01", color = "#cecece") %>%
  dyShading(from = "2020-02-01", to= "2020-04-01" ,color = "#cecece")
graphThree

# Save as an html widget.
saveWidget(graphThree, "employment-to-population.html")

# Filter data to tidy # filter data to comply with "tidy" standards as ggplot is a tidyverse package -- (https://r4ds.had.co.nz/tidy-data.html).
df <- emptopop %>%
  select(date, `National`) %>%
  gather(key = "variable", value = "value", -date)

# ggplot-- static
static_graphThree<-ggplot(df, aes(x = date, y = value)) + labs(x = "Date", y="Employment to Population Ratio") +
  geom_rect(xmin=as.Date("1980-01-01"), xmax=as.Date("1980-07-01"), ymin=0, ymax=Inf, fill="#cecece", alpha=0.2) +
  geom_rect(xmin=as.Date("1981-07-01"), xmax=as.Date("1982-11-01"), ymin=0, ymax=Inf, fill="#cecece", alpha=0.2) +
  geom_rect(xmin=as.Date("1990-07-01"), xmax=as.Date("1991-03-01"), ymin=0, ymax=Inf, fill="#cecece", alpha=0.2) +
  geom_rect(xmin=as.Date("2001-03-01"), xmax=as.Date("2001-11-01"), ymin=0, ymax=Inf, fill="#cecece", alpha=0.2) +
  geom_rect(xmin=as.Date("2007-12-01"), xmax=as.Date("2009-06-01"), ymin=0, ymax=Inf, fill="#cecece", alpha=0.2) +
  geom_rect(xmin=as.Date("2020-02-01"), xmax=as.Date("2020-04-01"), ymin=0, ymax=Inf, fill="#cecece", alpha=0.2) +
  geom_line(aes(color = variable), size=1) +
  scale_color_manual(values = c("#B22234", "#003366"))+
  theme_bw() +
  theme(legend.position = c(.14, .95) ,legend.title=element_blank(), 
  legend.background=element_rect(fill = alpha("white", 0)))
static_graphThree

# Save as a png object.
ggsave("static employment to population.png",
       plot = static_graphThree,
       device = "png",
       width = 10,
       height = 10,
       units= "in")

### PACKAGES
# fredr-- https://cran.r-project.org/web/packages/fredr/vignettes/fredr.html
    # API key-- https://research.stlouisfed.org/docs/api/api_key.html
# data.table-- https://cran.r-project.org/web/packages/data.table/data.table.pdf
# dygraphs-- https://rstudio.github.io/dygraphs/ ; https://cran.r-project.org/web/packages/dygraphs/dygraphs.pdf
# tidyverse-- https://www.tidyverse.org/packages/
# xts-- https://cran.r-project.org/web/packages/xts/xts.pdf