# load required packages
library(data.table)
library(dplyr)
library(dygraphs)
library(fredr)
library(widgetframe)
library(xts)

# set FRED API key
API = Sys.getenv("API_key")
fredr_set_key(API)



# Import National Unemployment Rate data from FRED, only keep date and value columns, name columns. Import South Carolina Unemployment Rate data from FRED, only keep date and value columns, name columns.
    ## National Employment Level: https://fred.stlouisfed.org/series/CE16OV
    ## South Carolina All Employees: https://fred.stlouisfed.org/series/SCNA
SCemp<-fredr(series_id= "SCNA", observation_start = as.Date("2020-02-01"))
SCemp<-SCemp[,c(1,3)]
colnames(SCemp)<-c("date", "value")
Nemp<-fredr(series_id= "CE16OV", observation_start = as.Date("2020-02-01"))
Nemp<-Nemp[,c(1,3)]
colnames(Nemp)<-c("date", "value")

# Convert to data.table
SCemp<- data.table(SCemp)
Nemp<- data.table(Nemp)

# Calculate National and South Carolina percent change from February (multiply by 100 to get percent).
SC<-SCemp %>% 
  mutate(pChange = ((value- 2196.4)/2196.4) * 100)
National<-Nemp %>%
  mutate(pChange= ((value- 158732)/158732) * 100)

# Merge data tables and only keep date and percent change value, name columns.
q<-merge(SC, National, by="date")
q<-q[, c(1,3,5)]
colnames(q)<- c("date", "South Carolina", "National")

end_date = Sys.Date() + 30

# dyGraph-- dynamic
graphFour<-dygraph(q, xlab = "Date", ylab = "Percent Change <br> from February 2020") %>%
  dySeries("National", label = "National", color = "#B22234" ) %>%
  dySeries("South Carolina", label="South Carolina", color = "#003366") %>%
  dyAxis("y", valueRange = c(-18, 3)) %>%
  dyRangeSelector(dateWindow = c(as.Date("2020-02-01"), as.Date(end_date))) %>%
  dyShading(from = "2020-02-01", to= "2020-04-01" ,color = "#cecece") %>%
  dyOptions(drawPoints = TRUE, strokeWidth = 3, rightGap = 10) %>%
  dyLegend(width = 150, labelsSeparateLines = TRUE) %>% 
  dyHighlight()
graphFour

# Save as an html widget.
saveWidget(graphFour, "jobs-recovery.html")

# Filter data to tidy # filter data to comply with "tidy" standards as ggplot is a tidyverse package -- (https://r4ds.had.co.nz/tidy-data.html).
staticdf <- q %>%
  select(date, National, `South Carolina`) %>%
  gather(key = "variable", value = "value", -date)

# ggplot-- static
static_jobsrecovery<-ggplot(staticdf, aes(x = date, y = value)) +
  labs(x = "Date", y="Percent Change from February 2021") +
  geom_rect(xmin=as.Date("2020-02-01"), xmax=as.Date("2020-04-01"), ymin=-20, ymax=Inf, fill="#cecece", alpha=0.06) +
  geom_line(aes(color = variable), size=1) +
  scale_color_manual(values = c("#B22234", "#003366")) +
  theme_bw() +
  theme(legend.position = c(.8, .15) ,legend.title=element_blank(),legend.background=element_rect(fill = alpha("white", 0)))
static_jobsrecovery 

# Save as a png object.
ggsave("static jobs recovery.png",
       plot = static_jobsrecovery,
       device = "png",
       width = 10,
       height = 10,
       units= "in")


### PACKAGES
# fredr-- https://cran.r-project.org/web/packages/fredr/vignettes/fredr.html
    # API key-- https://research.stlouisfed.org/docs/api/api_key.html
# data.table-- https://cran.r-project.org/web/packages/data.table/data.table.pdf
# dplyr-- https://cran.r-project.org/web/packages/dplyr/vignettes/dplyr.html ; https://dplyr.tidyverse.org/
# dygraphs-- https://rstudio.github.io/dygraphs/ ; https://cran.r-project.org/web/packages/dygraphs/dygraphs.pdf
# xts-- https://cran.r-project.org/web/packages/xts/xts.pdf
