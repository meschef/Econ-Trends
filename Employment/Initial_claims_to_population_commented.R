# load required packages
library(data.table)
library(dygraphs)
library(dplyr)
library(fredr)
library(ggplot2)
library(stringr)
library(tidyr)
library(tidyverse)
library(widgetframe)
library(xts)

# set FRED API key
API = Sys.getenv("API_key")
fredr_set_key(API)


# Import national initial claims data from FRED, only keep date and value columns. Import national population data from FRED, only keep date and value columns. 
    ## https://fred.stlouisfed.org/series/ICSA
    ## https://fred.stlouisfed.org/series/POPTHM
National_initial_claims<-fredr(series_id= "ICSA", observation_start = as.Date("2020-02-01"))
view(National_initial_claims)
National_initial_claims<- National_initial_claims[,c(1,3)]
POPTHM<-fredr(series_id= "POPTHM", observation_start = as.Date("2020-02-01"))
POPTHM<- POPTHM[,c(1,3)]

# Merge national initial claims and population data frames, rename columns for recall.
NICPOP<-merge(x=National_initial_claims, y=POPTHM, by="date", all.x = TRUE)
colnames(NICPOP)<- c("date", "initial claims", "population")

# Initial claims data is reported weekly and population data is reported monthly, so you must fill every week with the population numbers from that month in order to be able to calculate a ratio.
NICPOP<-NICPOP%>% fill(population, .direction= "down")

# Create new column within your dataframe for the result of initial claims/ population to be saved in, population data is recorded in thousands in FRED but initial claims is the number so you must convert units.
NICPOP$National<- NICPOP$`initial claims`/(NICPOP$population*10) 

# Only keep date and value column.
nicpop<-as.data.frame(NICPOP[,c(1,4)])

# Import South Carolina initial claims data from FRED, only keep date and value columns. Import South Carolina population data from FRED, only keep date and value columns.
    ## https://fred.stlouisfed.org/series/SCICLAIMS
    ## https://fred.stlouisfed.org/series/SCPOP
SCICLAIMS<-fredr(series_id= "SCICLAIMS", observation_start = as.Date("2020-01-01"))
SCICLAIMS<-SCICLAIMS[,c(1,3)]
SCPOP<-fredr(series_id= "SCPOP", observation_start = as.Date("2020-01-01"))
SCPOP<-SCPOP[,c(1,3)]

# Use column bind function to merge initial claims and population data, only keep date and value columns.
SCICPOP<-cbind(SCICLAIMS, SCPOP)
SCICPOP<-SCICPOP[,c(1,2,4)]

# Name columns for recall.
colnames(SCICPOP)<- c("date", "initial claims", "population")
# Create new column within your dataframe for the result of initial claims/ population to be saved in, population data is recorded in thousands in FRED but initial claims is the number so you must convert units. only keep date and value columns. 
SCICPOP$SC<- SCICPOP$`initial claims`/(SCICPOP$population*10)
scicpop<- as.data.frame(SCICPOP[, c(1,4)])


# Merge South Carolina ratio and national ratio data frames to prepare to graph.
nicscic<-merge(nicpop,scicpop, all.x=TRUE)

# Saved a static version of the data frame for a later step.
staticdf<-nicscic
colnames(staticdf)<- c("date", "National", "South Carolina")

# Convert first data frame to an XTS object as required by dygraphs.
nicscic<-xts(nicscic, order.by= nicscic$date)
# Remove date column as xts format forces the dates to be row names, if you leave this column your graph is less aesthetically pleasing.
nicscic<-nicscic[,c(2,3)]


end_date = Sys.Date() + 1095

# dyGraph-- dynamic
graphOneBar<-dygraph(nicscic, ylab = "Weekly Initial Claims (% of population)", xlab = "Date")  %>%
  dySeries("National", label= "National", color = "#B22234") %>%
  dySeries("SC", label= "South Carolina", color = "#003366") %>%
  dyRangeSelector(dateWindow = c(as.Date("2020-02-01"), as.Date(end_date))) %>%
  dyShading(from = "2020-02-01", to= "2020-04-01" ,color = "#cecece")%>%
  dyOptions(fillAlpha= 0.8) %>%
  dyLegend(width = 150, labelsSeparateLines = TRUE) %>%
  dyBarChart()
graphOneBar

# Save as an html widget.
saveWidget(graphOneBar, "dygraph initial claims ratio.html")

# Filter the static version of the final data frame we created in line 59 to comply with R's standard of "tidy data" as ggplot is a tidyverse package -- (https://r4ds.had.co.nz/tidy-data.html).
df <- staticdf %>%
  select(date, `National`, `South Carolina`) %>%
  gather(key = "variable", value = "value", -date)

# ggplot -- static
graphOneStatic<-ggplot(df, aes(x = date, y = value, fill=variable)) + 
  labs(x = "Date", y="Weekly Initial Claims (% of population)") +
  geom_rect(xmin=as.Date("2020-02-01"), xmax=as.Date("2020-04-01"), ymin=0, ymax=Inf, fill="#cecece", alpha=0.06) +
  geom_col() +
  scale_fill_manual(values = c("#B22234", "#003366")) +
  theme_bw() +
  theme(legend.position = c(.8, .9) ,legend.title=element_blank(), legend.background=element_rect(fill = alpha("white", 0)))
graphOneStatic

# Save as a png object.
ggsave("static weekly initial claims.png",
       plot = graphOneStatic,
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
    ## stringr-- https://stringr.tidyverse.org/ 
    ## tidyr-- https://tidyr.tidyverse.org/
# data.table-- https://cran.r-project.org/web/packages/data.table/data.table.pdf
# dygraphs-- https://rstudio.github.io/dygraphs/ ; https://cran.r-project.org/web/packages/dygraphs/dygraphs.pdf
# widgetframe-- https://cran.r-project.org/web/packages/widgetframe/widgetframe.pdf
# xts-- https://cran.r-project.org/web/packages/xts/xts.pdf


### HELPFUL URLs
# https://www.rstudio.com/resources/cheatsheets/


