#Job openings over unemployment 
library(dplyr)
library(dygraphs)
library(fredr)
library(ggplot2)
library(tidyverse)
library(widgetframe)
library(xts)
library(readxl)

#Set API Key
API = Sys.getenv("API_key")
fredr_set_key(API)

#National Job Openings Data 
job_openings_US <- read_excel("/Users/meghanscheffey/OneDrive - Clemson University/Econ_Trends/Econ-Trends/Data/JOLTS/Job_openings_US.xlsx", skip=12)
job_openings_US <- data.frame(job_openings_US)
colnames(job_openings_US) = c("Series", "Year", "Month", "Job Openings")
job_openings_US$Month <- recode(job_openings_US$Month, "M01" = "01", "M02"="02", "M03"="03", "M04"="04", "M05"="05", "M06"="06", "M07"="07", "M08" = "08", "M09"="09", "M10"="10", "M11"="11", "M12"="12")
job_openings_US$Date <- paste(job_openings_US$Year, job_openings_US$Month, "01", sep = "-")
job_openings_US = subset(job_openings_US, select = -c(Series, Year, Month))
job_openings_US$Date = as.Date(job_openings_US$Date)

#National Unemployment Level 
unemployment_US<- fredr(series_id = "UNEMPLOY", observation_start = as.Date("2000-12-01"))
unemployment_US <- unemployment_US[,c(1,3)]
colnames(unemployment_US)<-c("Date","Unemployment")
unemployment_US<-data.frame(unemployment_US)

#merge National data and get job openings over unemployed
dt = left_join(x=job_openings_US, y=unemployment_US, by="Date")
dt$US_Value = dt$`Job Openings`/dt$Unemployment
dt <- dt[,c(2,4)]

#South Carolina Openings Data
job_openings_SC <- read_excel("/Users/meghanscheffey/OneDrive - Clemson University/Econ_Trends/Econ-Trends/Data/JOLTS/Job_openings_SC.xlsx", skip=12)
job_openings_SC <- data.frame(job_openings_SC)
colnames(job_openings_SC) = c("Series", "Year", "Month", "Job Openings")
job_openings_SC$Month <- recode(job_openings_SC$Month, "M01" = "01", "M02"="02", "M03"="03", "M04"="04", "M05"="05", "M06"="06", "M07"="07", "M08" = "08", "M09"="09", "M10"="10", "M11"="11", "M12"="12")
job_openings_SC$Date <- paste(job_openings_SC$Year, job_openings_SC$Month, "01", sep = "-")
job_openings_SC = subset(job_openings_SC, select = -c(Series, Year, Month))
job_openings_SC$Date = as.Date(job_openings_SC$Date)

#South Carolina Unemployment Monthly Data
unemployment_SC <-read_excel("/Users/meghanscheffey/OneDrive - Clemson University/Econ_Trends/Econ-Trends/Data/JOLTS/SC_unemployment.xlsx", skip = 10)
unemployment_SC <- unemployment_SC[,-c(1)]
unemployment_SC$Period <- recode(unemployment_SC$Period, "M01" = "01", "M02"="02", "M03"="03", "M04"="04", "M05"="05", "M06"="06", "M07"="07", "M08" = "08", "M09"="09", "M10"="10", "M11"="11", "M12"="12")
unemployment_SC$Date <- paste(unemployment_SC$Year, unemployment_SC$Period, "01", sep = "-")
unemployment_SC$Date <- as.Date(unemployment_SC$Date)
unemployment_SC<-unemployment_SC[,c(3,4)]
colnames(unemployment_SC)<-c("unemployment","Date")
unemployment_SC$unemployment<-unemployment_SC$unemployment/1000

#merge SC data and get job openings over unemployed
SC = left_join(x=job_openings_SC, y=unemployment_SC, by="Date")
SC$SC_Value = SC$`Job Openings`/SC$unemployment
SC <- SC[,c(2,4)]

#South job openings 
south_openings<-read_excel("/Users/meghanscheffey/OneDrive - Clemson University/Econ_Trends/Econ-Trends/Data/JOLTS/South_openings.xlsx", skip=12)
colnames(south_openings) = c("Series", "Year", "Month", "Job Openings")
south_openings$Month <- recode(south_openings$Month, "M01" = "01", "M02"="02", "M03"="03", "M04"="04", "M05"="05", "M06"="06", "M07"="07", "M08" = "08", "M09"="09", "M10"="10", "M11"="11", "M12"="12")
south_openings$Date <- paste(south_openings$Year, south_openings$Month, "01", sep = "-")
south_openings = subset(south_openings, select = -c(Series, Year, Month))
south_openings$Date = as.Date(south_openings$Date)

#South unemployment level
south_unemployment = read_excel("/Users/meghanscheffey/OneDrive - Clemson University/Econ_Trends/Econ-Trends/Data/JOLTS/South_unemployment.xlsx", skip=10)
south_unemployment$Period <- recode(south_unemployment$Period,"M01" = "01", "M02"="02", "M03"="03", "M04"="04", "M05"="05", "M06"="06", "M07"="07", "M08" = "08", "M09"="09", "M10"="10", "M11"="11", "M12"="12")
south_unemployment$Date <- paste(south_unemployment$Year, south_unemployment$Period, "01", sep = "-")
south_unemployment<- south_unemployment[,c(4,5)]
south_unemployment$Date<-as.Date(south_unemployment$Date)
south_unemployment$Value=south_unemployment$Value/1000

#Merge South data to get job openings over unemployed
South=left_join(x=south_openings, y=south_unemployment, by= "Date")
South$South_Value= South$`Job Openings`/South$Value
South<-South[,c(2,4)]

#Merge all data sets
dt <-list(dt,SC,South) %>% reduce(left_join, by="Date")
dynamic_dt<- xts(dt, order.by = dt$Date)
dynamic_dt<-dynamic_dt[,c(2:4)]

openings_over_unemployed <- dygraph(dynamic_dt, ylab="Ratio, seasonally adjusted", xlab="Date")%>%
  dySeries("US_Value", label= "National", color = "#B22234") %>%
  dySeries("SC_Value", label = "South Carolina", color = "#003366") %>%
  dySeries("South_Value", label = "South", color = "#1a9a0a")%>%
  dyOptions(drawPoints = TRUE, strokeWidth = 3) %>%
  dyLegend(width = 150, labelsSeparateLines = TRUE) %>% 
  dyHighlight() %>%
  dyRangeSelector(dateWindow = c(as.Date("2000-11-01"), as.Date("2022-10-01"))) %>%
  dyShading(from= "2001-03-01", to="2001-11-01", color = "#cecece") %>%
  dyShading(from = "2007-12-01", to="2009-06-01", color = "#cecece") %>%
  dyShading(from = "2020-02-01", to= "2020-04-01" ,color = "#cecece")%>%
  dyAxis("y", valueRange=c(0,3))

openings_over_unemployed

saveWidget(openings_over_unemployed, "openings_over_unemployed.html")



colnames(dt)=c("Date","National","South Carolina", "South")
staticdt <- dt %>%
  select(Date, National, `South Carolina`, South) %>%
  gather(key = "variable", value = "value",-Date)


#static graph
opening_over_unempoyed_static = ggplot(staticdt, aes(x = Date, y = value))+
  geom_rect(xmin=as.Date("2001-03-01"), xmax=as.Date("2001-11-01"), ymin=-5, ymax=Inf, fill="#cecece", alpha=0.01)+
  geom_rect(xmin=as.Date("2007-12-01"), xmax=as.Date("2009-06-01"), ymin=-5, ymax=Inf, fill="#cecece", alpha=0.01)+
  geom_rect(xmin=as.Date("2020-02-01"), xmax=as.Date("2020-04-01"), ymin=-5, ymax=Inf, fill="#cecece", alpha=0.01)+
  geom_path(aes(color = variable), size = .8)+
  theme_bw()+
  labs(x ="Date", y = "Ratio, seasonally adjusted", title = "Figure 1:Job Openings over Unemployed",
       caption = "Data are from Job Openings and Labor Turnover Survey conducted by the US Bureau of Labor Statistics.")+
  theme(legend.position = c(.2,.84),legend.text = element_text(size=16), legend.title=element_blank(),legend.background=element_rect(fill = alpha("white", 0)))+
  scale_color_manual(values=c("#B22234", "#398F1D","#003366"))
  
opening_over_unempoyed_static 

ggsave("opening_over_unemployed_static.png",
       plot = opening_over_unempoyed_static,
       device = "png",
       width = 12,
       height = 10,
       units= "in")
  
