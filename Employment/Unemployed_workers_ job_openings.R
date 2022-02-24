# load required packages
library(data.table)
library(dplyr)
library(fredr)
library(ggplot2)
library(tidyverse)
library(dygraphs)

# set FRED API key
API = Sys.getenv("API_key")
fredr_set_key("aae3658b653ba18c15305b7b43fcdb08")

UNEMPLOY<-fredr(series_id = "UNEMPLOY", observation_start = as.Date("2001-01-01"))
JTSJOL<-fredr(series_id = "JTSJOL", observation_start = as.Date("2001-01-01"))

UNEMPLOY<-UNEMPLOY[,c(1,3)]
JTSJOL<-JTSJOL[,c(1,3)]

unemployed_opening<-merge(UNEMPLOY, JTSJOL, by ="date")
colnames(unemployed_opening)<- c("date", "Unemployed Workers", "Job Openings")

unemployed_opening <-data.table(unemployed_opening)
end_date = Sys.Date() +365

##dynamic graph
Jolts_graph<-dygraph(unemployed_opening, xlab = "Date", ylab = "Thousands of People") %>%
  dySeries("Unemployed Workers", label = "Unemployed", color = "#B22234") %>%
  dySeries("Job Openings", label = "Job Openings", color = "#003366") %>%
  dyRangeSelector(dateWindow = c(as.Date("2000-11-01"), as.Date(end_date))) %>%
  dyOptions(drawPoints = TRUE, strokeWidth = 3, rightGap = 10) %>%
  dyLegend(width = 150, labelsSeparateLines = TRUE) %>% 
  dyAxis("y", valueRange=c(0,28000))%>%
  dyShading(from= "2001-03-01", to="2001-11-01", color = "#cecece") %>%
  dyShading(from = "2007-12-01", to="2009-06-01", color = "#cecece") %>%
  dyShading(from = "2020-02-01", to= "2020-04-01" ,color = "#cecece") %>%
  dyHighlight()
Jolts_graph
saveWidget(Jolts_graph, "unemployed_openings.html")


df <- unemployed_opening %>%
  gather(key = "variable", value = "value", -date)

##static graph
openings_and_unemployed<-
  ggplot(df, aes(x = date, y = value)) + 
  labs(x = "Date", y="Thousands of People", title = "Figure 1: Job Openings and Unemployed Workers",  
       caption= "Job Openings data from JOLTS, Unemployed Workers from BLS, \n all data retrieved from FRED. (https://fred.stlouisfed.org/)") +
  geom_rect(xmin=as.Date("2007-12-01"), xmax=as.Date("2009-06-01"), ymin=0, ymax=Inf, fill="#cecece", alpha=0.2) +
  geom_rect(xmin=as.Date("2020-02-01"), xmax=as.Date("2020-04-01"), ymin=0, ymax=Inf, fill="#cecece", alpha=0.2) +
  geom_rect(xmin=as.Date("2001-03-01"), xmax=as.Date("2001-11-01"), ymin=0, ymax=Inf, fill="#cecece", alpha=0.2) +
  geom_line(aes(color = variable), size=1) +
  scale_color_manual(values = c( "#003366", "#B22234")) +
  theme_bw() +
  theme(text = element_text(size = 15), legend.position = c(.2, .9) ,legend.title=element_blank(),legend.background=element_rect(fill = alpha("white", 0))) +
  scale_x_date(limits = c(as.Date("2001-01-01"), as.Date("2022-01-01")))
openings_and_unemployed

ggsave("openings_unemployed_st.png",
       plot = openings_and_unemployed,
       device = "png",
       width = 12,
       height = 10,
       units= "in")




