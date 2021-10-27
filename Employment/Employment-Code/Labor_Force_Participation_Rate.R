# set FRED API key
API = Sys.getenv("API_key")
fredr_set_key(API)
# load required packages
library(dplyr)
library(fredr)
library(ggplot2)
library(tidyverse)


CIVPART<-fredr(series_id = "CIVPART", observation_start = as.Date("2001-01-01"))

CIVPART_static<-ggplot(CIVPART, aes(x= date, y=value)) + 
  labs(x = "Date", y="Percent", title = "Figure 2: Labor Force Participation Rate", caption= "data from BLS, \n retrieved from FRED (https://fred.stlouisfed.org/)") +
  geom_rect(xmin=as.Date("2007-12-01"), xmax=as.Date("2009-06-01"), ymin=0, ymax=Inf, fill="#cecece", alpha=0.2) +
  geom_rect(xmin=as.Date("2020-02-01"), xmax=as.Date("2020-04-01"), ymin=0, ymax=Inf, fill="#cecece", alpha=0.2) +
  geom_rect(xmin=as.Date("2001-03-01"), xmax=as.Date("2001-11-01"), ymin=0, ymax=Inf, fill="#cecece", alpha=0.2) +
  geom_line(colour= "#B22234") +
  theme_bw() +
  theme(text = element_text(size = 15), legend.position = c(.25, .9) ,legend.title=element_blank(),legend.background=element_rect(fill = alpha("white", 0))) 

CIVPART_static

ggsave("LFPR_st.png",
       plot = CIVPART_static,
       device = "png",
       width = 12,
       height = 10,
       units= "in")


