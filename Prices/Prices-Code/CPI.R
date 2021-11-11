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
fredr_set_key("aae3658b653ba18c15305b7b43fcdb08")

# Consumer Price Index on All Items
CPIAUCSL <- fredr("CPIAUCSL", observation_start = as.Date("1990-01-01"))
CPIAUCSL_dt<-as.data.table(CPIAUCSL[,c(1,3)])
CPIAUSCL_change<-CPIAUCSL_dt[,"CPI All Items":=  (value/shift(value, n=12, fill = NA)-1)*100]
CPIAUSCL_change<-CPIAUSCL_change[,c(1,3)]

# Consumer Price Index Excluding Food and Energy
CPILFESL <- fredr("CPILFESL", observation_start = as.Date("1990-01-01"))
CPILFESL_dt <- as.data.table(CPILFESL[,c(1,3)])
CPILFESL_change <- CPILFESL_dt[,"Excluding Food and Energy":=  (value/shift(value, n=12, fill = NA)-1)*100]
CPILFESL_change<-CPILFESL_change[,c(1,3)]

#combine
dt<-list(CPIAUSCL_change, CPILFESL_change) %>% reduce(left_join, by= "date")

#Static CPI graph
staticdt <- dt %>%
  select(date, `CPI All Items`, `Excluding Food and Energy`) %>%
  gather(key = "variable", value = "value", -date)

CPI_static_graph= ggplot(staticdt, aes(x = date, y = value))+
  geom_rect(xmin=as.Date("1990-07-01"), xmax=as.Date("1991-03-01"), ymin=-5, ymax=Inf, fill="#cecece", alpha=0.01)+
  geom_rect(xmin=as.Date("2001-03-01"), xmax=as.Date("2001-11-01"), ymin=-5, ymax=Inf, fill="#cecece", alpha=0.01)+
  geom_rect(xmin=as.Date("2007-12-01"), xmax=as.Date("2009-06-01"), ymin=-5, ymax=Inf, fill="#cecece", alpha=0.01)+
  geom_rect(xmin=as.Date("2020-02-01"), xmax=as.Date("2020-04-01"), ymin=-5, ymax=Inf, fill="#cecece", alpha=0.01)+
  geom_path(aes(color = variable), size = .9)+
  ylim(-3,8)+
  theme_bw()+
  labs(x ="Date", y = "Percent Change from a Year Ago", title = "Figure 1: Consumer Price Index Inflation",
       caption = "Consumer price index data from U.S. Bureau of Labor Statistics.\n All data retrieved from FRED. (https://fred.stlouisfed.org/)")+
  theme(legend.position = c(.2,.87),legend.text = element_text(size=12), legend.title=element_blank(),legend.background=element_rect(fill = alpha("white", 0)))+
  scale_color_manual(values=c("#B22234", "#003366"))


CPI_static_graph
ggsave("CPI_static_graph.png",
       plot = CPI_static_graph,
       device = "png",
       width = 12,
       height = 10,
       units= "in")

