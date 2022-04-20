library(fredr)
library(ggplot2)
library(ggcorrplot)
library(tidyr)
library(dplyr)
library(tidyverse)

API = Sys.getenv("API_key")
fredr_set_key(API)
##Upload Data
#SC Unrate 
scunrate<- fredr(series_id= "SCUR", observation_start = as.Date("2011-01-01"))
scunrate<-scunrate[,c(1,3)]
colnames(scunrate)<- c("date","South Carolina")

#Abbevile County Unrate
abbeville_unrate<- fredr(series_id= "SCABBE1URN", observation_start = as.Date("2011-01-01"))
abbeville_unrate<-abbeville_unrate[,c(1,3)]
colnames(abbeville_unrate)<-c("date","Abbeville")
dt_abbeville<-list(abbeville_unrate, scunrate) %>% reduce(left_join, by ="date")%>%
  select(date, `South Carolina`, `Abbeville`) %>%
  gather(key = "variable", value = "value", -date)

#PLOT together
plot_SC_Abbeville_Unrate<- ggplot(dt_abbeville, aes(x=date, y=value)) +
  labs(title = "Abbeville County, SC Unemployment Rate", x="Date", 
       y="Rate", caption ="Data from Fred.") + 
  geom_path(aes(color = variable), size=0.8)+
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") + 
  scale_y_continuous(labels = scales::label_percent(scale = 1,suffix ="%"),limits = c(0.5,21)) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.text=element_text(size=14), legend.position=c(.15,.1), legend.title=element_blank())+
    scale_color_manual(values=c("#B22234", "#003366"))

plot_SC_Abbeville_Unrate
ggsave("Abbevile_SC_UNRATE.png",
       device="png")

#Aiken County, SC Unrate
aiken_unrate<- fredr(series_id= "SCAIKE0URN", observation_start = as.Date("2011-01-01"))
aiken_unrate<-aiken_unrate[,c(1,3)]
colnames(aiken_unrate)<-c("date","Aiken")
dt_aiken<-list(aiken_unrate, scunrate) %>% reduce(left_join, by ="date")%>%
  select(date, `South Carolina`, `Aiken`) %>%
  gather(key = "variable", value = "value", -date)

#Plot
plot_SC_Aiken_Unrate<- ggplot(dt_aiken, aes(x=date, y=value)) +
  labs(title = "Aiken County, SC Unemployment Rate", x="Date", 
       y="Rate", caption ="Data from Fred.") + 
  geom_path(aes(color = variable), size=0.8)+
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") + 
  scale_y_continuous(labels = scales::label_percent(scale = 1,suffix ="%"),limits = c(0.5,21)) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.text=element_text(size=14), legend.position=c(.15,.1), legend.title=element_blank())+
  scale_color_manual(values=c("#B22234", "#003366"))
plot_SC_Aiken_Unrate

ggsave("Aiken_SC_UNRATE.png",
       device="png")

#Allendale County, SC Unrate
allendale_unrate<- fredr(series_id= "SCALLE5URN", observation_start = as.Date("2011-01-01"))
allendale_unrate<-allendale_unrate[,c(1,3)]
colnames(allendale_unrate)<-c("date","Allendale")
dt_allendale<-list(allendale_unrate, scunrate) %>% reduce(left_join, by ="date")%>%
  select(date, `South Carolina`, `Allendale`) %>%
  gather(key = "variable", value = "value", -date)

#Plot 
plot_SC_Allendale_Unrate<- ggplot(dt_allendale, aes(x=date, y=value)) +
  labs(title = "Allendale County, SC Unemployment Rate", x="Date", 
       y="Rate", caption ="Data from Fred.") + 
  geom_path(aes(color = variable), size=0.8)+
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") + 
  scale_y_continuous(labels = scales::label_percent(scale = 1,suffix ="%"),limits = c(0.5,21)) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.text=element_text(size=14), legend.position=c(.15,.1), legend.title=element_blank())+
  scale_color_manual(values=c("#B22234", "#003366"))
plot_SC_Allendale_Unrate

ggsave("Allendale_SC_UNRATE.png",
       device="png")

#Anderson County, SC Unrate
anderson_unrate<- fredr(series_id= "SCANDE7URN", observation_start = as.Date("2011-01-01"))
anderson_unrate<-anderson_unrate[,c(1,3)]
colnames(anderson_unrate)<-c("date","Anderson")
dt_anderson<-list(anderson_unrate, scunrate) %>% reduce(left_join, by ="date")%>%
  select(date, `South Carolina`, `Anderson`) %>%
  gather(key = "variable", value = "value", -date)

#Plot Together
plot_SC_Anderson_Unrate<- ggplot(dt_anderson, aes(x=date, y=value)) +
  labs(title = "Anderson County, SC Unemployment Rate", x="Date", 
       y="Rate", caption ="Data from Fred.") + 
  geom_path(aes(color = variable), size=0.8)+
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") + 
  scale_y_continuous(labels = scales::label_percent(scale = 1,suffix ="%"),limits = c(0.5,21)) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.text=element_text(size=14), legend.position=c(.15,.1), legend.title=element_blank())+
  scale_color_manual(values=c("#B22234", "#003366"))
plot_SC_Anderson_Unrate

ggsave("Anderson_SC_UNRATE.png",
       device="png")

#Bamberg County, SC Unrate
bamberg_unrate<- fredr(series_id= "SCBAMB9URN", observation_start = as.Date("2011-01-01"))
bamberg_unrate<-bamberg_unrate[,c(1,3)]
colnames(bamberg_unrate)<-c("date","Bamberg")
dt_bamberg<-list(bamberg_unrate, scunrate) %>% reduce(left_join, by ="date")%>%
  select(date, `South Carolina`, `Bamberg`) %>%
  gather(key = "variable", value = "value", -date)

#Plot Together
plot_SC_Bamberg_Unrate<-ggplot(dt_bamberg, aes(x=date, y=value)) +
  labs(title = "Bamberg County, SC Unemployment Rate", x="Date", 
       y="Rate", caption ="Data from Fred.") + 
  geom_path(aes(color = variable), size=0.8)+
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") + 
  scale_y_continuous(labels = scales::label_percent(scale = 1,suffix ="%"),limits = c(0.5,21)) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.text=element_text(size=14), legend.position=c(.15,.1), legend.title=element_blank())+
  scale_color_manual(values=c("#B22234", "#003366"))
plot_SC_Bamberg_Unrate

ggsave("Bamberg_SC_UNRATE.png",
       device="png")

#Barnwell County, SC Unrate
barnwell_unrate<- fredr(series_id= "SCBARN1URN", observation_start = as.Date("2011-01-01"))
barnwell_unrate<-barnwell_unrate[,c(1,3)]
colnames(barnwell_unrate)<-c("date","Barnwell")
dt_barnwell<-list(barnwell_unrate, scunrate) %>% reduce(left_join, by ="date")%>%
  select(date, `South Carolina`, `Barnwell`) %>%
  gather(key = "variable", value = "value", -date)

#Plot Together
plot_SC_Barnwell_Unrate<- ggplot(dt_barnwell, aes(x=date, y=value)) +
  labs(title = "Barnwell County, SC Unemployment Rate", x="Date", 
       y="Rate", caption ="Data from Fred.") + 
  geom_path(aes(color = variable), size=0.8)+
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") + 
  scale_y_continuous(labels = scales::label_percent(scale = 1,suffix ="%"),limits = c(0.5,21)) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.text=element_text(size=14), legend.position=c(.15,.1), legend.title=element_blank())+
  scale_color_manual(values=c("#B22234", "#003366"))
plot_SC_Barnwell_Unrate

ggsave("Barnwell_SC_UNRATE.png",
       device="png")

#Beaufort County, SC Unrate
beaufort_unrate<- fredr(series_id= "SCBEAU5URN", observation_start = as.Date("2011-01-01"))
beaufort_unrate<-beaufort_unrate[,c(1,3)]
colnames(beaufort_unrate)<-c("date","Beaufort")
dt_beaufort<-list(beaufort_unrate, scunrate) %>% reduce(left_join, by ="date")%>%
  select(date, `South Carolina`, `Beaufort`) %>%
  gather(key = "variable", value = "value", -date)

#Plot Together
plot_SC_Beaufort_Unrate<- ggplot(dt_beaufort, aes(x=date, y=value)) +
  labs(title = "Beaufort County, SC Unemployment Rate", x="Date", 
       y="Rate", caption ="Data from Fred.") + 
  geom_path(aes(color = variable), size=0.8)+
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") + 
  scale_y_continuous(labels = scales::label_percent(scale = 1,suffix ="%"),limits = c(0.5,21)) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.text=element_text(size=14), legend.position=c(.15,.1), legend.title=element_blank())+
  scale_color_manual(values=c("#B22234", "#003366"))
plot_SC_Beaufort_Unrate

ggsave("Beaufort_SC_UNRATE.png",
       device="png")

#Berkeley County, SC Unrate
berkeley_unrate<- fredr(series_id= "SCBERK0URN", observation_start = as.Date("2011-01-01"))
berkeley_unrate<-berkeley_unrate[,c(1,3)]
colnames(berkeley_unrate)<-c("date","Berkeley")
dt_berkeley<-list(berkeley_unrate, scunrate) %>% reduce(left_join, by ="date")%>%
  select(date, `South Carolina`, `Berkeley`) %>%
  gather(key = "variable", value = "value", -date)

#Plot Together
plot_SC_Berkeley_Unrate<- ggplot(dt_berkeley, aes(x=date, y=value)) +
  labs(title = "Berkeley County, SC Unemployment Rate", x="Date", 
       y="Rate", caption ="Data from Fred.") + 
  geom_path(aes(color = variable), size=0.8)+
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") + 
  scale_y_continuous(labels = scales::label_percent(scale = 1,suffix ="%"),limits = c(0.5,21)) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.text=element_text(size=14), legend.position=c(.15,.1), legend.title=element_blank())+
  scale_color_manual(values=c("#B22234", "#003366"))
plot_SC_Berkeley_Unrate

ggsave("Berkeley_SC_UNRATE.png",
       device="png")

#Calhoun County, SC Unrate
calhoun_unrate<- fredr(series_id= "SCCALH7URN", observation_start = as.Date("2011-01-01"))
calhoun_unrate<-calhoun_unrate[,c(1,3)]
colnames(calhoun_unrate)<-c("date","Calhoun")
dt_calhoun<-list(calhoun_unrate, scunrate) %>% reduce(left_join, by ="date")%>%
  select(date, `South Carolina`, `Calhoun`) %>%
  gather(key = "variable", value = "value", -date)


#Plot Together
plot_SC_Calhoun_Unrate<- ggplot(dt_calhoun, aes(x=date, y=value)) +
  labs(title = "Calhoun County, SC Unemployment Rate", x="Date", 
       y="Rate", caption ="Data from Fred.") + 
  geom_path(aes(color = variable), size=0.8)+
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") + 
  scale_y_continuous(labels = scales::label_percent(scale = 1,suffix ="%"),limits = c(0.5,21)) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.text=element_text(size=14), legend.position=c(.15,.1), legend.title=element_blank())+
  scale_color_manual(values=c("#B22234", "#003366"))
plot_SC_Calhoun_Unrate

ggsave("Calhoun_SC_UNRATE.png",
       device="png")

#Charleston County, SC Unrate
charleston_unrate<- fredr(series_id= "SCCHAR9URN", observation_start = as.Date("2011-01-01"))
charleston_unrate<-charleston_unrate[,c(1,3)]
colnames(charleston_unrate)<-c("date","Charleston")
dt_charleston<-list(charleston_unrate, scunrate) %>% reduce(left_join, by ="date")%>%
  select(date, `South Carolina`, `Charleston`) %>%
  gather(key = "variable", value = "value", -date)

#Plot Together
plot_SC_Charleston_Unrate<- ggplot(dt_charleston, aes(x=date, y=value)) +
  labs(title = "Charleston County, SC Unemployment Rate", x="Date", 
       y="Rate", caption ="Data from Fred.") + 
  geom_path(aes(color = variable), size=0.8)+
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") + 
  scale_y_continuous(labels = scales::label_percent(scale = 1,suffix ="%"),limits = c(0.5,21)) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.text=element_text(size=14), legend.position=c(.15,.1), legend.title=element_blank())+
  scale_color_manual(values=c("#B22234", "#003366"))
plot_SC_Charleston_Unrate

ggsave("Charleston_SC_UNRATE.png",
       device="png")

#Cherokee County, SC Unrate
cherokee_unrate<- fredr(series_id= "SCCHER1URN", observation_start = as.Date("2011-01-01"))
cherokee_unrate<-cherokee_unrate[,c(1,3)]
colnames(cherokee_unrate)<-c("date","Cherokee")
dt_cherokee<-list(cherokee_unrate, scunrate) %>% reduce(left_join, by ="date")%>%
  select(date, `South Carolina`, `Cherokee`) %>%
  gather(key = "variable", value = "value", -date)

#Plot Together
plot_SC_Cherokee_Unrate<- ggplot(dt_cherokee, aes(x=date, y=value)) +
  labs(title = "Cherokee County, SC Unemployment Rate", x="Date", 
       y="Rate", caption ="Data from Fred.") + 
  geom_path(aes(color = variable), size=0.8)+
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") + 
  scale_y_continuous(labels = scales::label_percent(scale = 1,suffix ="%"),limits = c(0.5,21)) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.text=element_text(size=14), legend.position=c(.15,.1), legend.title=element_blank())+
  scale_color_manual(values=c("#B22234", "#003366"))
plot_SC_Cherokee_Unrate

ggsave("Cherokee_SC_UNRATE.png",
       device="png")

#Chester County, SC Unrate
chester_unrate<- fredr(series_id= "SCCHES3URN", observation_start = as.Date("2011-01-01"))
chester_unrate<-chester_unrate[,c(1,3)]
colnames(chester_unrate)<-c("date","Chester")
dt_chester<-list(chester_unrate, scunrate) %>% reduce(left_join, by ="date")%>%
  select(date, `South Carolina`, `Chester`) %>%
  gather(key = "variable", value = "value", -date)


#Plot Together
plot_SC_Chester_Unrate<- ggplot(dt_chester, aes(x=date, y=value)) +
  labs(title = "Chester County, SC Unemployment Rate", x="Date", 
       y="Rate", caption ="Data from Fred.") + 
  geom_path(aes(color = variable), size=0.8)+
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") + 
  scale_y_continuous(labels = scales::label_percent(scale = 1,suffix ="%"),limits = c(0.5,21)) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.text=element_text(size=14), legend.position=c(.15,.1), legend.title=element_blank())+
  scale_color_manual(values=c("#B22234", "#003366"))
plot_SC_Chester_Unrate

ggsave("Chester_SC_UNRATE.png",
       device="png")

#Chesterfield County, SC Unrate
chesterfield_unrate<- fredr(series_id= "SCCHES5URN", observation_start = as.Date("2011-01-01"))
chesterfield_unrate<-chesterfield_unrate[,c(1,3)]
colnames(chesterfield_unrate)<-c("date","Chesterfield")
dt_chesterfield<-list(chesterfield_unrate, scunrate) %>% reduce(left_join, by ="date")%>%
  select(date, `South Carolina`, `Chesterfield`) %>%
  gather(key = "variable", value = "value", -date)

#Plot Together
plot_SC_Chesterfield_Unrate<- ggplot(dt_chesterfield, aes(x=date, y=value)) +
  labs(title = "Chesterfield County, SC Unemployment Rate", x="Date", 
       y="Rate", caption ="Data from Fred.") + 
  geom_path(aes(color = variable), size=0.8)+
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") + 
  scale_y_continuous(labels = scales::label_percent(scale = 1,suffix ="%"),limits = c(0.5,21)) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.text=element_text(size=14), legend.position=c(.15,.1), legend.title=element_blank())+
  scale_color_manual(values=c("#B22234", "#003366"))
plot_SC_Chesterfield_Unrate

ggsave("Chesterfield_SC_UNRATE.png",
       device="png")

#Clarendon County, SC Unrate
clarendon_unrate<- fredr(series_id= "SCCLAR7URN", observation_start = as.Date("2011-01-01"))
clarendon_unrate<-clarendon_unrate[,c(1,3)]
colnames(clarendon_unrate)<-c("date","Clarendon")
dt_clarendon<-list(clarendon_unrate, scunrate) %>% reduce(left_join, by ="date")%>%
  select(date, `South Carolina`, `Clarendon`) %>%
  gather(key = "variable", value = "value", -date)

#Plot Together
plot_SC_Clarendon_Unrate<- ggplot(dt_clarendon, aes(x=date, y=value)) +
  labs(title = "Clarendon County, SC Unemployment Rate", x="Date", 
       y="Rate", caption ="Data from Fred.") + 
  geom_path(aes(color = variable), size=0.8)+
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") + 
  scale_y_continuous(labels = scales::label_percent(scale = 1,suffix ="%"),limits = c(0.5,21)) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.text=element_text(size=14), legend.position=c(.15,.1), legend.title=element_blank())+
  scale_color_manual(values=c("#B22234", "#003366"))
plot_SC_Clarendon_Unrate

ggsave("Clarendon_SC_UNRATE.png",
       device="png")

#Colleton County, SC Unrate
colleton_unrate<- fredr(series_id= "SCCOLL9URN", observation_start = as.Date("2011-01-01"))
colleton_unrate<-colleton_unrate[,c(1,3)]
colnames(colleton_unrate)<-c("date","Colleton")
dt_colleton<-list(colleton_unrate, scunrate) %>% reduce(left_join, by ="date")%>%
  select(date, `South Carolina`, `Colleton`) %>%
  gather(key = "variable", value = "value", -date)

#Plot Together
plot_SC_Colleton_Unrate<- ggplot(dt_colleton, aes(x=date, y=value)) +
  labs(title = "Colleton County, SC Unemployment Rate", x="Date", 
       y="Rate", caption ="Data from Fred.") + 
  geom_path(aes(color = variable), size=0.8)+
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") + 
  scale_y_continuous(labels = scales::label_percent(scale = 1,suffix ="%"),limits = c(0.5,21)) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.text=element_text(size=14), legend.position=c(.15,.1), legend.title=element_blank())+
  scale_color_manual(values=c("#B22234", "#003366"))
plot_SC_Colleton_Unrate

ggsave("Colleton_SC_UNRATE.png",
       device="png")

#Darlington County, SC Unrate
darlington_unrate<- fredr(series_id= "SCDARL5URN", observation_start = as.Date("2011-01-01"))
darlington_unrate<-darlington_unrate[,c(1,3)]
colnames(darlington_unrate)<-c("date","Darlington")
dt_darlington<-list(darlington_unrate, scunrate) %>% reduce(left_join, by ="date")%>%
  select(date, `South Carolina`, `Darlington`) %>%
  gather(key = "variable", value = "value", -date)

#Plot Together
plot_SC_Darlington_Unrate<- ggplot(dt_darlington, aes(x=date, y=value)) +
  labs(title = "Darlington County, SC Unemployment Rate", x="Date", 
       y="Rate", caption ="Data from Fred.") + 
  geom_path(aes(color = variable), size=0.8)+
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") + 
  scale_y_continuous(labels = scales::label_percent(scale = 1,suffix ="%"),limits = c(0.5,21)) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.text=element_text(size=14), legend.position=c(.15,.1), legend.title=element_blank())+
  scale_color_manual(values=c("#B22234", "#003366"))
plot_SC_Darlington_Unrate

ggsave("Darlington_SC_UNRATE.png",
       device="png")

#Dillon County, SC Unrate
dillon_unrate<- fredr(series_id= "SCDILL3URN", observation_start = as.Date("2011-01-01"))
dillon_unrate<-dillon_unrate[,c(1,3)]
colnames(dillon_unrate)<-c("date","Dillon")
dt_dillon<-list(dillon_unrate, scunrate) %>% reduce(left_join, by ="date")%>%
  select(date, `South Carolina`, `Dillon`) %>%
  gather(key = "variable", value = "value", -date)

#Plot Together
plot_SC_Dillon_Unrate<-ggplot(dt_dillon, aes(x=date, y=value)) +
  labs(title = "Dillon County, SC Unemployment Rate", x="Date", 
       y="Rate", caption ="Data from Fred.") + 
  geom_path(aes(color = variable), size=0.8)+
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") + 
  scale_y_continuous(labels = scales::label_percent(scale = 1,suffix ="%"),limits = c(0.5,21)) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.text=element_text(size=14), legend.position=c(.15,.1), legend.title=element_blank())+
  scale_color_manual(values=c("#B22234", "#003366"))
plot_SC_Dillon_Unrate

ggsave("Dillon_SC_UNRATE.png",
       device="png")

#Dorchester County, SC Unrate
dorchester_unrate<- fredr(series_id= "SCDORC7URN", observation_start = as.Date("2011-01-01"))
dorchester_unrate<-dorchester_unrate[,c(1,3)]
colnames(dorchester_unrate)<-c("date","Dorchester")
dt_dorchester<-list(dorchester_unrate, scunrate) %>% reduce(left_join, by ="date")%>%
  select(date, `South Carolina`, `Dorchester`) %>%
  gather(key = "variable", value = "value", -date)

#Plot Together
plot_SC_Dorchester_Unrate<- ggplot(dt_dorchester, aes(x=date, y=value)) +
  labs(title = "Dorchester County, SC Unemployment Rate", x="Date", 
       y="Rate", caption ="Data from Fred.") + 
  geom_path(aes(color = variable), size=0.8)+
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") + 
  scale_y_continuous(labels = scales::label_percent(scale = 1,suffix ="%"),limits = c(0.5,21)) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.text=element_text(size=14), legend.position=c(.15,.1), legend.title=element_blank())+
  scale_color_manual(values=c("#B22234", "#003366"))
plot_SC_Dorchester_Unrate

ggsave("Dorchester_SC_UNRATE.png",
       device="png")

#Edgefield County, SC Unrate
edgefield_unrate<- fredr(series_id= "SCEDGE7URN", observation_start = as.Date("2011-01-01"))
edgefield_unrate<-edgefield_unrate[,c(1,3)]
colnames(edgefield_unrate)<-c("date","Edgefield")
dt_edgefield<-list(edgefield_unrate, scunrate) %>% reduce(left_join, by ="date")%>%
  select(date, `South Carolina`, `Edgefield`) %>%
  gather(key = "variable", value = "value", -date)

#Plot Together
plot_SC_Edgefield_Unrate<- ggplot(dt_edgefield, aes(x=date, y=value)) +
  labs(title = "Edgefield County, SC Unemployment Rate", x="Date", 
       y="Rate", caption ="Data from Fred.") + 
  geom_path(aes(color = variable), size=0.8)+
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") + 
  scale_y_continuous(labels = scales::label_percent(scale = 1,suffix ="%"),limits = c(0.5,21)) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.text=element_text(size=14), legend.position=c(.15,.1), legend.title=element_blank())+
  scale_color_manual(values=c("#B22234", "#003366"))
plot_SC_Edgefield_Unrate

ggsave("Edgefield_SC_UNRATE.png",
       device="png")

#Fairfield County, SC Unrate
fairfield_unrate<- fredr(series_id= "SCFAIR9URN", observation_start = as.Date("2011-01-01"))
fairfield_unrate<-fairfield_unrate[,c(1,3)]
colnames(fairfield_unrate)<-c("date","Fairfield")
dt_fairfield<-list(fairfield_unrate, scunrate) %>% reduce(left_join, by ="date")%>%
  select(date, `South Carolina`, `Fairfield`) %>%
  gather(key = "variable", value = "value", -date)

#Plot Together
plot_SC_Fairfield_Unrate<- ggplot(dt_fairfield, aes(x=date, y=value)) +
  labs(title = "Fairfield County, SC Unemployment Rate", x="Date", 
       y="Rate", caption ="Data from Fred.") + 
  geom_path(aes(color = variable), size=0.8)+
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") + 
  scale_y_continuous(labels = scales::label_percent(scale = 1,suffix ="%"),limits = c(0.5,21)) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.text=element_text(size=14), legend.position=c(.15,.1), legend.title=element_blank())+
  scale_color_manual(values=c("#B22234", "#003366"))
plot_SC_Fairfield_Unrate

ggsave("Fairfield_SC_UNRATE.png",
       device="png")

#Florence County, SC Unrate
florence_unrate<- fredr(series_id= "SCFLOR0URN", observation_start = as.Date("2011-01-01"))
florence_unrate<-florence_unrate[,c(1,3)]
colnames(florence_unrate)<-c("date","Florence")
dt_florence<-list(florence_unrate, scunrate) %>% reduce(left_join, by ="date")%>%
  select(date, `South Carolina`, `Florence`) %>%
  gather(key = "variable", value = "value", -date)

#Plot Together
plot_SC_Florence_Unrate<- ggplot(dt_florence, aes(x=date, y=value)) +
  labs(title = "Florence County, SC Unemployment Rate", x="Date", 
       y="Rate", caption ="Data from Fred.") + 
  geom_path(aes(color = variable), size=0.8)+
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") + 
  scale_y_continuous(labels = scales::label_percent(scale = 1,suffix ="%"),limits = c(0.5,21)) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.text=element_text(size=14), legend.position=c(.15,.1), legend.title=element_blank())+
  scale_color_manual(values=c("#B22234", "#003366"))
plot_SC_Florence_Unrate

ggsave("Florence_SC_UNRATE.png",
       device="png")

#Georgetown County, SC Unrate
georgetown_unrate<- fredr(series_id= "SCGEOR3URN", observation_start = as.Date("2011-01-01"))
georgetown_unrate<-georgetown_unrate[,c(1,3)]
colnames(georgetown_unrate)<-c("date","Georgetown")
dt_georgetown<-list(georgetown_unrate, scunrate) %>% reduce(left_join, by ="date")%>%
  select(date, `South Carolina`, `Georgetown`) %>%
  gather(key = "variable", value = "value", -date)

#Plot Together
plot_SC_Georgetown_Unrate<- ggplot(dt_georgetown, aes(x=date, y=value)) +
  labs(title = "Georgetown County, SC Unemployment Rate", x="Date", 
       y="Rate", caption ="Data from Fred.") + 
  geom_path(aes(color = variable), size=0.8)+
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") + 
  scale_y_continuous(labels = scales::label_percent(scale = 1,suffix ="%"),limits = c(0.5,21)) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.text=element_text(size=14), legend.position=c(.15,.1), legend.title=element_blank())+
  scale_color_manual(values=c("#B22234", "#003366"))
plot_SC_Georgetown_Unrate

ggsave("Georgetown_SC_UNRATE.png",
       device="png")

#Greenville County, SC Unrate
greenville_unrate<- fredr(series_id= "SCGREE5URN", observation_start = as.Date("2011-01-01"))
greenville_unrate<-greenville_unrate[,c(1,3)]
colnames(greenville_unrate)<-c("date","Greenville")
dt_greenville<-list(greenville_unrate, scunrate) %>% reduce(left_join, by ="date")%>%
  select(date, `South Carolina`, `Greenville`) %>%
  gather(key = "variable", value = "value", -date)

#Plot Together
plot_SC_Greenville_Unrate<- ggplot(dt_greenville, aes(x=date, y=value)) +
  labs(title = "Greenville County, SC Unemployment Rate", x="Date", 
       y="Rate", caption ="Data from Fred.") + 
  geom_path(aes(color = variable), size=0.8)+
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") + 
  scale_y_continuous(labels = scales::label_percent(scale = 1,suffix ="%"),limits = c(0.5,21)) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.text=element_text(size=14), legend.position=c(.15,.1), legend.title=element_blank())+
  scale_color_manual(values=c("#B22234", "#003366"))
plot_SC_Greenville_Unrate

ggsave("Greenville_SC_UNRATE.png",
       device="png")

#Greenwood County Unrate
greenwood_unrate<- fredr(series_id= "SCGREE0URN", observation_start = as.Date("2011-01-01"))
greenwood_unrate<-greenwood_unrate[,c(1,3)]
colnames(greenwood_unrate)<-c("date","Greenwood")
dt_greenwood<-list(greenwood_unrate, scunrate) %>% reduce(left_join, by ="date")%>%
  select(date, `South Carolina`, `Greenwood`) %>%
  gather(key = "variable", value = "value", -date)

#PLOT
plot_SC_Greenwood_Unrate<- ggplot(dt_greenwood, aes(x=date, y=value)) +
  labs(title = "Greenwood County, SC Unemployment Rate", x="Date", 
       y="Rate", caption ="Data from Fred.") + 
  geom_path(aes(color = variable), size=0.8)+
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") + 
  scale_y_continuous(labels = scales::label_percent(scale = 1,suffix ="%"),limits = c(0.5,21)) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.text=element_text(size=14), legend.position=c(.15,.1), legend.title=element_blank())+
  scale_color_manual(values=c("#B22234", "#003366"))
plot_SC_Greenwood_Unrate
#SAVE
ggsave("Greenwood_SC_UNRATE.png",
       device = "png")

#Hampton County Unrate
hampton_unrate<- fredr(series_id= "SCHAMP9URN", observation_start = as.Date("2011-01-01"))
hampton_unrate<-hampton_unrate[,c(1,3)]
colnames(hampton_unrate)<-c("date","Hampton")
dt_hampton<-list(hampton_unrate, scunrate) %>% reduce(left_join, by ="date")%>%
  select(date, `South Carolina`, `Hampton`) %>%
  gather(key = "variable", value = "value", -date)

#PLOT
plot_SC_Hampton_Unrate<- ggplot(dt_hampton, aes(x=date, y=value)) +
  labs(title = "Hampton County, SC Unemployment Rate", x="Date", 
       y="Rate", caption ="Data from Fred.") + 
  geom_path(aes(color = variable), size=0.8)+
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") + 
  scale_y_continuous(labels = scales::label_percent(scale = 1,suffix ="%"),limits = c(0.5,21)) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.text=element_text(size=14), legend.position=c(.15,.1), legend.title=element_blank())+
  scale_color_manual(values=c("#B22234", "#003366"))
plot_SC_Hampton_Unrate

ggsave("Hampton_SC_UNRATE.png",
       device = "png")

#HORRY County Unrate
horry_unrate<- fredr(series_id= "SCHORR1URN", observation_start = as.Date("2011-01-01"))
horry_unrate<-horry_unrate[,c(1,3)]
colnames(horry_unrate)<-c("date","Horry")
dt_horry<-list(horry_unrate, scunrate) %>% reduce(left_join, by ="date")%>%
  select(date, `South Carolina`, `Horry`) %>%
  gather(key = "variable", value = "value", -date)

#PLOT
plot_SC_Horry_Unrate<- ggplot(dt_horry, aes(x=date, y=value)) +
  labs(title = "Horry County, SC Unemployment Rate", x="Date", 
       y="Rate", caption ="Data from Fred.") + 
  geom_path(aes(color = variable), size=0.8)+
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") + 
  scale_y_continuous(labels = scales::label_percent(scale = 1,suffix ="%"),limits = c(0.5,21)) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.text=element_text(size=14), legend.position=c(.15,.1), legend.title=element_blank())+
  scale_color_manual(values=c("#B22234", "#003366"))
plot_SC_Horry_Unrate

ggsave("Horry_SC_UNRATE.png",
       device="png")

#JASPER County Unrate
jasper_unrate<- fredr(series_id= "SCJASP3URN", observation_start = as.Date("2011-01-01"))
jasper_unrate<-jasper_unrate[,c(1,3)]
colnames(jasper_unrate)<-c("date","Jasper")
dt_jasper<-list(jasper_unrate, scunrate) %>% reduce(left_join, by ="date")%>%
  select(date, `South Carolina`, `Jasper`) %>%
  gather(key = "variable", value = "value", -date)

#PLOT
plot_SC_Jasper_Unrate<- ggplot(dt_jasper, aes(x=date, y=value)) +
  labs(title = "Jasper County, SC Unemployment Rate", x="Date", 
       y="Rate", caption ="Data from Fred.") + 
  geom_path(aes(color = variable), size=0.8)+
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") + 
  scale_y_continuous(labels = scales::label_percent(scale = 1,suffix ="%"),limits = c(0.5,21)) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.text=element_text(size=14), legend.position=c(.15,.1), legend.title=element_blank())+
  scale_color_manual(values=c("#B22234", "#003366"))
plot_SC_Jasper_Unrate

ggsave("Jasper_SC_UNRATE.png",
       device="png")


# Kershaw
kershaw_unrate<- fredr(series_id= "SCKERS5URN", observation_start = as.Date("2011-01-01"))
kershaw_unrate<-kershaw_unrate[,c(1,3)]
colnames(kershaw_unrate)<-c("date","Kershaw")
dt_kershaw<-list(kershaw_unrate, scunrate) %>% reduce(left_join, by ="date")%>%
  select(date, `South Carolina`, `Kershaw`) %>%
  gather(key = "variable", value = "value", -date)

#PLOT
plot_SC_Kershaw_Unrate<- ggplot(dt_kershaw, aes(x=date, y=value)) +
  labs(title = "Kershaw County, SC Unemployment Rate", x="Date", 
       y="Rate", caption ="Data from Fred.") + 
  geom_path(aes(color = variable), size=0.8)+
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") + 
  scale_y_continuous(labels = scales::label_percent(scale = 1,suffix ="%"),limits = c(0.5,21)) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.text=element_text(size=14), legend.position=c(.15,.1), legend.title=element_blank())+
  scale_color_manual(values=c("#B22234", "#003366"))
plot_SC_Kershaw_Unrate

ggsave("Kershaw_SC_UNRATE.png",
       device="png")

lancaster_unrate<- fredr(series_id= "SCLANC7URN", observation_start = as.Date("2011-01-01"))
lancaster_unrate<-lancaster_unrate[,c(1,3)]
colnames(lancaster_unrate)<-c("date","Lancaster")
dt_lancaster<-list(lancaster_unrate, scunrate) %>% reduce(left_join, by ="date")%>%
  select(date, `South Carolina`, `Lancaster`) %>%
  gather(key = "variable", value = "value", -date)

#PLOT
plot_SC_Lancaster_Unrate<- ggplot(dt_lancaster, aes(x=date, y=value)) +
  labs(title = "Lancaster County, SC Unemployment Rate", x="Date", 
       y="Rate", caption ="Data from Fred.") + 
  geom_path(aes(color = variable), size=0.8)+
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") + 
  scale_y_continuous(labels = scales::label_percent(scale = 1,suffix ="%"),limits = c(0.5,21)) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.text=element_text(size=14), legend.position=c(.15,.1), legend.title=element_blank())+
  scale_color_manual(values=c("#B22234", "#003366"))
plot_SC_Lancaster_Unrate

ggsave("Lancaster_SC_UNRATE.png",
       device="png")

#Laurens County
laurens_unrate<- fredr(series_id= "SCLAUR5URN", observation_start = as.Date("2011-01-01"))
laurens_unrate<-laurens_unrate[,c(1,3)]
colnames(laurens_unrate)<-c("date","Laurens")
dt_laurens<-list(laurens_unrate, scunrate) %>% reduce(left_join, by ="date")%>%
  select(date, `South Carolina`, `Laurens`) %>%
  gather(key = "variable", value = "value", -date)

#PLOT
plot_SC_Laurens_Unrate<- ggplot(dt_laurens, aes(x=date, y=value)) +
  labs(title = "Laurens County, SC Unemployment Rate", x="Date", 
       y="Rate", caption ="Data from Fred.") + 
  geom_path(aes(color = variable), size=0.8)+
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") + 
  scale_y_continuous(labels = scales::label_percent(scale = 1,suffix ="%"),limits = c(0.5,21)) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.text=element_text(size=14), legend.position=c(.15,.1), legend.title=element_blank())+
  scale_color_manual(values=c("#B22234", "#003366"))
plot_SC_Laurens_Unrate

ggsave("Laurens_SC_UNRATE.png",
       device="png")

lee_unrate<- fredr(series_id= "SCLEEC1URN", observation_start = as.Date("2011-01-01"))
lee_unrate<-lee_unrate[,c(1,3)]
colnames(lee_unrate)<-c("date","Lee")
dt_lee<-list(lee_unrate, scunrate) %>% reduce(left_join, by ="date")%>%
  select(date, `South Carolina`, `Lee`) %>%
  gather(key = "variable", value = "value", -date)

#PLOT
plot_SC_Lee_Unrate<- ggplot(dt_lee, aes(x=date, y=value)) +
  labs(title = "Lee County, SC Unemployment Rate", x="Date", 
       y="Rate", caption ="Data from Fred.") + 
  geom_path(aes(color = variable), size=0.8)+
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") + 
  scale_y_continuous(labels = scales::label_percent(scale = 1,suffix ="%"),limits = c(0.5,21)) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.text=element_text(size=14), legend.position=c(.15,.1), legend.title=element_blank())+
  scale_color_manual(values=c("#B22234", "#003366"))
plot_SC_Lee_Unrate

ggsave("Lee_SC_UNRATE.png",
       device="png")

lexington_unrate<- fredr(series_id= "SCLEXI0URN", observation_start = as.Date("2011-01-01"))
lexington_unrate<-lexington_unrate[,c(1,3)]
colnames(lexington_unrate)<-c("date","Lexington")
dt_lexington<-list(lexington_unrate, scunrate) %>% reduce(left_join, by ="date")%>%
  select(date, `South Carolina`, `Lexington`) %>%
  gather(key = "variable", value = "value", -date)

#PLOT
plot_SC_Lexington_Unrate<- ggplot(dt_lexington, aes(x=date, y=value)) +
  labs(title = "Lexington County, SC Unemployment Rate", x="Date", 
       y="Rate", caption ="Data from Fred.") + 
  geom_path(aes(color = variable), size=0.8)+
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") + 
  scale_y_continuous(labels = scales::label_percent(scale = 1,suffix ="%"),limits = c(0.5,21)) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.text=element_text(size=14), legend.position=c(.15,.1), legend.title=element_blank())+
  scale_color_manual(values=c("#B22234", "#003366"))
plot_SC_Lexington_Unrate

ggsave("Lexington_SC_UNRATE.png",
       device="png")

marion_unrate<- fredr(series_id= "SCMARI7URN", observation_start = as.Date("2011-01-01"))
marion_unrate<-marion_unrate[,c(1,3)]
colnames(marion_unrate)<-c("date","Marion")
dt_marion<-list(marion_unrate, scunrate) %>% reduce(left_join, by ="date")%>%
  select(date, `South Carolina`, `Marion`) %>%
  gather(key = "variable", value = "value", -date)

#PLOT
plot_SC_Marion_Unrate<- ggplot(dt_marion, aes(x=date, y=value)) +
  labs(title = "Marion County, SC Unemployment Rate", x="Date", 
       y="Rate", caption ="Data from Fred.") + 
  geom_path(aes(color = variable), size=0.8)+
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") + 
  scale_y_continuous(labels = scales::label_percent(scale = 1,suffix ="%"),limits = c(0.5,21)) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.text=element_text(size=14), legend.position=c(.15,.1), legend.title=element_blank())+
  scale_color_manual(values=c("#B22234", "#003366"))
plot_SC_Marion_Unrate

ggsave("Marion_SC_UNRATE.png",
       device="png")

marlboro_unrate<- fredr(series_id= "SCMARL9URN", observation_start = as.Date("2011-01-01"))
marlboro_unrate<-marlboro_unrate[,c(1,3)]
colnames(marlboro_unrate)<-c("date","Marlboro")
dt_marlboro<-list(marlboro_unrate, scunrate) %>% reduce(left_join, by ="date")%>%
  select(date, `South Carolina`, `Marlboro`) %>%
  gather(key = "variable", value = "value", -date)

#PLOT
plot_SC_Marlboro_Unrate<- ggplot(dt_marlboro, aes(x=date, y=value)) +
  labs(title = "Marlboro County, SC Unemployment Rate", x="Date", 
       y="Rate", caption ="Data from Fred.") + 
  geom_path(aes(color = variable), size=0.8)+
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") + 
  scale_y_continuous(labels = scales::label_percent(scale = 1,suffix ="%"),limits = c(0.5,21)) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.text=element_text(size=14), legend.position=c(.15,.1), legend.title=element_blank())+
  scale_color_manual(values=c("#B22234", "#003366"))
plot_SC_Marlboro_Unrate

ggsave("Marlboro_SC_UNRATE.png",
       device="png")

mcCormick_unrate<- fredr(series_id= "SCMCCO5URN", observation_start = as.Date("2011-01-01"))
mcCormick_unrate<-mcCormick_unrate[,c(1,3)]
colnames(mcCormick_unrate)<-c("date","McCormick")
dt_mcCormick<-list(mcCormick_unrate, scunrate) %>% reduce(left_join, by ="date")%>%
  select(date, `South Carolina`, `McCormick`) %>%
  gather(key = "variable", value = "value", -date)

#PLOT
plot_SC_McCormick_Unrate<- ggplot(dt_mcCormick, aes(x=date, y=value)) +
  labs(title = "McCormick County, SC Unemployment Rate", x="Date", 
       y="Rate", caption ="Data from Fred.") + 
  geom_path(aes(color = variable), size=0.8)+
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") + 
  scale_y_continuous(labels = scales::label_percent(scale = 1,suffix ="%"),limits = c(0.5,21)) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.text=element_text(size=14), legend.position=c(.15,.1), legend.title=element_blank())+
  scale_color_manual(values=c("#B22234", "#003366"))
plot_SC_McCormick_Unrate

ggsave("McCormick_SC_UNRATE.png",
       device="png")


newberry_unrate<- fredr(series_id= "SCNEWB1URN", observation_start = as.Date("2011-01-01"))
newberry_unrate<-newberry_unrate[,c(1,3)]
colnames(newberry_unrate)<-c("date","Newberry")
dt_newberry<-list(newberry_unrate, scunrate) %>% reduce(left_join, by ="date")%>%
  select(date, `South Carolina`, `Newberry`) %>%
  gather(key = "variable", value = "value", -date)

#PLOT
plot_SC_Newberry_Unrate<- ggplot(dt_newberry, aes(x=date, y=value)) +
  labs(title = "Newberry County, SC Unemployment Rate", x="Date", 
       y="Rate", caption ="Data from Fred.") + 
  geom_path(aes(color = variable), size=0.8)+
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") + 
  scale_y_continuous(labels = scales::label_percent(scale = 1,suffix ="%"),limits = c(0.5,21)) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.text=element_text(size=14), legend.position=c(.15,.1), legend.title=element_blank())+
  scale_color_manual(values=c("#B22234", "#003366"))
plot_SC_Newberry_Unrate

ggsave("Newberry_SC_UNRATE.png",
       device="png")



oconee_unrate<- fredr(series_id= "SCOCON3URN", observation_start = as.Date("2011-01-01"))
oconee_unrate<-oconee_unrate[,c(1,3)]
colnames(oconee_unrate)<-c("date","Oconee")
dt_oconee<-list(oconee_unrate, scunrate) %>% reduce(left_join, by ="date")%>%
  select(date, `South Carolina`, `Oconee`) %>%
  gather(key = "variable", value = "value", -date)

#PLOT
plot_SC_Oconee_Unrate<- ggplot(dt_oconee, aes(x=date, y=value)) +
  labs(title = "Oconee County, SC Unemployment Rate", x="Date", 
       y="Rate", caption ="Data from Fred.") + 
  geom_path(aes(color = variable), size=0.8)+
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") + 
  scale_y_continuous(labels = scales::label_percent(scale = 1,suffix ="%"),limits = c(0.5,21)) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.text=element_text(size=14), legend.position=c(.15,.1), legend.title=element_blank())+
  scale_color_manual(values=c("#B22234", "#003366"))
plot_SC_Oconee_Unrate

ggsave("Oconee_SC_UNRATE.png",
       device="png")


orangeburg_unrate<- fredr(series_id= "SCORAN5URN", observation_start = as.Date("2011-01-01"))
orangeburg_unrate<-orangeburg_unrate[,c(1,3)]
colnames(orangeburg_unrate)<-c("date","Orangeburg")
dt_orangeburg<-list(orangeburg_unrate, scunrate) %>% reduce(left_join, by ="date")%>%
  select(date, `South Carolina`, `Orangeburg`) %>%
  gather(key = "variable", value = "value", -date)

#PLOT
plot_SC_Orangeburg_Unrate<- ggplot(dt_orangeburg, aes(x=date, y=value)) +
  labs(title = "Orangeburg County, SC Unemployment Rate", x="Date", 
       y="Rate", caption ="Data from Fred.") + 
  geom_path(aes(color = variable), size=0.8)+
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") + 
  scale_y_continuous(labels = scales::label_percent(scale = 1,suffix ="%"),limits = c(0.5,21)) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.text=element_text(size=14), legend.position=c(.15,.1), legend.title=element_blank())+
  scale_color_manual(values=c("#B22234", "#003366"))
plot_SC_Orangeburg_Unrate

ggsave("Orangeburg_SC_UNRATE.png",
       device="png")

pickens_unrate<- fredr(series_id= "SCPICK0URN", observation_start = as.Date("2011-01-01"))
pickens_unrate<-pickens_unrate[,c(1,3)]
colnames(pickens_unrate)<-c("date","Pickens")
dt_pickens<-list(pickens_unrate, scunrate) %>% reduce(left_join, by ="date")%>%
  select(date, `South Carolina`, `Pickens`) %>%
  gather(key = "variable", value = "value", -date)

#PLOT
plot_SC_Pickens_Unrate<- ggplot(dt_pickens, aes(x=date, y=value)) +
  labs(title = "Pickens County, SC Unemployment Rate", x="Date", 
       y="Rate", caption ="Data from Fred.") + 
  geom_path(aes(color = variable), size=0.8)+
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") + 
  scale_y_continuous(labels = scales::label_percent(scale = 1,suffix ="%"),limits = c(0.5,21)) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.text=element_text(size=14), legend.position=c(.15,.1), legend.title=element_blank())+
  scale_color_manual(values=c("#B22234", "#003366"))
plot_SC_Pickens_Unrate

ggsave("Pickens_SC_UNRATE.png",
       device="png")


richland_unrate<- fredr(series_id= "SCRICH9URN", observation_start = as.Date("2011-01-01"))
richland_unrate<-richland_unrate[,c(1,3)]
colnames(richland_unrate)<-c("date","Richland")
dt_richland<-list(richland_unrate, scunrate) %>% reduce(left_join, by ="date")%>%
  select(date, `South Carolina`, `Richland`) %>%
  gather(key = "variable", value = "value", -date)

#PLOT
plot_SC_Richland_Unrate<- ggplot(dt_richland, aes(x=date, y=value)) +
  labs(title = "Richland County, SC Unemployment Rate", x="Date", 
       y="Rate", caption ="Data from Fred.") + 
  geom_path(aes(color = variable), size=0.8)+
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") + 
  scale_y_continuous(labels = scales::label_percent(scale = 1,suffix ="%"),limits = c(0.5,21)) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.text=element_text(size=14), legend.position=c(.15,.1), legend.title=element_blank())+
  scale_color_manual(values=c("#B22234", "#003366"))
plot_SC_Richland_Unrate

ggsave("Richland_SC_UNRATE.png",
       device="png")


saluda_unrate<- fredr(series_id= "SCSALU1URN", observation_start = as.Date("2011-01-01"))
saluda_unrate<-saluda_unrate[,c(1,3)]
colnames(saluda_unrate)<-c("date","Saluda")
dt_saluda<-list(saluda_unrate, scunrate) %>% reduce(left_join, by ="date")%>%
  select(date, `South Carolina`, `Saluda`) %>%
  gather(key = "variable", value = "value", -date)

#PLOT
plot_SC_Saluda_Unrate<- ggplot(dt_saluda, aes(x=date, y=value)) +
  labs(title = "Saluda County, SC Unemployment Rate", x="Date", 
       y="Rate", caption ="Data from Fred.") + 
  geom_path(aes(color = variable), size=0.8)+
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") + 
  scale_y_continuous(labels = scales::label_percent(scale = 1,suffix ="%"),limits = c(0.5,21)) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.text=element_text(size=14), legend.position=c(.15,.1), legend.title=element_blank())+
  scale_color_manual(values=c("#B22234", "#003366"))
plot_SC_Saluda_Unrate

ggsave("Saluda_SC_UNRATE.png",
       device="png")

spartanburg_unrate<- fredr(series_id= "SCSPAR0URN", observation_start = as.Date("2011-01-01"))
spartanburg_unrate<-spartanburg_unrate[,c(1,3)]
colnames(spartanburg_unrate)<-c("date","Spartanburg")
dt_spartanburg<-list(spartanburg_unrate, scunrate) %>% reduce(left_join, by ="date")%>%
  select(date, `South Carolina`, `Spartanburg`) %>%
  gather(key = "variable", value = "value", -date)

#PLOT
plot_SC_Spartanburg_Unrate<- ggplot(dt_spartanburg, aes(x=date, y=value)) +
  labs(title = "Spartanburg County, SC Unemployment Rate", x="Date", 
       y="Rate", caption ="Data from Fred.") + 
  geom_path(aes(color = variable), size=0.8)+
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") + 
  scale_y_continuous(labels = scales::label_percent(scale = 1,suffix ="%"),limits = c(0.5,21)) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.text=element_text(size=14), legend.position=c(.15,.1), legend.title=element_blank())+
  scale_color_manual(values=c("#003366", "#B22234"))
plot_SC_Spartanburg_Unrate

ggsave("Spartanburg_SC_UNRATE.png",
       device="png")


sumter_unrate<- fredr(series_id= "SCSUMT5URN", observation_start = as.Date("2011-01-01"))
sumter_unrate<-sumter_unrate[,c(1,3)]
colnames(sumter_unrate)<-c("date","Sumter")
dt_sumter<-list(sumter_unrate, scunrate) %>% reduce(left_join, by ="date")%>%
  select(date, `South Carolina`, `Sumter`) %>%
  gather(key = "variable", value = "value", -date)

#PLOT
plot_SC_Sumter_Unrate<- ggplot(dt_sumter, aes(x=date, y=value)) +
  labs(title = "Sumter County, SC Unemployment Rate", x="Date", 
       y="Rate", caption ="Data from Fred.") + 
  geom_path(aes(color = variable), size=0.8)+
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") + 
  scale_y_continuous(labels = scales::label_percent(scale = 1,suffix ="%"),limits = c(0.5,21)) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.text=element_text(size=14), legend.position=c(.15,.1), legend.title=element_blank())+
  scale_color_manual(values=c("#003366", "#B22234"))
plot_SC_Sumter_Unrate

ggsave("Sumter_SC_UNRATE.png",
       device="png")

union_unrate<- fredr(series_id= "SCUNIO7URN", observation_start = as.Date("2011-01-01"))
union_unrate<-union_unrate[,c(1,3)]
colnames(union_unrate)<-c("date","Union")
dt_union<-list(union_unrate, scunrate) %>% reduce(left_join, by ="date")%>%
  select(date, `South Carolina`, `Union`) %>%
  gather(key = "variable", value = "value", -date)

#PLOT
plot_SC_Union_Unrate<- ggplot(dt_union, aes(x=date, y=value)) +
  labs(title = "Union County, SC Unemployment Rate", x="Date", 
       y="Rate", caption ="Data from Fred.") + 
  geom_path(aes(color = variable), size=0.8)+
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") + 
  scale_y_continuous(labels = scales::label_percent(scale = 1,suffix ="%"),limits = c(0.5,21)) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.text=element_text(size=14), legend.position=c(.15,.1), legend.title=element_blank())+
  scale_color_manual(values=c("#003366", "#B22234"))
plot_SC_Union_Unrate

ggsave("Union_SC_UNRATE.png",
       device="png")

williamsburg_unrate<- fredr(series_id= "SCWILL9URN", observation_start = as.Date("2011-01-01"))
williamsburg_unrate<-williamsburg_unrate[,c(1,3)]
colnames(williamsburg_unrate)<-c("date","Williamsburg")
dt_williamsburg<-list(williamsburg_unrate, scunrate) %>% reduce(left_join, by ="date")%>%
  select(date, `South Carolina`, `Williamsburg`) %>%
  gather(key = "variable", value = "value", -date)

#PLOT
plot_SC_Williamsburg_Unrate<- ggplot(dt_williamsburg, aes(x=date, y=value)) +
  labs(title = "Williamsburg County, SC Unemployment Rate", x="Date", 
       y="Rate", caption ="Data from Fred.") + 
  geom_path(aes(color = variable), size=0.8)+
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") + 
  scale_y_continuous(labels = scales::label_percent(scale = 1,suffix ="%"),limits = c(0.5,21)) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.text=element_text(size=14), legend.position=c(.15,.1), legend.title=element_blank())+
  scale_color_manual(values=c("#003366", "#B22234"))
plot_SC_Williamsburg_Unrate

ggsave("Williamsburg_SC_UNRATE.png",
       device="png")

york_unrate<- fredr(series_id= "SCYORK5URN", observation_start = as.Date("2011-01-01"))
york_unrate<-york_unrate[,c(1,3)]
colnames(york_unrate)<-c("date","York")
dt_york<-list(york_unrate, scunrate) %>% reduce(left_join, by ="date")%>%
  select(date, `South Carolina`, `York`) %>%
  gather(key = "variable", value = "value", -date)

#PLOT
plot_SC_York_Unrate<- ggplot(dt_york, aes(x=date, y=value)) +
  labs(title = "York County, SC Unemployment Rate", x="Date", 
       y="Rate", caption ="Data from Fred.") + 
  geom_path(aes(color = variable), size=0.8)+
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year") + 
  scale_y_continuous(labels = scales::label_percent(scale = 1,suffix ="%"),limits = c(0.5,21)) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        legend.text=element_text(size=14), legend.position=c(.15,.1), legend.title=element_blank())+
  scale_color_manual(values=c("#003366", "#B22234"))
plot_SC_York_Unrate

ggsave("York_SC_UNRATE.png",
       device="png")
