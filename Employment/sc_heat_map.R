##SC Heat map
library(fredr)
library(ggplot2)
library(ggcorrplot)
library(maps)
library(mapsdata)
library(stringr)
library(dplyr)

state<-map_data("state")
sc <- subset(state, region=="south carolina")
counties <- map_data("county")
sc_county <- subset(counties, region=="south carolina")

sc_base<- ggplot(data=sc, mapping=aes(x=long,y=lat, group=group))+
  coord_fixed(1.1)+
  geom_polygon(color="black", fill="gray")+
  geom_polygon(data=sc_county, fill = NA, color="white")+
  geom_polygon(color="black", fill=NA)
sc_base


abbeville<- fredr(series_id= "SCABBE1URN", observation_start = as.Date("2021-12-01"))
abbeville<-abbeville[,c(2:3)]
colnames(abbeville)<-c("subregion","Unemployment Rate")
abbeville$subregion=recode(abbeville$subregion, "SCABBE1URN" = "abbeville")

aiken<- fredr(series_id= "SCAIKE0URN", observation_start = as.Date("2021-12-01"))
aiken<-aiken[,c(2:3)]
colnames(aiken)<-c("subregion","Unemployment Rate")
aiken$subregion=recode(aiken$subregion, "SCAIKE0URN" = "aiken")

allendale<- fredr(series_id= "SCALLE5URN", observation_start = as.Date("2021-12-01"))
allendale<-allendale[,c(2:3)]
colnames(allendale)<-c("subregion","Unemployment Rate")
allendale$subregion=recode(allendale$subregion, "SCALLE5URN" = "allendale")

anderson<- fredr(series_id= "SCANDE7URN", observation_start = as.Date("2021-12-01"))
anderson<-anderson[,c(2:3)]
colnames(anderson)<-c("subregion","Unemployment Rate")
anderson$subregion=recode(anderson$subregion, "SCANDE7URN" = "anderson")

bamberg<- fredr(series_id= "SCBAMB9URN", observation_start = as.Date("2021-12-01"))
bamberg<-bamberg[,c(2:3)]
colnames(bamberg)<-c("subregion","Unemployment Rate")
bamberg$subregion=recode(bamberg$subregion, "SCBAMB9URN" = "bamberg")

barnwell<- fredr(series_id= "SCBARN1URN", observation_start = as.Date("2021-12-01"))
barnwell<-barnwell[,c(2:3)]
colnames(barnwell)<-c("subregion","Unemployment Rate")
barnwell$subregion=recode(barnwell$subregion, "SCBARN1URN" = "barnwell")

beaufort<- fredr(series_id= "SCBEAU5URN", observation_start = as.Date("2021-12-01"))
beaufort<-beaufort[,c(2:3)]
colnames(beaufort)<-c("subregion","Unemployment Rate")
beaufort$subregion=recode(beaufort$subregion, "SCBEAU5URN" = "beaufort")

berkeley<- fredr(series_id= "SCBERK0URN", observation_start = as.Date("2021-12-01"))
berkeley<-berkeley[,c(2:3)]
colnames(berkeley)<-c("subregion","Unemployment Rate")
berkeley$subregion=recode(berkeley$subregion, "SCBERK0URN" = "berkeley")

calhoun<- fredr(series_id= "SCCALH7URN", observation_start = as.Date("2021-12-01"))
calhoun<-calhoun[,c(2:3)]
colnames(calhoun)<-c("subregion","Unemployment Rate")
calhoun$subregion=recode(calhoun$subregion, "SCCALH7URN" = "calhoun")

charleston<- fredr(series_id= "SCCHAR9URN", observation_start = as.Date("2021-12-01"))
charleston<-charleston[,c(2:3)]
colnames(charleston)<-c("subregion","Unemployment Rate")
charleston$subregion=recode(charleston$subregion, "SCCHAR9URN" = "charleston")

cherokee<- fredr(series_id= "SCCHER1URN", observation_start = as.Date("2021-12-01"))
cherokee<-cherokee[,c(2:3)]
colnames(cherokee)<-c("subregion","Unemployment Rate")
cherokee$subregion=recode(cherokee$subregion, "SCCHER1URN" = "cherokee")

chester<- fredr(series_id= "SCCHES3URN", observation_start = as.Date("2021-12-01"))
chester<-chester[,c(2:3)]
colnames(chester)<-c("subregion","Unemployment Rate")
chester$subregion=recode(chester$subregion, "SCCHES3URN" = "chester")

chesterfield<- fredr(series_id= "SCCHES5URN", observation_start = as.Date("2021-12-01"))
chesterfield<-chesterfield[,c(2:3)]
colnames(chesterfield)<-c("subregion","Unemployment Rate")
chesterfield$subregion=recode(chesterfield$subregion, "SCCHES5URN" = "chesterfield")

clarendon<- fredr(series_id= "SCCLAR7URN", observation_start = as.Date("2021-12-01"))
clarendon<-clarendon[,c(2:3)]
colnames(clarendon)<-c("subregion","Unemployment Rate")
clarendon$subregion=recode(clarendon$subregion, "SCCLAR7URN" = "clarendon")

colleton<- fredr(series_id= "SCCOLL9URN", observation_start = as.Date("2021-12-01"))
colleton<-colleton[,c(2:3)]
colnames(colleton)<-c("subregion","Unemployment Rate")
colleton$subregion=recode(colleton$subregion, "SCCOLL9URN" = "colleton")

darlington<- fredr(series_id= "SCDARL5URN", observation_start = as.Date("2021-12-01"))
darlington<-darlington[,c(2:3)]
colnames(darlington)<-c("subregion","Unemployment Rate")
darlington$subregion=recode(darlington$subregion, "SCDARL5URN" = "darlington")

dillon<- fredr(series_id= "SCDILL3URN", observation_start = as.Date("2021-12-01"))
dillon<-dillon[,c(2:3)]
colnames(dillon)<-c("subregion","Unemployment Rate")
dillon$subregion=recode(dillon$subregion, "SCDILL3URN" = "dillon")

dorchester<- fredr(series_id= "SCDORC7URN", observation_start = as.Date("2021-12-01"))
dorchester<-dorchester[,c(2:3)]
colnames(dorchester)<-c("subregion","Unemployment Rate")
dorchester$subregion=recode(dorchester$subregion, "SCDORC7URN" = "dorchester")

edgefield<- fredr(series_id= "SCEDGE7URN", observation_start = as.Date("2021-12-01"))
edgefield<-edgefield[,c(2:3)]
colnames(edgefield)<-c("subregion","Unemployment Rate")
edgefield$subregion=recode(edgefield$subregion, "SCEDGE7URN" = "edgefield")

fairfield<- fredr(series_id= "SCFAIR9URN", observation_start = as.Date("2021-12-01"))
fairfield<-fairfield[,c(2:3)]
colnames(fairfield)<-c("subregion","Unemployment Rate")
fairfield$subregion=recode(fairfield$subregion, "SCFAIR9URN" = "fairfield")

florence<- fredr(series_id= "SCFLOR0URN", observation_start = as.Date("2021-12-01"))
florence<-florence[,c(2:3)]
colnames(florence)<-c("subregion","Unemployment Rate")
florence$subregion=recode(florence$subregion, "SCFLOR0URN" = "florence")

georgetown<- fredr(series_id= "SCGEOR3URN", observation_start = as.Date("2021-12-01"))
georgetown<-georgetown[,c(2:3)]
colnames(georgetown)<-c("subregion","Unemployment Rate")
georgetown$subregion=recode(georgetown$subregion, "SCGEOR3URN" = "georgetown")

greenville<- fredr(series_id= "SCGREE5URN", observation_start = as.Date("2021-12-01"))
greenville<-greenville[,c(2:3)]
colnames(greenville)<-c("subregion","Unemployment Rate")
greenville$subregion=recode(greenville$subregion, "SCGREE5URN" = "greenville")

greenwood<- fredr(series_id= "SCGREE0URN", observation_start = as.Date("2021-12-01"))
greenwood<-greenwood[,c(2:3)]
colnames(greenwood)<-c("subregion","Unemployment Rate")
greenwood$subregion=recode(greenwood$subregion, "SCGREE0URN" = "greenwood")

hampton<- fredr(series_id= "SCHAMP9URN", observation_start = as.Date("2021-12-01"))
hampton<-hampton[,c(2:3)]
colnames(hampton)<-c("subregion","Unemployment Rate")
hampton$subregion=recode(hampton$subregion, "SCHAMP9URN" = "hampton")

horry<- fredr(series_id= "SCHORR1URN", observation_start = as.Date("2021-12-01"))
horry<-horry[,c(2:3)]
colnames(horry)<-c("subregion","Unemployment Rate")
horry$subregion=recode(horry$subregion, "SCHORR1URN" = "horry")

jasper<- fredr(series_id= "SCJASP3URN", observation_start = as.Date("2021-12-01"))
jasper<-jasper[,c(2:3)]
colnames(jasper)<-c("subregion","Unemployment Rate")
jasper$subregion=recode(jasper$subregion, "SCJASP3URN" = "jasper")

kershaw<- fredr(series_id= "SCKERS5URN", observation_start = as.Date("2021-12-01"))
kershaw<-kershaw[,c(2:3)]
colnames(kershaw)<-c("subregion","Unemployment Rate")
kershaw$subregion=recode(kershaw$subregion, "SCKERS5URN" = "kershaw")

lancaster<- fredr(series_id= "SCLANC7URN", observation_start = as.Date("2021-12-01"))
lancaster<-lancaster[,c(2:3)]
colnames(lancaster)<-c("subregion","Unemployment Rate")
lancaster$subregion=recode(lancaster$subregion, "SCLANC7URN" = "lancaster")

laurens<- fredr(series_id= "SCLAUR5URN", observation_start = as.Date("2021-12-01"))
laurens<-laurens[,c(2:3)]
colnames(laurens)<-c("subregion","Unemployment Rate")
laurens$subregion=recode(laurens$subregion, "SCLAUR5URN" = "laurens")

lee<- fredr(series_id= "SCLEEC1URN", observation_start = as.Date("2021-12-01"))
lee<-lee[,c(2:3)]
colnames(lee)<-c("subregion","Unemployment Rate")
lee$subregion=recode(lee$subregion, "SCLEEC1URN" = "lee")

lexington<- fredr(series_id= "SCLEXI0URN", observation_start = as.Date("2021-12-01"))
lexington<-lexington[,c(2:3)]
colnames(lexington)<-c("subregion","Unemployment Rate")
lexington$subregion=recode(lexington$subregion, "SCLEXI0URN" = "lexington")

marion<- fredr(series_id= "SCMARI7URN", observation_start = as.Date("2021-12-01"))
marion<-marion[,c(2:3)]
colnames(marion)<-c("subregion","Unemployment Rate")
marion$subregion=recode(marion$subregion, "SCMARI7URN" = "marion")

marlboro<- fredr(series_id= "SCMARL9URN", observation_start = as.Date("2021-12-01"))
marlboro<-marlboro[,c(2:3)]
colnames(marlboro)<-c("subregion","Unemployment Rate")
marlboro$subregion=recode(marlboro$subregion, "SCMARL9URN" = "marlboro")

mccormick<- fredr(series_id= "SCMCCO5URN", observation_start = as.Date("2021-12-01"))
mccormick<-mccormick[,c(2:3)]
colnames(mccormick)<-c("subregion","Unemployment Rate")
mccormick$subregion=recode(mccormick$subregion, "SCMCCO5URN" = "mccormick")

newberry<- fredr(series_id= "SCNEWB1URN", observation_start = as.Date("2021-12-01"))
newberry<-newberry[,c(2:3)]
colnames(newberry)<-c("subregion","Unemployment Rate")
newberry$subregion=recode(newberry$subregion, "SCNEWB1URN" = "newberry")

oconee<- fredr(series_id= "SCOCON3URN", observation_start = as.Date("2021-12-01"))
oconee<-oconee[,c(2:3)]
colnames(oconee)<-c("subregion","Unemployment Rate")
oconee$subregion=recode(oconee$subregion, "SCOCON3URN" = "oconee")

orangeburg<- fredr(series_id= "SCORAN5URN", observation_start = as.Date("2021-12-01"))
orangeburg<-orangeburg[,c(2:3)]
colnames(orangeburg)<-c("subregion","Unemployment Rate")
orangeburg$subregion=recode(orangeburg$subregion, "SCORAN5URN" = "orangeburg")

pickens<- fredr(series_id= "SCPICK0URN", observation_start = as.Date("2021-12-01"))
pickens<-pickens[,c(2:3)]
colnames(pickens)<-c("subregion","Unemployment Rate")
pickens$subregion=recode(pickens$subregion, "SCPICK0URN" = "pickens")

richland<- fredr(series_id= "SCRICH9URN", observation_start = as.Date("2021-12-01"))
richland<-richland[,c(2:3)]
colnames(richland)<-c("subregion","Unemployment Rate")
richland$subregion=recode(richland$subregion, "SCRICH9URN" = "richland")

saluda<- fredr(series_id= "SCSALU1URN", observation_start = as.Date("2021-12-01"))
saluda<-saluda[,c(2:3)]
colnames(saluda)<-c("subregion","Unemployment Rate")
saluda$subregion=recode(saluda$subregion, "SCSALU1URN" = "saluda")

spartanburg<- fredr(series_id= "SCSPAR0URN", observation_start = as.Date("2021-12-01"))
spartanburg<-spartanburg[,c(2:3)]
colnames(spartanburg)<-c("subregion","Unemployment Rate")
spartanburg$subregion=recode(spartanburg$subregion, "SCSPAR0URN" = "spartanburg")

sumter<- fredr(series_id= "SCSUMT5URN", observation_start = as.Date("2021-12-01"))
sumter<-sumter[,c(2:3)]
colnames(sumter)<-c("subregion","Unemployment Rate")
sumter$subregion=recode(sumter$subregion, "SCSUMT5URN" = "sumter")

union<- fredr(series_id= "SCUNIO7URN", observation_start = as.Date("2021-12-01"))
union<-union[,c(2:3)]
colnames(union)<-c("subregion","Unemployment Rate")
union$subregion=recode(union$subregion, "SCUNIO7URN" = "union")

williamsburg<- fredr(series_id= "SCWILL9URN", observation_start = as.Date("2021-12-01"))
williamsburg<-williamsburg[,c(2:3)]
colnames(williamsburg)<-c("subregion","Unemployment Rate")
williamsburg$subregion=recode(williamsburg$subregion, "SCWILL9URN" = "williamsburg")

york<- fredr(series_id= "SCYORK5URN", observation_start = as.Date("2021-12-01"))
york<-york[,c(2:3)]
colnames(york)<-c("subregion","Unemployment Rate")
york$subregion=recode(york$subregion, "SCYORK5URN" = "york")



all_counties=rbind(abbeville,aiken,allendale,anderson,bamberg,barnwell,beaufort,berkeley,calhoun,charleston,cherokee,chester,chesterfield,clarendon,colleton,darlington,dillon,dorchester,
                   edgefield,fairfield,florence,georgetown,greenville,greenwood,hampton,horry,jasper,kershaw,lancaster,laurens,lee,lexington,marion,marlboro,mccormick,
                   newberry,oconee,orangeburg,pickens,richland,saluda,spartanburg,sumter,union,williamsburg,york)
final<- inner_join(sc_county,all_counties,by="subregion")

unemployment_map_SC <- sc_base+
  geom_polygon(data=final, aes(fill=`Unemployment Rate`), color="white")+
  geom_polygon(color="black",fill=NA)+
  theme_bw()+
  theme(axis.text=element_blank(),
        axis.line=element_blank(),
        axis.ticks=element_blank(),
        panel.border=element_blank(),
        panel.grid = element_blank(),
        axis.title=element_blank(),
        legend.position=c(0.15,0.25))+
  labs(title="South Carolina County Level Unemployment Rate",caption="Data are from the US Bureau of Labor Statistics.\n Retrieved from Fred.")+
  scale_fill_gradient(low="white",high="#003366")

unemployment_map_SC

ggsave("unemployment_map_SC.png",
       plot = unemployment_map_SC,
       device = "png",
       width = 12,
       height = 10,
       units= "in")






