### DIET MODEL FOR ACCASP INVERT-GROUNDFISH PROJECT ----
# Purpose: Model proportion of crab and shrimp in diet
# Author: S. Zabihi-Seissan

#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
                                                      ### Packages ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

library(ggplot2)
library(data.table)
library(mgcv)
library(sp)

#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
                                                      ### Load Data ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

# Load Data
Diet_Data<-fread('data/combined data/Diet_Data_Allregions.csv')

Abiotic_Data<-fread('data/combined data/Abiotic_Data_Allregions.csv')

RV_Data<-fread('data/combined data/RV_Data_Allregions.csv')

# Merging Data
Diet_Data$ID<-paste(Diet_Data$region,Diet_Data$year_surv,Diet_Data$vessel,Diet_Data$trip,Diet_Data$set)
Abiotic_Data$ID<-paste(Abiotic_Data$region,Abiotic_Data$year_surv,Abiotic_Data$vessel,Abiotic_Data$trip,Abiotic_Data$set)
Abiotic_Data[,c(1:10)]<-NULL
RV_Data$ID<-paste(RV_Data$region,RV_Data$year_surv,RV_Data$vessel,RV_Data$trip,RV_Data$set)
RV_Data[,c(1:10)]<-NULL
Data<-merge(Diet_Data,Abiotic_Data, by ="ID")
Data<-merge(Data,RV_Data,by="ID")
Data$ID<-NULL
Diet_Data<-Data

rm(list=setdiff(ls(), c("Diet_Data")))

# Modify coordinate system to meters
coordinates(Diet_Data)<-~longitude + latitude
proj4string(Diet_Data) <- CRS("+init=epsg:4326") # WGS 84
CRS.new <- CRS("+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs") #determined using proj4string(NL)
Diet_Data<- spTransform(Diet_Data, CRS.new)
Diet_Data<-as.data.table(Diet_Data)

# Load spatial SDMs for predicted values
models <- list.files("code/outputs/models/spatial", pattern="*.rda", full.names=TRUE)
lapply(models,load,.GlobalEnv)

#Predict values by region
Diet_Data[region=="Newfoundland"&(month==9|month==10|month==11|month==12|month==1|month==2),region:="Newfoundland_fall"]
Diet_Data[region=="Newfoundland"&(month==3|month==4|month==5|month==6|month==7),region:="Newfoundland_spring"]
NL_fall_Data<-Diet_Data[region=="Newfoundland_fall"]
NL_spring_Data<-Diet_Data[region=="Newfoundland_spring"]
MAR_Data<-Diet_Data[region=="Maritimes"]
QC_Data<-Diet_Data[region=="Quebec"]
ARC_Data<-Diet_Data[region=="Arctic"]
GUL_Data<-Diet_Data[region=="Gulf"]

MAR_Data<-MAR_Data[year_surv>2007]
MAR_Data$pred_shrimp<-predict(mar_shrimpmodel,newdata=MAR_Data,type="response")
MAR_Data$pred_crab<-predict(mar_crabsmallmodel,newdata=MAR_Data,type="response")
MAR_Data$pred_cod<-predict(mar_codmodel,newdata=MAR_Data,type="response")
MAR_Data$pred_turbot<-predict(mar_turbotmodel,newdata=MAR_Data,type="response")
MAR_Data$pred_redfish<-predict(mar_redfishmodel,newdata=MAR_Data,type="response")
MAR_Data$pred_planktivores<-predict(mar_planktivoremodel,newdata=MAR_Data,type="response")

QC_Data<-QC_Data[year_surv>1992]
QC_Data$pred_shrimp<-predict(qc_shrimpmodel,newdata=QC_Data,type="response")
QC_Data$pred_crab<-predict(qc_crabsmallmodel,newdata=QC_Data,type="response")
QC_Data$pred_cod<-predict(qc_codmodel,newdata=QC_Data,type="response")
QC_Data$pred_turbot<-predict(qc_turbotmodel,newdata=QC_Data,type="response")
QC_Data$pred_redfish<-predict(qc_redfishmodel,newdata=QC_Data,type="response")
QC_Data$pred_planktivores<-predict(qc_planktivoremodel,newdata=QC_Data,type="response")

ARC_Data$pred_shrimp<-predict(arc_shrimpmodel,newdata=ARC_Data,type="response")
#ARC_Data$pred_crab<-predict(arc_crabsmallmodel,newdata=ARC_Data,type="response")
ARC_Data$pred_cod<-predict(arc_codmodel,newdata=ARC_Data,type="response")
ARC_Data$pred_turbot<-predict(arc_turbotmodel,newdata=ARC_Data,type="response")
ARC_Data$pred_redfish<-predict(arc_redfishmodel,newdata=ARC_Data,type="response")
ARC_Data$pred_planktivores<-predict(arc_planktivoremodel,newdata=ARC_Data,type="response")

GUL_Data$pred_shrimp<-predict(gul_shrimpmodel,newdata=GUL_Data,type="response")
GUL_Data$pred_crab<-predict(gul_crabsmallmodel,newdata=GUL_Data,type="response")
GUL_Data$pred_cod<-predict(gul_codmodel,newdata=GUL_Data,type="response")
GUL_Data$pred_turbot<-predict(gul_turbotmodel,newdata=GUL_Data,type="response")
GUL_Data$pred_redfish<-predict(gul_redfishmodel,newdata=GUL_Data,type="response")
GUL_Data$pred_planktivores<-predict(gul_planktivoremodel,newdata=GUL_Data,type="response")

NL_fall_Data$pred_shrimp<-predict(nl_shrimpmodel_fall,newdata=NL_fall_Data,type="response")
NL_fall_Data$pred_crab<-predict(nl_crabsmallmodel_fall,newdata=NL_fall_Data,type="response")
NL_fall_Data$pred_cod<-predict(nl_codmodel_fall,newdata=NL_fall_Data,type="response")
NL_fall_Data$pred_turbot<-predict(nl_turbotmodel_fall,newdata=NL_fall_Data,type="response")
NL_fall_Data$pred_redfish<-predict(nl_redfishmodel_fall,newdata=NL_fall_Data,type="response")
NL_fall_Data$pred_planktivores<-predict(nl_planktivoremodel_fall,newdata=NL_fall_Data,type="response")

NL_spring_Data$pred_shrimp<-predict(nl_shrimpmodel_spring,newdata=NL_spring_Data,type="response")
NL_spring_Data$pred_crab<-predict(nl_crabsmallmodel_spring,newdata=NL_spring_Data,type="response")
NL_spring_Data$pred_cod<-predict(nl_codmodel_spring,newdata=NL_spring_Data,type="response")
NL_spring_Data$pred_turbot<-predict(nl_turbotmodel_spring,newdata=NL_spring_Data,type="response")
NL_spring_Data$pred_redfish<-predict(nl_redfishmodel_spring,newdata=NL_spring_Data,type="response")
NL_spring_Data$pred_planktivores<-predict(nl_planktivoremodel_spring,newdata=NL_spring_Data,type="response")

rm(list=setdiff(ls(), c("Diet_Data","MAR_Data","QC_Data","ARC_Data","GUL_Data","NL_fall_Data","NL_spring_Data")))

#Re-merge data
Diet_Data<-rbind(MAR_Data,QC_Data,ARC_Data,GUL_Data,NL_fall_Data,NL_spring_Data,fill=TRUE)
fwrite(Diet_Data, "code/outputs/data/Diet_Data_Projected.csv")
Diet_Data<-fread('code/outputs/data/Diet_Data_Projected.csv')

#Calculate presence/absence of shrimp and crab in diet
Diet_Data[pandalus_borealis_weight==0,shrimp_pres:=0]
Diet_Data[pandalus_borealis_weight>0,shrimp_pres:=1]
Diet_Data[chionoecetes_opilio_weight==0,crab_pres:=0]
Diet_Data[chionoecetes_opilio_weight>0,crab_pres:=1]

Diet_Data$dummy<-1

#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
                                                    ### Run Diet models ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

#Presence absence models
Diet_Data$region<-as.factor(Diet_Data$region)
Diet_Data$year_surv<-as.factor(Diet_Data$year_surv)
cod_shrimp_presence<-gam(shrimp_pres~s(temperature_at_bottom,k=6)+
                           pred_shrimp+pred_planktivores+
                           s(region,year_surv,bs="re",by=dummy),family=binomial,data=Diet_Data,subset = predator_species=="gadus_morhua",method="REML")
turbot_shrimp_presence<-gam(shrimp_pres~s(temperature_at_bottom,k=6)+
                              pred_shrimp+pred_planktivores+
                              s(region,year_surv,bs="re",by=dummy),family=binomial,data=Diet_Data,subset = predator_species=="reinhardtius_hippoglossoides",method="REML")
redfish_shrimp_presence<-gam(shrimp_pres~s(temperature_at_bottom,k=6)+
                              pred_shrimp+pred_planktivores+
                             s(region,year_surv,bs="re",by=dummy),family=binomial,data=Diet_Data,subset = predator_species=="sebastes_mentella",method="REML")
cod_crab_presence<-gam(crab_pres~s(temperature_at_bottom,k=6)+
                           pred_crab+pred_planktivores+
                           s(region,year_surv,bs="re",by=dummy),family=binomial,data=Diet_Data,subset = predator_species=="gadus_morhua",method="REML")


#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
                                                    ### Save models ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

save(cod_shrimp_presence, file = "code/outputs/models/diet/Cod_Diet_Shrimp_Presence.rda")
save(turbot_shrimp_presence, file = "code/outputs/models/diet/Turbot_Diet_Shrimp_Presence.rda")
save(redfish_shrimp_presence, file = "code/outputs/models/diet/Redfish_Diet_Shrimp_Presence.rda")
save(cod_crab_presence, file = "code/outputs/models/diet/Cod_Diet_Crab_Presence.rda")



#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
### Load models ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

models <- list.files("code/outputs/models/diet", pattern="*.rda", full.names=TRUE)
lapply(models,load,.GlobalEnv)

#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
                                              ### Model output projections ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

# Projections
fam <- family(cod_shrimp_presence)
ilink <- fam$linkinv
NewData = expand.grid(temperature_at_bottom = 2.4,
                      pred_planktivores = 279.8,
                      pred_shrimp = seq(from=0,to=26553.92 ,by=100),
                      region= c("Arctic","Maritimes","Gulf","Newfoundland_fall","Newfoundland_spring","Quebec"),
                      year_surv= seq(from=1993, to=2018,by=1),dummy=0)
NewData<-as.data.table(NewData)
NewData$pred<-predict(cod_shrimp_presence,newdata = NewData)
NewData$fit<-ilink(NewData$pred)
NewData$fit<-(NewData$fit/(1-NewData$fit))            #Turn probability into odds for plotting
NewData$se.fit<-predict(cod_shrimp_presence,newdata = NewData,se.fit=TRUE)$se.fit
NewData$cilow <- ilink(NewData$pred - (1.96 * NewData$se.fit))
NewData$cilow<-(NewData$cilow/(1-NewData$cilow))
NewData$cihi <- ilink(NewData$pred + (1.96 * NewData$se.fit))
NewData$cihi<-(NewData$cihi/(1-NewData$cihi))
NewData$Species<-"Cod"
Cod_Shrimp_NewData<-NewData
Cod_Data<-Diet_Data[predator_species=="gadus_morhua"]
Plot_Shrimp_Temperature<-ggplot()+
 # geom_ribbon(data=Cod_Shrimp_NewData, aes(pred_shrimp,ymin=cilow,ymax=cihi),fill="grey35",alpha=0.25)+
#  geom_line(data=Cod_Shrimp_NewData, aes(pred_shrimp,fit),size=1)+
  geom_point(data=Cod_Data,aes(pred_crab,crab_pres))+
  facet_wrap(~region,scales="free")+
  ylab("Odds of crab in diet")+xlab("Predicted crab density (kg/km^2)")+
 # geom_hline(yintercept=1,linetype="dashed")+
  #ylim(0,1)+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key=element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
Plot_Shrimp_Temperature

#Plot Turbot
fam <- family(turbot_shrimp_presence)
ilink <- fam$linkinv
NewData = expand.grid(temperature_at_bottom = 3.3,
                      pred_planktivores = 213,
                      pred_shrimp = seq(from=0,to=26553.92 ,by=100),
                      region= c("Arctic","Maritimes","Gulf","Newfoundland_fall","Newfoundland_spring","Quebec"),
                      year_surv= seq(from=1993, to=2018,by=1),dummy=0)
NewData<-as.data.table(NewData)
NewData$pred<-predict(turbot_shrimp_presence,newdata = NewData)
NewData$fit<-ilink(NewData$pred)
NewData$fit<-(NewData$fit/(1-NewData$fit))            #Turn probability into odds for plotting
NewData$se.fit<-predict(turbot_shrimp_presence,newdata = NewData,se.fit=TRUE)$se.fit
NewData$cilow <- ilink(NewData$pred - (1.96 * NewData$se.fit))
NewData$cilow<-(NewData$cilow/(1-NewData$cilow)) 
NewData$cihi <- ilink(NewData$pred + (1.96 * NewData$se.fit))
NewData$cihi<-(NewData$cihi/(1-NewData$cihi)) 
NewData$Species<-"Turbot"
Turbot_Shrimp_NewData<-NewData

Plot_Shrimp_Temperature<-ggplot()+
  geom_ribbon(data=Turbot_Shrimp_NewData, aes(pred_shrimp,ymin=cilow,ymax=cihi),fill="grey35",alpha=0.25)+
  geom_line(data=Turbot_Shrimp_NewData, aes(pred_shrimp,fit),size=1)+
 # facet_wrap(~region,scales="free")+
  ylab("Odds of shrimp in diet")+xlab("Predicted shrimp density (kg/km^2)")+
  #ylim(0,1)+
  geom_hline(yintercept=1,linetype="dashed")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key=element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
Plot_Shrimp_Temperature

#Plot Redfish
fam <- family(redfish_shrimp_presence)
ilink <- fam$linkinv
NewData = expand.grid(temperature_at_bottom = 5,
                      pred_planktivores = 134.6,
                      pred_shrimp = seq(from=0,to=11027.14 ,by=100),
                      region= c("Arctic","Maritimes","Gulf","Newfoundland_fall","Newfoundland_spring","Quebec"),
                      year_surv= seq(from=1993, to=2018,by=1),dummy=0)
NewData<-as.data.table(NewData)
NewData<-NewData[!(year_surv==1998|year_surv==2000|year_surv==2001|year_surv==2002|year_surv==2003|
                     year_surv==2007)]
NewData$pred<-predict(redfish_shrimp_presence,newdata = NewData)
NewData$fit<-ilink(NewData$pred)
NewData$fit<-(NewData$fit/(1-NewData$fit))            #Turn probability into odds for plotting
NewData$se.fit<-predict(redfish_shrimp_presence,newdata = NewData,se.fit=TRUE)$se.fit
NewData$cilow <- ilink(NewData$pred - (1.96 * NewData$se.fit))
NewData$cilow<-(NewData$cilow/(1-NewData$cilow))
NewData$cihi <- ilink(NewData$pred + (1.96 * NewData$se.fit))
NewData$cihi<-(NewData$cihi/(1-NewData$cihi))
NewData$Species<-"Redfish"
Redfish_Shrimp_NewData<-NewData

Plot_Shrimp_Temperature<-ggplot()+
  geom_ribbon(data=Redfish_Shrimp_NewData, aes(pred_shrimp,ymin=cilow,ymax=cihi),fill="grey35",alpha=0.25)+
  geom_line(data=Redfish_Shrimp_NewData, aes(pred_shrimp,fit),size=1)+
 # facet_wrap(~region,scales="free")+
  ylab("Odds of shrimp in diet")+xlab("Predicted shrimp density (kg/km^2)")+
 # ylim(0,1)+
  geom_hline(yintercept=1,linetype="dashed")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key=element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
Plot_Shrimp_Temperature

### Plotting alternative prey

# Projections
fam <- family(cod_shrimp_presence)
ilink <- fam$linkinv
NewData = expand.grid(temperature_at_bottom = 2.4,
                      pred_shrimp = 753.01,
                      pred_planktivores = seq(from=0,to=3306.685,by=100),
                      region= c("Arctic","Maritimes","Gulf","Newfoundland_fall","Newfoundland_spring","Quebec"),
                      year_surv= seq(from=1993, to=2018,by=1),dummy=0)
NewData<-as.data.table(NewData)
NewData$pred<-predict(cod_shrimp_presence,newdata = NewData)
NewData$fit<-ilink(NewData$pred)
NewData$fit<-(NewData$fit/(1-NewData$fit)) 
NewData$se.fit<-predict(cod_shrimp_presence,newdata = NewData,se.fit=TRUE)$se.fit
NewData$cilow <- ilink(NewData$pred - (1.96 * NewData$se.fit))
NewData$cilow<-(NewData$cilow/(1-NewData$cilow)) 
NewData$cihi <- ilink(NewData$pred + (1.96 * NewData$se.fit))
NewData$cihi<-(NewData$cihi/(1-NewData$cihi)) 
NewData$Species<-"Cod"
Cod_Shrimp_NewData<-NewData

Plot_Shrimp_Temperature<-ggplot()+
  geom_ribbon(data=Cod_Shrimp_NewData, aes(pred_planktivores,ymin=cilow,ymax=cihi),fill="grey35",alpha=0.25)+
  geom_line(data=Cod_Shrimp_NewData, aes(pred_planktivores,fit),size=1)+
  #  facet_wrap(~region,scales="free")+
  ylab("Odds of shrimp in diet")+xlab("Predicted planktivore density (kg/km^2)")+
 # ylim(0,1)+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key=element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
Plot_Shrimp_Temperature

#Plot Turbot
fam <- family(turbot_shrimp_presence)
ilink <- fam$linkinv
NewData = expand.grid(temperature_at_bottom = 3.3,
                      pred_shrimp = 2135.3,
                      pred_planktivores = seq(from=0,to=2279.331,by=100),
                      region= c("Arctic","Maritimes","Gulf","Newfoundland_fall","Newfoundland_spring","Quebec"),
                      year_surv= seq(from=1993, to=2018,by=1),dummy=0)
NewData<-as.data.table(NewData)
NewData$pred<-predict(turbot_shrimp_presence,newdata = NewData)
NewData$fit<-ilink(NewData$pred)
NewData$fit<-(NewData$fit/(1-NewData$fit)) 
NewData$se.fit<-predict(turbot_shrimp_presence,newdata = NewData,se.fit=TRUE)$se.fit
NewData$cilow <- ilink(NewData$pred - (1.96 * NewData$se.fit))
NewData$cilow<-(NewData$cilow/(1-NewData$cilow)) 
NewData$cihi <- ilink(NewData$pred + (1.96 * NewData$se.fit))
NewData$cihi<-(NewData$cihi/(1-NewData$cihi)) 
NewData$Species<-"Turbot"
Turbot_Shrimp_NewData<-NewData

Plot_Shrimp_Temperature<-ggplot()+
  geom_ribbon(data=Turbot_Shrimp_NewData, aes(pred_planktivores,ymin=cilow,ymax=cihi),fill="grey35",alpha=0.25)+
  geom_line(data=Turbot_Shrimp_NewData, aes(pred_planktivores,fit),size=1)+
  # facet_wrap(~region,scales="free")+
  ylab("Odds of shrimp in diet")+xlab("Predicted planktivore density (kg/km^2)")+
  #ylim(0,1)+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key=element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
Plot_Shrimp_Temperature

#Plot Redfish
fam <- family(redfish_shrimp_presence)
ilink <- fam$linkinv
NewData = expand.grid(temperature_at_bottom = 5,
                      pred_shrimp = 702.4,
                      pred_planktivores = seq(from=0,to=2025.234,by=100),
                      region= c("Arctic","Maritimes","Gulf","Newfoundland_fall","Newfoundland_spring","Quebec"),
                      year_surv= seq(from=1993, to=2018,by=1),dummy=0)
NewData<-as.data.table(NewData)
NewData<-NewData[!(year_surv==1998|year_surv==2000|year_surv==2001|year_surv==2002|year_surv==2003|
                     year_surv==2007)]
NewData$pred<-predict(redfish_shrimp_presence,newdata = NewData)
NewData$fit<-ilink(NewData$pred)
NewData$fit<-(NewData$fit/(1-NewData$fit)) 
NewData$se.fit<-predict(redfish_shrimp_presence,newdata = NewData,se.fit=TRUE)$se.fit
NewData$cilow <- ilink(NewData$pred - (1.96 * NewData$se.fit))
NewData$cilow<-(NewData$cilow/(1-NewData$cilow)) 
NewData$cihi <- ilink(NewData$pred + (1.96 * NewData$se.fit))
NewData$cihi<-(NewData$cihi/(1-NewData$cihi)) 
NewData$Species<-"Redfish"
Redfish_Shrimp_NewData<-NewData

Plot_Shrimp_Temperature<-ggplot()+
  geom_ribbon(data=Redfish_Shrimp_NewData, aes(pred_planktivores,ymin=cilow,ymax=cihi),fill="grey35",alpha=0.25)+
  geom_line(data=Redfish_Shrimp_NewData, aes(pred_planktivores,fit),size=1)+
  # facet_wrap(~region,scales="free")+
  ylab("Odds of shrimp in diet")+xlab("Predicted planktivore density (kg/km^2)")+
 # ylim(0,1)+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key=element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
Plot_Shrimp_Temperature


### Effect of temperature

# Projections
fam <- family(cod_shrimp_presence)
ilink <- fam$linkinv
NewData = expand.grid(pred_planktivores = 279.8,
                      pred_shrimp = 753.01,
                      temperature_at_bottom = seq(from=-1.9,to=15.1,by=0.1),
                      region= c("Arctic","Maritimes","Gulf","Newfoundland_fall","Newfoundland_spring","Quebec"),
                      year_surv= seq(from=1993, to=2018,by=1),dummy=0)
NewData<-as.data.table(NewData)
NewData$pred<-predict(cod_shrimp_presence,newdata = NewData)
NewData$fit<-ilink(NewData$pred)
NewData$fit<-(NewData$fit/(1-NewData$fit)) 
NewData$se.fit<-predict(cod_shrimp_presence,newdata = NewData,se.fit=TRUE)$se.fit
NewData$cilow <- ilink(NewData$pred - (1.96 * NewData$se.fit))
NewData$cilow<-(NewData$cilow/(1-NewData$cilow)) 
NewData$cihi <- ilink(NewData$pred + (1.96 * NewData$se.fit))
NewData$cihi<-(NewData$cihi/(1-NewData$cihi)) 
NewData$Species<-"Cod"
Cod_Shrimp_NewData<-NewData

Plot_Shrimp_Temperature<-ggplot()+
  geom_ribbon(data=Cod_Shrimp_NewData, aes(temperature_at_bottom,ymin=cilow,ymax=cihi),fill="grey35",alpha=0.25)+
  geom_line(data=Cod_Shrimp_NewData, aes(temperature_at_bottom,fit),size=1)+
  #  facet_wrap(~region,scales="free")+
  ylab("Odds of shrimp in diet")+xlab("Bottom temperature (C)")+
 # ylim(0,1)+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key=element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
Plot_Shrimp_Temperature

#Plot Turbot
fam <- family(turbot_shrimp_presence)
ilink <- fam$linkinv
NewData = expand.grid(pred_planktivores = 213,
                      pred_shrimp = 2135.3,
                      temperature_at_bottom = seq(from=-1.5,to=9.1,by=0.1),
                      region= c("Arctic","Maritimes","Gulf","Newfoundland_fall","Newfoundland_spring","Quebec"),
                      year_surv= seq(from=1993, to=2018,by=1),dummy=0)
NewData<-as.data.table(NewData)
NewData$pred<-predict(turbot_shrimp_presence,newdata = NewData)
NewData$fit<-ilink(NewData$pred)
NewData$fit<-(NewData$fit/(1-NewData$fit)) 
NewData$se.fit<-predict(turbot_shrimp_presence,newdata = NewData,se.fit=TRUE)$se.fit
NewData$cilow <- ilink(NewData$pred - (1.96 * NewData$se.fit))
NewData$cilow<-(NewData$cilow/(1-NewData$cilow)) 
NewData$cihi <- ilink(NewData$pred + (1.96 * NewData$se.fit))
NewData$cihi<-(NewData$cihi/(1-NewData$cihi)) 
NewData$Species<-"Turbot"
Turbot_Shrimp_NewData<-NewData

Plot_Shrimp_Temperature<-ggplot()+
  geom_ribbon(data=Turbot_Shrimp_NewData, aes(temperature_at_bottom,ymin=cilow,ymax=cihi),fill="grey35",alpha=0.25)+
  geom_line(data=Turbot_Shrimp_NewData, aes(temperature_at_bottom,fit),size=1)+
  # facet_wrap(~region,scales="free")+
  ylab("Odds of shrimp in diet")+xlab("Bottom temperature (C)")+
 # ylim(0,1)+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key=element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
Plot_Shrimp_Temperature

#Plot Redfish
fam <- family(redfish_shrimp_presence)
ilink <- fam$linkinv
NewData = expand.grid(pred_planktivores = 134.6,
                      pred_shrimp = 702.4,
                      temperature_at_bottom = seq(from=-1.1,to=11.79,by=0.1),
                      region= c("Arctic","Maritimes","Gulf","Newfoundland_fall","Newfoundland_spring","Quebec"),
                      year_surv= seq(from=1993, to=2018,by=1),dummy=0)
NewData<-as.data.table(NewData)
NewData<-NewData[!(year_surv==1998|year_surv==2000|year_surv==2001|year_surv==2002|year_surv==2003|
                     year_surv==2007)]
NewData$pred<-predict(redfish_shrimp_presence,newdata = NewData)
NewData$fit<-ilink(NewData$pred)
NewData$fit<-(NewData$fit/(1-NewData$fit)) 
NewData$se.fit<-predict(redfish_shrimp_presence,newdata = NewData,se.fit=TRUE)$se.fit
NewData$cilow <- ilink(NewData$pred - (1.96 * NewData$se.fit))
NewData$cilow<-(NewData$cilow/(1-NewData$cilow)) 
NewData$cihi <- ilink(NewData$pred + (1.96 * NewData$se.fit))
NewData$cihi<-(NewData$cihi/(1-NewData$cihi)) 
NewData$Species<-"Redfish"
Redfish_Shrimp_NewData<-NewData

Plot_Shrimp_Temperature<-ggplot()+
  geom_ribbon(data=Redfish_Shrimp_NewData, aes(temperature_at_bottom,ymin=cilow,ymax=cihi),fill="grey35",alpha=0.25)+
  geom_line(data=Redfish_Shrimp_NewData, aes(temperature_at_bottom,fit),size=1)+
  # facet_wrap(~region,scales="free")+
  ylab("Odds of shrimp in diet")+xlab("Bottom temperature (C)")+
 # ylim(0,1)+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key=element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
Plot_Shrimp_Temperature


### Cod crab diet model

# Crab density effect
fam <- family(cod_crab_presence)
ilink <- fam$linkinv
NewData = expand.grid(pred_planktivores = 279.8,
                      temperature_at_bottom = 2.4,
                      pred_crab = seq(from=0,to=19.62628,by=0.1),
                      region= c("Maritimes","Gulf","Newfoundland_fall","Newfoundland_spring","Quebec"),
                      year_surv= seq(from=1993, to=2018,by=1),dummy=0)
NewData<-as.data.table(NewData)
NewData$pred<-predict(cod_crab_presence,newdata = NewData)
NewData$fit<-ilink(NewData$pred)
NewData$fit<-(NewData$fit/(1-NewData$fit)) 
NewData$se.fit<-predict(cod_crab_presence,newdata = NewData,se.fit=TRUE)$se.fit
NewData$cilow <- ilink(NewData$pred - (1.96 * NewData$se.fit))
NewData$cilow<-(NewData$cilow/(1-NewData$cilow)) 
NewData$cihi <- ilink(NewData$pred + (1.96 * NewData$se.fit))
NewData$cihi<-(NewData$cihi/(1-NewData$cihi)) 
NewData$Species<-"Cod"
Cod_Crab_NewData<-NewData

Plot_Crab_Temperature<-ggplot()+
  geom_ribbon(data=Cod_Crab_NewData, aes(pred_crab,ymin=cilow,ymax=cihi),fill="grey35",alpha=0.25)+
  geom_line(data=Cod_Crab_NewData, aes(pred_crab,fit),size=1)+
  #  facet_wrap(~region,scales="free")+
  ylab("Odds of crab in diet")+xlab("Predicted small crab density (kg/km^2)")+
 # ylim(0,1)+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key=element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
Plot_Crab_Temperature

# Planktivore effect
fam <- family(cod_crab_presence)
ilink <- fam$linkinv
NewData = expand.grid(pred_crab = 0.16,
                      temperature_at_bottom = 2.4,
                      pred_planktivores = seq(from=0,to=12912.12,by=100),
                      region= c("Maritimes","Gulf","Newfoundland_fall","Newfoundland_spring","Quebec"),
                      year_surv= seq(from=1993, to=2018,by=1),dummy=0)
NewData<-as.data.table(NewData)
NewData$pred<-predict(cod_crab_presence,newdata = NewData)
NewData$fit<-ilink(NewData$pred)
NewData$fit<-(NewData$fit/(1-NewData$fit)) 
NewData$se.fit<-predict(cod_crab_presence,newdata = NewData,se.fit=TRUE)$se.fit
NewData$cilow <- ilink(NewData$pred - (1.96 * NewData$se.fit))
NewData$cilow<-(NewData$cilow/(1-NewData$cilow)) 
NewData$cihi <- ilink(NewData$pred + (1.96 * NewData$se.fit))
NewData$cihi<-(NewData$cihi/(1-NewData$cihi)) 
NewData$Species<-"Cod"
Cod_Crab_NewData<-NewData

Plot_Crab_Temperature<-ggplot()+
  geom_ribbon(data=Cod_Crab_NewData, aes(pred_planktivores,ymin=cilow,ymax=cihi),fill="grey35",alpha=0.25)+
  geom_line(data=Cod_Crab_NewData, aes(pred_planktivores,fit),size=1)+
  #  facet_wrap(~region,scales="free")+
  ylab("Odds of crab in diet")+xlab("Predicted planktivore density (kg/km^2)")+
  #ylim(0,1)+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key=element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
Plot_Crab_Temperature

#Plot temperautre effect
fam <- family(cod_crab_presence)
ilink <- fam$linkinv
NewData = expand.grid(pred_crab = 0.16,
                      pred_planktivores = 279.8,
                      temperature_at_bottom = seq(from=-1.9,to=15.1,by=0.1),
                      region= c("Maritimes","Gulf","Newfoundland_fall","Newfoundland_spring","Quebec"),
                      year_surv= seq(from=1993, to=2018,by=1),dummy=0)
NewData<-as.data.table(NewData)
NewData$pred<-predict(cod_crab_presence,newdata = NewData)
NewData$fit<-ilink(NewData$pred)
NewData$se.fit<-predict(cod_crab_presence,newdata = NewData,se.fit=TRUE)$se.fit
NewData$cilow <- ilink(NewData$pred - (1.96 * NewData$se.fit))
NewData$cihi <- ilink(NewData$pred + (1.96 * NewData$se.fit))
NewData$Species<-"Cod"
Cod_Crab_NewData<-NewData

Plot_Crab_Temperature<-ggplot()+
  geom_ribbon(data=Cod_Crab_NewData, aes(temperature_at_bottom,ymin=cilow,ymax=cihi),fill="grey35",alpha=0.25)+
  geom_line(data=Cod_Crab_NewData, aes(temperature_at_bottom,fit),size=1)+
  #  facet_wrap(~region,scales="free")+
  ylab("Odds of crab in diet")+xlab("Bottom temperature (C)")+
 # ylim(0,1)+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key=element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
Plot_Crab_Temperature

DietDistribution_Plot<-ggplot()+
  geom_point(data=subset(Diet_Data,predator_species=="gadus_morhua"&year_surv==2015), aes(longitude,latitude,color=region))+
  ylab("latitude")+xlab("longitude")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  coord_equal()
DietDistribution_Plot

DietDistribution_Plot<-ggplot()+
  geom_point(data=subset(Diet_Data,predator_species=="reinhardtius_hippoglossoides"&year_surv==2015), aes(longitude,latitude,color=region))+
  ylab("latitude")+xlab("longitude")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
DietDistribution_Plot

DietDistribution_Plot<-ggplot()+
  geom_point(data=subset(Diet_Data,predator_species=="sebastes_mentella"&year_surv==2015), aes(longitude,latitude,color=region))+
  ylab("latitude")+xlab("longitude")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
DietDistribution_Plot



