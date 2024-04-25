### SMOOTHED ABUNDANCE SDM MODELS ----
# Purpose: Create smoothed abundance estimates based on GAM SDMs
# Author: S. Zabihi-Seissan

#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
                                                    ### Load packages ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

library(data.table)
library(googledrive)
library(mgcv)
library(sp)

#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
                                                ### Load data ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

# Abiotic data
Abiotic_Data<-fread('data/combined data/Abiotic_Data_Allregions.csv')

# RV data
RV_Data<-fread('data/combined data/RV_Data_Allregions.csv')

#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
                                                    ### Merge datasets ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

# Create merging ID
Abiotic_Data$ID<-paste(Abiotic_Data$region,Abiotic_Data$year_surv,Abiotic_Data$vessel,Abiotic_Data$trip,Abiotic_Data$set)
RV_Data$ID<-paste(RV_Data$region,RV_Data$year_surv,RV_Data$vessel,RV_Data$trip,RV_Data$set)

# Remove duplicate data columns from RV data
RV_Data[,c(1:10)]<-NULL

# Merge the datasets
Data<-merge(Abiotic_Data,RV_Data, by ="ID")

#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
                                                      ### Modify coordimate system ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

coordinates(Data)<-~longitude + latitude
proj4string(Data)<- CRS ("+init=epsg:4326") # WGS 84
CRS.new <- CRS("+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs") #determined using proj4string(NL)
Data<- spTransform(Data, CRS.new)
Data<-as.data.table(Data)

#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
                                                  ### Prepare data for modelling ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

# Standardize abundances based on tow_area
Data$pandalus_borealis<-(Data$pandalus_borealis/Data$tow_area)
Data$chionoecetes_opilio_small<-(Data$chionoecetes_opilio_small/Data$tow_area)
Data$gadus_morhua<-(Data$gadus_morhua/Data$tow_area)
Data$reinhardtius_hippoglossoides<-(Data$reinhardtius_hippoglossoides/Data$tow_area)
Data$sebastes_mentella<-(Data$sebastes_mentella/Data$tow_area)
Data$planktivores<-(Data$planktivores/Data$tow_area)

# Fix Newfoundland region into fall and spring surveys
Data[region=="Newfoundland"&(month==9|month==10|month==11|month==12|month==1|month==2),region:="Newfoundland_fall"]
Data[region=="Newfoundland"&(month==3|month==4|month==5|month==6|month==7),region:="Newfoundland_spring"]

rm(list=setdiff(ls(), c("Data")))

#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
                                                          ### Models ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

# Newfoundland fall models
nl_codmodel_fall<-gam(gadus_morhua~te(longitude,latitude,year_surv,d= c(2,1), k = c(60,15)),family=tw(),data=Data,subset=(region=="Newfoundland_fall"),method="REML")
nl_turbotmodel_fall<-gam(reinhardtius_hippoglossoides~te(longitude,latitude,year_surv,d= c(2,1), k = c(60,15)),family=tw(),data=Data,subset=(region=="Newfoundland_fall"),method="REML")
nl_redfishmodel_fall<-gam(sebastes_mentella~te(longitude,latitude,year_surv,d= c(2,1), k = c(60,15)),family=tw(),data=Data,subset=(region=="Newfoundland_fall"),method="REML")
nl_crabmodel_fall<-gam(chionoecetes_opilio~te(longitude,latitude,year_surv,d= c(2,1), k = c(60,15)),family=tw(),data=Data,subset=(region=="Newfoundland_fall"),method="REML")
nl_crabsmallmodel_fall<-gam(chionoecetes_opilio_small~te(longitude,latitude,year_surv,d= c(2,1), k = c(60,15)),family=tw(),data=Data,subset=(region=="Newfoundland_fall"),method="REML")
nl_shrimpmodel_fall<-gam(pandalus_borealis~te(longitude,latitude,year_surv,d= c(2,1), k = c(60,15)),family=tw(),data=Data,subset=(region=="Newfoundland_fall"),method="REML")
nl_planktivoremodel_fall<-gam(planktivores~te(longitude,latitude,year_surv,d= c(2,1), k = c(60,15)),family=tw(),data=Data,subset=(region=="Newfoundland_fall"),method="REML")

# Newfoundland spring models
nl_codmodel_spring<-gam(gadus_morhua~te(longitude,latitude,year_surv,d= c(2,1), k = c(60,15)),family=tw(),data=Data,subset=(region=="Newfoundland_spring"),method="REML")
nl_turbotmodel_spring<-gam(reinhardtius_hippoglossoides~te(longitude,latitude,year_surv,d= c(2,1), k = c(60,15)),family=tw(),data=Data,subset=(region=="Newfoundland_spring"),method="REML")
nl_redfishmodel_spring<-gam(sebastes_mentella~te(longitude,latitude,year_surv,d= c(2,1), k = c(60,15)),family=tw(),data=Data,subset=(region=="Newfoundland_spring"),method="REML")
nl_crabmodel_spring<-gam(chionoecetes_opilio~te(longitude,latitude,year_surv,d= c(2,1), k = c(60,15)),family=tw(),data=Data,subset=(region=="Newfoundland_spring"),method="REML")
nl_crabsmallmodel_spring<-gam(chionoecetes_opilio_small~te(longitude,latitude,year_surv,d= c(2,1), k = c(60,15)),family=tw(),data=Data,subset=(region=="Newfoundland_spring"),method="REML")
nl_shrimpmodel_spring<-gam(pandalus_borealis~te(longitude,latitude,year_surv,d= c(2,1), k = c(60,15)),family=tw(),data=Data,subset=(region=="Newfoundland_spring"),method="REML")
nl_planktivoremodel_spring<-gam(planktivores~te(longitude,latitude,year_surv,d= c(2,1), k = c(60,15)),family=tw(),data=Data,subset=(region=="Newfoundland_spring"),method="REML")

# Maritimes models
mar_codmodel<-gam(gadus_morhua~te(longitude,latitude,year_surv,d= c(2,1), k = c(60,15)),family=tw(),data=Data,subset=region=="Maritimes",method="REML")
mar_turbotmodel<-gam(reinhardtius_hippoglossoides~te(longitude,latitude,year_surv,d= c(2,1), k = c(60,15)),family=tw(),data=Data,subset=region=="Maritimes",method="REML")
mar_redfishmodel<-gam(sebastes_mentella~te(longitude,latitude,year_surv,d= c(2,1), k = c(60,15)),family=tw(),data=Data,subset=region=="Maritimes",method="REML")
mar_crabmodel<-gam(chionoecetes_opilio~te(longitude,latitude,year_surv,d= c(2,1), k = c(60,15)),family=tw(),data=Data,subset=region=="Maritimes",method="REML")
mar_crabsmallmodel<-gam(chionoecetes_opilio_small~te(longitude,latitude,year_surv,d= c(2,1), k = c(60,15)),family=tw(),data=Data,subset=region=="Maritimes",method="REML")
mar_shrimpmodel<-gam(pandalus_borealis~te(longitude,latitude,year_surv,d= c(2,1), k = c(60,15)),family=tw(),data=Data,subset=region=="Maritimes",method="REML")
mar_planktivoremodel<-gam(planktivores~te(longitude,latitude,year_surv,d= c(2,1), k = c(60,15)),family=tw(),data=Data,subset=region=="Maritimes",method="REML")

# Gulf Models
gul_codmodel<-gam(gadus_morhua~te(longitude,latitude,year_surv,d= c(2,1), k = c(60,15)),family=tw(),data=Data,subset=region=="Gulf",method="REML")
gul_turbotmodel<-gam(reinhardtius_hippoglossoides~te(longitude,latitude,year_surv,d= c(2,1), k = c(60,15)),family=tw(),data=Data,subset=region=="Gulf",method="REML")
gul_redfishmodel<-gam(sebastes_mentella~te(longitude,latitude,year_surv,d= c(2,1), k = c(60,15)),family=tw(),data=Data,subset=region=="Gulf",method="REML")
gul_crabmodel<-gam(chionoecetes_opilio~te(longitude,latitude,year_surv,d= c(2,1), k = c(60,15)),family=tw(),data=Data,subset=region=="Gulf",method="REML")
gul_crabsmallmodel<-gam(chionoecetes_opilio_small~te(longitude,latitude,year_surv,d= c(2,1), k = c(60,15)),family=tw(),data=Data,subset=region=="Gulf",method="REML")
gul_shrimpmodel<-gam(pandalus_borealis~te(longitude,latitude,year_surv,d= c(2,1), k = c(60,13)),family=tw(),data=Data,subset=region=="Gulf",method="REML")
gul_planktivoremodel<-gam(planktivores~te(longitude,latitude,year_surv,d= c(2,1), k = c(60,13)),family=tw(),data=Data,subset=region=="Gulf",method="REML")

# Quebec Models
qc_codmodel<-gam(gadus_morhua~te(longitude,latitude,year_surv,d= c(2,1), k = c(60,15)),family=tw(),data=Data,subset=region=="Quebec",method="REML")
qc_turbotmodel<-gam(reinhardtius_hippoglossoides~te(longitude,latitude,year_surv,d= c(2,1), k = c(60,15)),family=tw(),data=Data,subset=region=="Quebec",method="REML")
qc_redfishmodel<-gam(sebastes_mentella~te(longitude,latitude,year_surv,d= c(2,1), k = c(60,15)),family=tw(),data=Data,subset=region=="Quebec",method="REML")
qc_crabmodel<-gam(chionoecetes_opilio~te(longitude,latitude,year_surv,d= c(2,1), k = c(60,15)),family=tw(),data=Data,subset=region=="Quebec",method="REML")
qc_crabsmallmodel<-gam(chionoecetes_opilio_small~te(longitude,latitude,year_surv,d= c(2,1), k = c(60,15)),family=tw(),data=Data,subset=region=="Quebec",method="REML")
qc_shrimpmodel<-gam(pandalus_borealis~te(longitude,latitude,year_surv,d= c(2,1), k = c(60,15)),family=tw(),data=Data,subset=region=="Quebec",method="REML")
qc_planktivoremodel<-gam(planktivores~te(longitude,latitude,year_surv,d= c(2,1), k = c(60,15)),family=tw(),data=Data,subset=region=="Quebec",method="REML")

# Arctic Models
arc_codmodel<-gam(gadus_morhua~te(longitude,latitude,year_surv,d= c(2,1), k = c(60,10)),
                  family=tw(),data=Data,subset=region=="Arctic",method="REML")
arc_turbotmodel<-gam(reinhardtius_hippoglossoides~te(longitude,latitude,year_surv,d= c(2,1), k = c(60,10)),family=tw(),data=Data,subset=region=="Arctic",method="REML")
arc_redfishmodel<-gam(sebastes_mentella~te(longitude,latitude,year_surv,d= c(2,1), k = c(60,10)),family=tw(),data=Data,subset=region=="Arctic",method="REML")
arc_shrimpmodel<-gam(pandalus_borealis~te(longitude,latitude,year_surv,d= c(2,1), k = c(60,10)),family=tw(),data=Data,subset=region=="Arctic",method="REML")
arc_planktivoremodel<-gam(planktivores~te(longitude,latitude,year_surv,d= c(2,1), k = c(60,10)),family=tw(),data=Data,subset=region=="Arctic",method="REML")


                                                        ### Save Models ----

# Newfoundland fall
save(nl_codmodel_fall, file = "code/outputs/models/spatial/Newfoundland_Fall_Cod_Spatial.rda")
save(nl_turbotmodel_fall, file = "code/outputs/models/spatial/Newfoundland_Fall_Turbot_Spatial.rda")
save(nl_redfishmodel_fall, file = "code/outputs/models/spatial/Newfoundland_Fall_Redfish_Spatial.rda")
save(nl_crabmodel_fall, file = "code/outputs/models/spatial/Newfoundland_Fall_Crab_Spatial.rda")
save(nl_crabsmallmodel_fall,file = "code/outputs/models/spatial/Newfoundland_Fall_CrabSmall_Spatial.rda")
save(nl_shrimpmodel_fall,file="code/outputs/models/spatial/Newfoundland_Fall_NorthShrimp_Spatial.rda")
save(nl_planktivoremodel_fall,file="code/outputs/models/spatial/Newfoundland_Fall_Planktivore_Spatial.rda")

# Newfoundland spring
save(nl_codmodel_spring, file = "code/outputs/models/spatial/Newfoundland_Spring_Cod_Spatial.rda")
save(nl_turbotmodel_spring, file = "code/outputs/models/spatial/Newfoundland_Spring_Turbot_Spatial.rda")
save(nl_redfishmodel_spring, file = "code/outputs/models/spatial/Newfoundland_Spring_Redfish_Spatial.rda")
save(nl_crabmodel_spring, file = "code/outputs/models/spatial/Newfoundland_Spring_Crab_Spatial.rda")
save(nl_crabsmallmodel_spring,file = "code/outputs/models/spatial/Newfoundland_Spring_CrabSmall_Spatial.rda")
save(nl_shrimpmodel_spring,file="code/outputs/models/spatial/Newfoundland_Spring_NorthShrimp_Spatial.rda")
save(nl_planktivoremodel_spring,file="code/outputs/models/spatial/Newfoundland_Spring_Planktivore_Spatial.rda")

# Maritimes
save(mar_codmodel, file = "code/outputs/models/spatial/Maritimes_Cod_Spatial.rda")
save(mar_turbotmodel, file = "code/outputs/models/spatial/Maritimes_Turbot_Spatial.rda")
save(mar_redfishmodel, file = "code/outputs/models/spatial/Maritimes_Redfish_Spatial.rda")
save(mar_crabmodel, file = "code/outputs/models/spatial/Maritimes_Crab_Spatial.rda")
save(mar_crabsmallmodel,file = "code/outputs/models/spatial/Maritimes_CrabSmall_Spatial.rda")
save(mar_shrimpmodel,file="code/outputs/models/spatial/Maritimes_NorthShrimp_Spatial.rda")
save(mar_planktivoremodel,file="code/outputs/models/spatial/Maritimes_Planktivore_Spatial.rda")

# Gulf
save(gul_codmodel, file = "code/outputs/models/spatial/Gulf_Cod_Spatial.rda")
save(gul_turbotmodel, file = "code/outputs/models/spatial/Gulf_Turbot_Spatial.rda")
save(gul_redfishmodel, file = "code/outputs/models/spatial/Gulf_Redfish_Spatial.rda")
save(gul_crabmodel, file = "code/outputs/models/spatial/Gulf_Crab_Spatial.rda")
save(gul_crabsmallmodel,file = "code/outputs/models/spatial/Gulf_CrabSmall_Spatial.rda")
save(gul_shrimpmodel,file="code/outputs/models/spatial/Gulf_NorthShrimp_Spatial.rda")
save(gul_planktivoremodel,file="code/outputs/models/spatial/Gulf_Planktivore_Spatial.rda")

# Quebec
save(qc_codmodel, file = "code/outputs/models/spatial/Quebec_Cod_Spatial.rda")
save(qc_turbotmodel, file = "code/outputs/models/spatial/Quebec_Turbot_Spatial.rda")
save(qc_redfishmodel, file = "code/outputs/models/spatial/Quebec_Redfish_Spatial.rda")
save(qc_crabmodel, file = "code/outputs/models/spatial/Quebec_Crab_Spatial.rda")
save(qc_crabsmallmodel,file = "code/outputs/models/spatial/Quebec_CrabSmall_Spatial.rda")
save(qc_shrimpmodel,file="code/outputs/models/spatial/Quebec_NorthShrimp_Spatial.rda")
save(qc_planktivoremodel,file="code/outputs/models/spatial/Quebec_Planktivore_Spatial.rda")

# Arctic
save(arc_codmodel, file = "code/outputs/models/spatial/Arctic_Cod_Spatial.rda")
save(arc_turbotmodel, file = "code/outputs/models/spatial/Arctic_Turbot_Spatial.rda")
save(arc_redfishmodel, file = "code/outputs/models/spatial/Arctic_Redfish_Spatial.rda")
save(arc_shrimpmodel,file="code/outputs/models/spatial/Arctic_NorthShrimp_Spatial.rda")
save(arc_planktivoremodel,file="code/outputs/models/spatial/Arctic_Planktivore_Spatial.rda")
