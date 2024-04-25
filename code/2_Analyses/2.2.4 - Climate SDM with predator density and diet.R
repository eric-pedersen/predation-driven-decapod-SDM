### Climate SDM MODELS + Predator Densities * Diet predictions ----
# Purpose: Create SDM for shrimp and crab including predator densities and predicted probability in diet as variables
# Author: S. Zabihi-Seissan

#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
                                                          ### Load packages ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

library(data.table)
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

# Fix Newfoundland region into fall and spring surveys
Data[region=="Newfoundland"&(month==9|month==10|month==11|month==12|month==1|month==2),region:="Newfoundland_fall"]
Data[region=="Newfoundland"&(month==3|month==4|month==5|month==6|month==7),region:="Newfoundland_spring"]

#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
                                              ### Modify coordimate system ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

coordinates(Data)<-~longitude + latitude
proj4string(Data)<- CRS ("+init=epsg:4326") # WGS 84
CRS.new <- CRS("+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs") #determined using proj4string(NL)
Data<- spTransform(Data, CRS.new)
Data<-as.data.table(Data)

#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
                                        ### Add predicted predator densities ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

# Load models
models <- list.files("code/outputs/models/climate", pattern="*.rda", full.names=TRUE)
lapply(models,load,.GlobalEnv)
models_diet <- list.files("code/outputs/models/diet", pattern="*.rda", full.names=TRUE)
lapply(models_diet,load,.GlobalEnv)


# Replace year_surv from current to previous year (lag)
Data$year_current<-Data$year_surv
Data$year_surv<-(Data$year_current-1)
Data$year_surv<-as.integer(Data$year_surv)
Data$dummy<-1

# Add topographical large regions
Data[region=="Newfoundland_fall"|region=="Newfoundland_spring",region_large:="NL_shelf"]
Data[region=="Gulf"|region=="Quebec",region_large:="SL_shelf"]
Data[region=="Maritimes",region_large:="SC_shelf"]
Data[region=="Arctic",region_large:="ART_shelf"]
Data$region_large<-as.factor(Data$region_large)

## Predict values by region (Climate models)
# Keep years where diets were sampled
Data<-Data[(region=="Arctic"&year_current==2018)|
             (region=="Gulf"&(year_current==2004|year_current==2005|year_current==2006))|
             (region=="Maritimes"&(year_current>2007&year_current<2017))|
             (region=="Quebec"&((year_current>1992&year_current<2010)|(year_current>2014&year_current<2019)))|
             (region=="Newfoundland_fall"&((year_current>1994&year_current<2002)|(year_current>2002&year_current<2018)))|
             (region=="Newfoundland_spring"&((year_current>1995&year_current<1998)|(year_current>2012&year_current<2019)))]

#setting shrimp and crab densities to 1 for predicting diet present to give
#pre-capita predation probabilities 
Data$pred_shrimp <- 1 
Data$pred_crab <- 1 


# Arctic
Data_Arctic<-Data[region=="Arctic"]
Data_Arctic$pred_cod<-predict(climate_codmodel,newdata=Data_Arctic,type="response")
Data_Arctic$pred_turbot<-predict(climate_turbotmodel,newdata=Data_Arctic,type="response")
Data_Arctic$pred_redfish<-predict(climate_redfishmodel,newdata=Data_Arctic,type="response")
Data_Arctic$pred_planktivores<-predict(climate_planktivoremodel,newdata=Data_Arctic,type="response")
#Data_Arctic$pred_crab<-NA

# Gulf
Data_Gulf<-Data[region=="Gulf"]
Data_Gulf$pred_cod<-predict(climate_codmodel,newdata=Data_Gulf,type="response")
Data_Gulf$pred_turbot<-predict(climate_turbotmodel,newdata=Data_Gulf,type="response")
Data_Gulf$pred_redfish<-predict(climate_redfishmodel,newdata=Data_Gulf,type="response")
Data_Gulf$pred_planktivores<-predict(climate_planktivoremodel,newdata=Data_Gulf,type="response")

# Maritimes
Data_Maritimes<-Data[region=="Maritimes"]
Data_Maritimes$pred_cod<-predict(climate_codmodel,newdata=Data_Maritimes,type="response")
Data_Maritimes$pred_turbot<-predict(climate_turbotmodel,newdata=Data_Maritimes,type="response")
Data_Maritimes$pred_redfish<-predict(climate_redfishmodel,newdata=Data_Maritimes,type="response")
Data_Maritimes$pred_planktivores<-predict(climate_planktivoremodel,newdata=Data_Maritimes,type="response")

# Quebec
Data_Quebec<-Data[region=="Quebec"]
Data_Quebec$pred_cod<-predict(climate_codmodel,newdata=Data_Quebec,type="response")
Data_Quebec$pred_turbot<-predict(climate_turbotmodel,newdata=Data_Quebec,type="response")
Data_Quebec$pred_redfish<-predict(climate_redfishmodel,newdata=Data_Quebec,type="response")
Data_Quebec$pred_planktivores<-predict(climate_planktivoremodel,newdata=Data_Quebec,type="response")

# Newfoundland fall
Data_Newfoundland_fall<-Data[region=="Newfoundland_fall"]
Data_Newfoundland_fall$pred_cod<-predict(climate_codmodel,newdata=Data_Newfoundland_fall,type="response")
Data_Newfoundland_fall$pred_turbot<-predict(climate_turbotmodel,newdata=Data_Newfoundland_fall,type="response")
Data_Newfoundland_fall$pred_redfish<-predict(climate_redfishmodel,newdata=Data_Newfoundland_fall,type="response")
Data_Newfoundland_fall$pred_planktivores<-predict(climate_planktivoremodel,newdata=Data_Newfoundland_fall,type="response")

# Newfoundland spring
Data_Newfoundland_spring<-Data[region=="Newfoundland_spring"]
Data_Newfoundland_spring$pred_cod<-predict(climate_codmodel,newdata=Data_Newfoundland_spring,type="response")
Data_Newfoundland_spring$pred_turbot<-predict(climate_turbotmodel,newdata=Data_Newfoundland_spring,type="response")
Data_Newfoundland_spring$pred_redfish<-predict(climate_redfishmodel,newdata=Data_Newfoundland_spring,type="response")
Data_Newfoundland_spring$pred_planktivores<-predict(climate_planktivoremodel,newdata=Data_Newfoundland_spring,type="response")

# Re-merge data
Data<-rbind(Data_Arctic,Data_Maritimes,Data_Gulf,Data_Quebec,Data_Newfoundland_fall,Data_Newfoundland_spring)
rm(list=setdiff(ls(), c("Data","cod_shrimp_presence","turbot_shrimp_presence","redfish_shrimp_presence","cod_crab_presence")))

# Predict diet data by species
Data$dummy<-1
Data$cod_shrimp<-predict(cod_shrimp_presence,newdata=Data,type="response")
Data[year_surv==1992,cod_shrimp:=NA]
Data$turbot_shrimp<-predict(turbot_shrimp_presence,newdata=Data,type="response")
Data[year_surv==1992,turbot_shrimp:=NA]
Data$cod_crab<-predict(cod_crab_presence,newdata=Data,type="response")
Data[year_surv==1992,cod_crab:=NA]
Data[region=="Arctic",cod_crab:=NA]
Data$redfish_shrimp<-predict(redfish_shrimp_presence,newdata=Data,type="response")
Data[year_surv==1992|year_surv==1998|year_surv==2000|year_surv==2001|year_surv==2002|year_surv==2003|year_surv==2007,redfish_shrimp:=NA]

# Create predation index for each predator species
Data$pred_index_shrimp<-((Data$pred_cod*Data$cod_shrimp)+(Data$pred_turbot*Data$turbot_shrimp)+(Data$pred_redfish*Data$redfish_shrimp))
Data$pred_index_crab<-(Data$pred_cod*Data$cod_crab)

#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
### Prepare data for modelling ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

# Revert years back to current
Data$year_surv<-Data$year_current
Data$year_surv<-as.factor(Data$year_surv)

# Standardize abundances based on tow_area
Data$pandalus_borealis<-(Data$pandalus_borealis/Data$tow_area)
Data$chionoecetes_opilio_small<-(Data$chionoecetes_opilio_small/Data$tow_area)

# Set year_surv as factor
Data$year_surv<-as.factor(Data$year_surv)
Data$region<-as.factor(Data$region)
Data$vessel<-as.factor(Data$vessel)
Data$region_large<-as.factor(Data$region_large)

# Add dummy variable to plot marginal effects (removal of random effects)
Data$dummy<-1

# Remove problematic temperature data
Data<-Data[!ID=="Gulf 2003 TEL  16"]
Data<-Data[!ID=="Gulf 1983 TEM  35"]
Data<-Data[!ID=="Quebec 1994 NED 5 56"]
Data<-Data[!ID=="Gulf 2015 TEL  135"]
Data<-Data[!ID=="Quebec 1999 NED 10 72"]
Data<-Data[!ID=="Gulf 2015 TEL  75"]
Data<-Data[!ID=="Maritimes 1991 HAM HAM1991231 42"] #Maybe for cod
Data<-Data[!ID=="Newfoundland 2010 NED 943 54"] #Maybe for cod

rm(list=setdiff(ls(), c("Data")))

#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
### General models (all regions)
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

# Northern shrimp
climate_shrimpmodel<-gam(pandalus_borealis~s(temperature_at_bottom,k=6)+
                           s(sqrt(depth),k=6) +
                           pred_index_shrimp+
                           s(region,year_surv, bs="re",by=dummy), #year-by-region random effects
                         family=tw(),
                         data=Data,
                         method="REML")

#Snow crab
climate_crabmodel<-gam(chionoecetes_opilio_small~s(temperature_at_bottom,k=6)+
                         s(sqrt(depth), region_large,bs="fs", k= 6) +
                         pred_index_crab+
                         s(region,year_surv, bs="re",by=dummy), #year-by-region random effects
                       family=tw(),
                       data=Data,subset=(region=="Maritimes"|region=="Quebec"|region=="Gulf"|region=="Newfoundland_fall"|region=="Newfoundland_spring"),
                       method="REML")

#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
### Save Models ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

save(climate_shrimpmodel, file = "code/outputs/models/climate/Climate_Predator_Index/Shrimp_Climate_Pred_Index.rda")
save(climate_crabmodel, file = "code/outputs/models/climate/Climate_Predator_Index/Crab_Climate_Pred_Index.rda")
