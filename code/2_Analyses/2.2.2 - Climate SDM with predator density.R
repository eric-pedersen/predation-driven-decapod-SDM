### Climate SDM MODELS + Predator Densities ----
# Purpose: Create SDM for shrimp and crab including predator densities as variables
# Author: S. Zabihi-Seissan

#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
                                                        ### Load packages ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

library(data.table)
library(mgcv)
library(sp)
library(lme4)

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

# Add topographical large regions
Data[region=="Newfoundland_fall"|region=="Newfoundland_spring",region_large:="NL_shelf"]
Data[region=="Gulf"|region=="Quebec",region_large:="SL_shelf"]
Data[region=="Maritimes",region_large:="SC_shelf"]
Data[region=="Arctic",region_large:="ART_shelf"]
Data$region_large<-as.factor(Data$region_large)

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

# Replace year_surv from current to previous year (lag)
Data$year_current<-Data$year_surv
Data$year_surv<-(Data$year_current-1)
Data$year_surv<-as.integer(Data$year_surv)
Data$dummy<-1

## Predict values by region

# Arctic
Data_Arctic<-Data[region=="Arctic"]
Data_Arctic$pred_cod<-predict(climate_codmodel,newdata=Data_Arctic,type="response")
Data_Arctic$pred_turbot<-predict(climate_turbotmodel,newdata=Data_Arctic,type="response")
Data_Arctic$pred_redfish<-predict(climate_redfishmodel,newdata=Data_Arctic,type="response")

# Gulf
Data_Gulf<-Data[region=="Gulf"]
Data_Gulf$pred_cod<-predict(climate_codmodel,newdata=Data_Gulf,type="response")
Data_Gulf$pred_turbot<-predict(climate_turbotmodel,newdata=Data_Gulf,type="response")
Data_Gulf$pred_redfish<-predict(climate_redfishmodel,newdata=Data_Gulf,type="response")

# Maritimes
Data_Maritimes<-Data[region=="Maritimes"&(!year_surv==1969)]
Data_Maritimes$pred_cod<-predict(climate_codmodel,newdata=Data_Maritimes,type="response")
Data_Maritimes$pred_turbot<-predict(climate_turbotmodel,newdata=Data_Maritimes,type="response")
Data_Maritimes$pred_redfish<-predict(climate_redfishmodel,newdata=Data_Maritimes,type="response")

# Quebec
Data_Quebec<-Data[region=="Quebec"]
Data_Quebec$pred_cod<-predict(climate_codmodel,newdata=Data_Quebec,type="response")
Data_Quebec$pred_turbot<-predict(climate_turbotmodel,newdata=Data_Quebec,type="response")
Data_Quebec$pred_redfish<-predict(climate_redfishmodel,newdata=Data_Quebec,type="response")

# Newfoundland fall
Data_Newfoundland_fall<-Data[region=="Newfoundland_fall"]
Data_Newfoundland_fall$pred_cod<-predict(climate_codmodel,newdata=Data_Newfoundland_fall,type="response")
Data_Newfoundland_fall$pred_turbot<-predict(climate_turbotmodel,newdata=Data_Newfoundland_fall,type="response")
Data_Newfoundland_fall$pred_redfish<-predict(climate_redfishmodel,newdata=Data_Newfoundland_fall,type="response")

# Newfoundland spring
Data_Newfoundland_spring<-Data[region=="Newfoundland_spring"]
Data_Newfoundland_spring$pred_cod<-predict(climate_codmodel,newdata=Data_Newfoundland_spring,type="response")
Data_Newfoundland_spring$pred_turbot<-predict(climate_turbotmodel,newdata=Data_Newfoundland_spring,type="response")
Data_Newfoundland_spring$pred_redfish<-predict(climate_redfishmodel,newdata=Data_Newfoundland_spring,type="response")

# Re-merge data
Data<-rbind(Data_Arctic,Data_Maritimes,Data_Gulf,Data_Quebec,Data_Newfoundland_fall,Data_Newfoundland_spring)
rm(list=setdiff(ls(), c("Data")))

#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
                                            ### Prepare data for modelling ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

# Revert years back to current
Data$year_surv<-Data$year_current

# Remove year 1996 for Newfoundland fall ### NEED TO FIGURE OUT WHY ###
Data<-Data[!(region=="Newfoundland_fall"&year_surv==1996)]

# Standardize pandalus_borealis and small crab based on tow_area
Data$pandalus_borealis<-(Data$pandalus_borealis/Data$tow_area)
Data$chionoecetes_opilio_small<-(Data$chionoecetes_opilio_small/Data$tow_area)

# Set year_surv as factor
Data$year_surv<-as.factor(Data$year_surv)
Data$region<-as.factor(Data$region)
Data$vessel<-as.factor(Data$vessel)

# Add dummy variable to plot marginal effects (removal of random effects)
Data$dummy<-1

# Remove problematic temperature data and biomass data
Data<-Data[!ID=="Gulf 2003 TEL  16"]
Data<-Data[!ID=="Gulf 1983 TEM  35"]
Data<-Data[!ID=="Quebec 1994 NED 5 56"]
Data<-Data[!ID=="Gulf 2015 TEL  135"]
Data<-Data[!ID=="Quebec 1999 NED 10 72"]
Data<-Data[!ID=="Gulf 2015 TEL  75"]
Data<-Data[!ID=="Maritimes 1991 HAM HAM1991231 42"] #Maybe for cod
Data<-Data[!ID=="Newfoundland 2010 NED 943 54"] #Maybe for cod
Data<-Data[!ID=="Quebec 2001 NED 12 202"] # Extreme large shrimp biomass
Data<-Data[!ID=="Newfoundland 2008 TEM 829 29"] # Extremely large redfish biomass

rm(list=setdiff(ls(), c("Data")))

# Save output for later use
fwrite(Data,file="code/outputs/data/2.2.2_climate_predator_density.csv")

#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
                                              ### General models (all regions)
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

# Northern shrimp
climate_shrimpmodel<-gam(pandalus_borealis~s(temperature_at_bottom,k=6)+
                           s(sqrt(depth),k=6) +
                           pred_cod+
                           pred_turbot+
                           pred_redfish+
                           s(region,year_surv, bs="re",by=dummy), #year-by-region random effects
                         family=tw(),
                         data=Data,
                         method="REML")

# Snow crab
climate_crabmodel<-gam(chionoecetes_opilio_small~s(temperature_at_bottom,k=6)+
                         s(sqrt(depth), region_large,bs="fs", k= 6) +
                         pred_cod+
                         s(region,year_surv, bs="re",by=dummy), #year-by-region random effects
                       family=tw(),
                       data=Data,subset=(region=="Maritimes"|region=="Quebec"|region=="Gulf"|region=="Newfoundland_fall"|region=="Newfoundland_spring"),
                       method="REML")

#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
                                                    ### Save Models ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

save(climate_shrimpmodel, file = "code/outputs/models/climate/Climate_Predator_Density/Shrimp_Climate_Pred_Density.rda")
save(climate_crabmodel, file = "code/outputs/models/climate/Climate_Predator_Density/Crab_Climate_Pred_Density.rda")
