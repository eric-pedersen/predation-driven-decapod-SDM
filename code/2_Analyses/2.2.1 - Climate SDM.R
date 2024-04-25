### Climate SDM MODELS ----
# Purpose: Create SDM for each species based on climate and depth
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
Data$gadus_morhua<-(Data$gadus_morhua/Data$tow_area)
Data$reinhardtius_hippoglossoides<-(Data$reinhardtius_hippoglossoides/Data$tow_area)
Data$sebastes_mentella<-(Data$sebastes_mentella/Data$tow_area)
Data$planktivores<-(Data$planktivores/Data$tow_area)
Data$chionoecetes_opilio_small<-(Data$chionoecetes_opilio_small/Data$tow_area)

# Fix Newfoundland region into fall and spring surveys
Data[region=="Newfoundland"&(month==9|month==10|month==11|month==12|month==1|month==2),region:="Newfoundland_fall"]
Data[region=="Newfoundland"&(month==3|month==4|month==5|month==6|month==7),region:="Newfoundland_spring"]

# Add topographical large regions
Data[region=="Newfoundland_fall"|region=="Newfoundland_spring",region_large:="NL_shelf"]
Data[region=="Gulf"|region=="Quebec",region_large:="SL_shelf"]
Data[region=="Maritimes",region_large:="SC_shelf"]
Data[region=="Arctic",region_large:="ART_shelf"]

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
                                                    ### Models ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

### General models (all regions)

# Northern shrimp
climate_shrimpmodel<-gam(pandalus_borealis~s(temperature_at_bottom,k=6)+
                         s(sqrt(depth),k=6) + s(region,year_surv, bs="re",by=dummy), #year random effects
                         family=tw(),
                         data=Data,
                         method="REML")

# Snow crab (using small crab <30cm carapace width)
climate_crabmodel<-gam(chionoecetes_opilio_small~s(temperature_at_bottom,k=6)+
                         s(sqrt(depth), region_large,bs="fs", k= 6) +
                         s(region,year_surv, bs="re",by=dummy), #year random effects
                         family=tw(),
                         data=Data,subset=(region=="Maritimes"|region=="Quebec"|region=="Gulf"|region=="Newfoundland_fall"|region=="Newfoundland_spring"),
                         method="REML")

# Atlantic cod
climate_codmodel<-gam(gadus_morhua~s(temperature_at_bottom,k=6)+
                      s(sqrt(depth),k=6) + s(region,year_surv, bs="re",by=dummy), #year random effects
                      family=tw(),
                      data=Data,
                      method="REML")

# Greenland halibut
climate_turbotmodel<-gam(reinhardtius_hippoglossoides~s(temperature_at_bottom,k=6)+
                         s(sqrt(depth),k=6) + s(region,year_surv, bs="re",by=dummy), #year random effects
                         family=tw(),
                         data=Data,
                         method="REML")

# Redfish
climate_redfishmodel<-gam(sebastes_mentella~s(temperature_at_bottom,k=6)+
                          s(sqrt(depth),k=6) + s(region,year_surv, bs="re",by=dummy), #year random effects
                          family=tw(),
                          data=Data,
                          method="REML")

# Planktivores
climate_planktivoremodel<-gam(planktivores~s(temperature_at_bottom,k=6)+
                              s(sqrt(depth),k=6) + s(region,year_surv, bs="re",by=dummy), #year random effects
                              family=tw(),
                              data=Data,
                              method="REML")

#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
                                                      ### Save models ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

save(climate_shrimpmodel, file = "code/outputs/models/climate/Shrimp_Climate.rda")
save(climate_turbotmodel, file = "code/outputs/models/climate/Turbot_Climate.rda")
save(climate_redfishmodel, file = "code/outputs/models/climate/Redfish_Climate.rda")
save(climate_crabmodel, file = "code/outputs/models/climate/Crab_Climate.rda")
save(climate_codmodel, file = "code/outputs/models/climate/Cod_Climate.rda")
save(climate_planktivoremodel, file = "code/outputs/models/climate/Planktivore_Climate.rda")
