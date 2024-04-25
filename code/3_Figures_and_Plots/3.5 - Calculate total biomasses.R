### Projecting shrimp and crab models using BNAM for 2075 ----
# Purpose: Caoculate total biomass from long term average and 2075 future projections.
# Author: S. Zabihi-Seissan

# Load packages
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
library(sf)
library(data.table)
library(mgcv)
library(ggplot2)
library(viridis)
library(grid)
library(rgdal)
library(ggpubr)
library(devtools)


#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
#### Load Data ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

# Load BNAM and depth data
BNAM_Data<-fread('code/outputs/data/BNAM_Data_Output.csv')
BNAM_Data$region<-as.factor(BNAM_Data$region)



#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
#### Formatting data for projections ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

# Remove unnecessary column from anomaly data
Data<-melt(BNAM_Data, id.vars = c("x","y","depth","region"), measure.vars = c("BNAM_1995_mean","BNAM_1996_mean","BNAM_2005_mean","BNAM_2006_mean","BNAM_2015_mean","BNAM_2016_mean","BNAM_2075_mean","BNAM_total_mean"))

# Rename groups and columns to match models
colnames(Data)[colnames(Data)=="value"] <- "temperature_at_bottom"
Data[variable=="BNAM_1995_mean",year_surv:="1995"]
Data[variable=="BNAM_1996_mean",year_surv:="1996"]
Data[variable=="BNAM_2005_mean",year_surv:="2005"]
Data[variable=="BNAM_2006_mean",year_surv:="2006"]
Data[variable=="BNAM_2015_mean",year_surv:="2015"]
Data[variable=="BNAM_2016_mean",year_surv:="2016"]
Data[variable=="BNAM_2075_mean",year_surv:="2075"]
Data[variable=="BNAM_total_mean",year_surv:="0000"]
Data$variable<-NULL
Data$year_surv<-as.factor(Data$year_surv)
Data<-Data[year_surv=="2075"|year_surv=="0000"]
Data$dummy<-0
Data<-Data[depth<0]
Data$depth<-(-(Data$depth))

# Add topographical large regions
Data[region=="Newfoundland_fall"|region=="Newfoundland_spring",region_large:="NL_shelf"]
Data[region=="Gulf"|region=="Quebec",region_large:="SL_shelf"]
Data[region=="Maritimes",region_large:="SC_shelf"]
Data[region=="Arctic",region_large:="ART_shelf"]
Data$region_large<-as.factor(Data$region_large)


#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
#### Projecting climate only models ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

# Load climate only SDMs
models <- list.files("code/outputs/models/climate", pattern="*.rda", full.names=TRUE)
lapply(models,load,.GlobalEnv)



Data$pred_shrimp<-predict(climate_shrimpmodel,newdata = Data,type="response")
Data$se.fit_shrimp<-predict(climate_shrimpmodel,newdata = Data,se.fit=TRUE,type="response")$se.fit
Data$cilow_shrimp <- (Data$pred_shrimp - (1.96 * Data$se.fit_shrimp))
Data$cihi_shrimp <- (Data$pred_shrimp + (1.96 * Data$se.fit_shrimp))
Data$pred_crab<-predict(climate_crabmodel,newdata= Data,type="response")
Data$se.fit_crab<-predict(climate_crabmodel,newdata = Data,se.fit=TRUE,type="response")$se.fit
Data$cilow_crab <- (Data$pred_crab - (1.96 * Data$se.fit_crab))
Data$cihi_crab <- (Data$pred_crab + (1.96 * Data$se.fit_crab))

# Store climate only shrimp and crab outputs
Data$shrimp_climate<-Data$pred_shrimp
Data$shrimp_climate_low<-Data$cilow_shrimp
Data$shrimp_climate_hi<-Data$cihi_shrimp
Data$crab_climate<-Data$pred_crab
Data$crab_climate_low<-Data$cilow_crab
Data$crab_climate_hi<-Data$cihi_crab

rm(list=setdiff(ls(), "Data"))


#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
#### Projecting climate+predator density models ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

# Load climate only and diet models for lagged predator density projection
models <- list.files("code/outputs/models/climate", pattern="*.rda", full.names=TRUE)
lapply(models,load,.GlobalEnv)


# Predict lagged predator density (Not actually lagged for 2075 predictions)
Data$pred_cod<-predict(climate_codmodel,newdata = Data,type="response")
Data$pred_turbot<-predict(climate_turbotmodel,newdata = Data,type="response")
Data$pred_redfish<-predict(climate_redfishmodel,newdata = Data,type="response")

rm(list=setdiff(ls(), "Data"))

# Load climate+predator SDMs
models <- list.files("code/outputs/models/climate/Climate_Predator_Density", pattern="*.rda", full.names=TRUE)
lapply(models,load,.GlobalEnv)

Data$pred_shrimp<-predict(climate_shrimpmodel,newdata = Data,type="response")
Data$se.fit_shrimp<-predict(climate_shrimpmodel,newdata = Data,se.fit=TRUE,type="response")$se.fit
Data$cilow_shrimp <- (Data$pred_shrimp - (1.96 * Data$se.fit_shrimp))
Data$cihi_shrimp <- (Data$pred_shrimp + (1.96 * Data$se.fit_shrimp))
Data$pred_crab<-predict(climate_crabmodel,newdata= Data,type="response")
Data$se.fit_crab<-predict(climate_crabmodel,newdata = Data,se.fit=TRUE,type="response")$se.fit
Data$cilow_crab <- (Data$pred_crab - (1.96 * Data$se.fit_crab))
Data$cihi_crab <- (Data$pred_crab + (1.96 * Data$se.fit_crab))

# Store climate only shrimp and crab outputs
Data$shrimp_climatepred<-Data$pred_shrimp
Data$shrimp_climatepred_low<-Data$cilow_shrimp
Data$shrimp_climatepred_hi<-Data$cihi_shrimp
Data$crab_climatepred<-Data$pred_crab
Data$crab_climatepred_low<-Data$cilow_crab
Data$crab_climatepred_hi<-Data$cihi_crab

rm(list=setdiff(ls(), "Data"))


#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
#### Projecting climate+predator index models ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

# Load climate only for lagged predator density projection
models <- list.files("code/outputs/models/climate", pattern="*.rda", full.names=TRUE)
lapply(models,load,.GlobalEnv)
models_diet <- list.files("code/outputs/models/diet", pattern="*.rda", full.names=TRUE)
lapply(models_diet,load,.GlobalEnv)

# Predict species density based on climate only (Not actually lagged for 2075 predictions)
Data$pred_cod<-predict(climate_codmodel,newdata = Data,type="response")
Data$pred_turbot<-predict(climate_turbotmodel,newdata = Data,type="response")
Data$pred_redfish<-predict(climate_redfishmodel,newdata = Data,type="response")
Data$pred_shrimp<-predict(climate_shrimpmodel,newdata = Data,type="response")
Data$pred_crab<-predict(climate_crabmodel,newdata = Data,type="response")
Data$pred_planktivores<-predict(climate_planktivoremodel,newdata = Data,type="response")

# Predict diet by species
Data$cod_shrimp<-predict(cod_shrimp_presence,newdata=Data,type="response")
Data$turbot_shrimp<-predict(turbot_shrimp_presence,newdata=Data,type="response")
Data$redfish_shrimp<-predict(redfish_shrimp_presence,newdata=Data,type="response")
Data$cod_crab<-predict(cod_crab_presence,newdata=Data,type="response")

# Create predation index for each predator species
Data$pred_index_shrimp<-((Data$pred_cod*Data$cod_shrimp)+(Data$pred_turbot*Data$turbot_shrimp)+(Data$pred_redfish*Data$redfish_shrimp))
Data$pred_index_crab<-(Data$pred_cod*Data$cod_crab)

rm(list=setdiff(ls(), "Data"))

# Load climate + predator index models
models <- list.files("code/outputs/models/climate/Climate_Predator_Index", pattern="*.rda", full.names=TRUE)
lapply(models,load,.GlobalEnv)

# Predict models
Data$pred_shrimp<-predict(climate_shrimpmodel,newdata = Data,type="response")
Data$se.fit_shrimp<-predict(climate_shrimpmodel,newdata = Data,se.fit=TRUE,type="response")$se.fit
Data$cilow_shrimp <- (Data$pred_shrimp - (1.96 * Data$se.fit_shrimp))
Data$cihi_shrimp <- (Data$pred_shrimp + (1.96 * Data$se.fit_shrimp))
Data$pred_crab<-predict(climate_crabmodel,newdata= Data,type="response")
Data$se.fit_crab<-predict(climate_crabmodel,newdata = Data,se.fit=TRUE,type="response")$se.fit
Data$cilow_crab <- (Data$pred_crab - (1.96 * Data$se.fit_crab))
Data$cihi_crab <- (Data$pred_crab + (1.96 * Data$se.fit_crab))

Data$shrimp_climateind<-Data$pred_shrimp
Data$shrimp_climateind_low<-Data$cilow_shrimp
Data$shrimp_climateind_hi<-Data$cihi_shrimp
Data$crab_climateind<-Data$pred_crab
Data$crab_climateind_low<-Data$cilow_crab
Data$crab_climateind_hi<-Data$cihi_crab

rm(list=setdiff(ls(), "Data"))

#Format data
Data_shrimp_predicted<-melt(Data, id.vars = c("x","y","depth","temperature_at_bottom","year_surv","region","dummy"), measure.vars = c("shrimp_climate","shrimp_climatepred","shrimp_climateind"))
Data_shrimp_predicted$predicted<-Data_shrimp_predicted$value
Data_shrimp_predicted$value<-NULL
Data_shrimp_low<-melt(Data, id.vars = c("x","y","depth","temperature_at_bottom","year_surv","region","dummy"), measure.vars = c("shrimp_climate_low","shrimp_climatepred_low","shrimp_climateind_low"))
Data_shrimp_low$low<-Data_shrimp_low$value
Data_shrimp_low$value<-NULL
Data_shrimp_low[variable=="shrimp_climate_low",variable:="shrimp_climate"]
Data_shrimp_low[variable=="shrimp_climatepred_low",variable:="shrimp_climatepred"]
Data_shrimp_low[variable=="shrimp_climateind_low",variable:="shrimp_climateind"]
Data_shrimp_high<-melt(Data, id.vars = c("x","y","depth","temperature_at_bottom","year_surv","region","dummy"), measure.vars = c("shrimp_climate_hi","shrimp_climatepred_hi","shrimp_climateind_hi"))
Data_shrimp_high$high<-Data_shrimp_high$value
Data_shrimp_high$value<-NULL
Data_shrimp_high[variable=="shrimp_climate_hi",variable:="shrimp_climate"]
Data_shrimp_high[variable=="shrimp_climatepred_hi",variable:="shrimp_climatepred"]
Data_shrimp_high[variable=="shrimp_climateind_hi",variable:="shrimp_climateind"]

Data_crab_predicted<-melt(Data, id.vars = c("x","y","depth","temperature_at_bottom","year_surv","region","dummy"), measure.vars = c("crab_climate","crab_climatepred","crab_climateind"))
Data_crab_predicted$predicted<-Data_crab_predicted$value
Data_crab_predicted$value<-NULL
Data_crab_low<-melt(Data, id.vars = c("x","y","depth","temperature_at_bottom","year_surv","region","dummy"), measure.vars = c("crab_climate_low","crab_climatepred_low","crab_climateind_low"))
Data_crab_low$low<-Data_crab_low$value
Data_crab_low$value<-NULL
Data_crab_low[variable=="crab_climate_low",variable:="crab_climate"]
Data_crab_low[variable=="crab_climatepred_low",variable:="crab_climatepred"]
Data_crab_low[variable=="crab_climateind_low",variable:="crab_climateind"]
Data_crab_high<-melt(Data, id.vars = c("x","y","depth","temperature_at_bottom","year_surv","region","dummy"), measure.vars = c("crab_climate_hi","crab_climatepred_hi","crab_climateind_hi"))
Data_crab_high$high<-Data_crab_high$value
Data_crab_high$value<-NULL
Data_crab_high[variable=="crab_climate_hi",variable:="crab_climate"]
Data_crab_high[variable=="crab_climatepred_hi",variable:="crab_climatepred"]
Data_crab_high[variable=="crab_climateind_hi",variable:="crab_climateind"]

Data_Shrimp<-merge(Data_shrimp_predicted,Data_shrimp_low,by =c("x","y","depth",
                                                           "region","dummy","year_surv","temperature_at_bottom","variable"),allow.cartesian=TRUE)
Data_Shrimp<-merge(Data_Shrimp,Data_shrimp_high,by=c("x","y","depth",
                                                     "region","dummy","variable","year_surv","temperature_at_bottom"),allow.cartesian=TRUE)

Data_Crab<-merge(Data_crab_predicted,Data_crab_low,by =c("x","y","depth",
                                                               "region","dummy","year_surv","temperature_at_bottom","variable"),allow.cartesian=TRUE)
Data_Crab<-merge(Data_Crab,Data_crab_high,by=c("x","y","depth",
                                                     "region","dummy","variable","year_surv","temperature_at_bottom"),allow.cartesian=TRUE)
Data_Shrimp$species<-"shrimp"
Data_Crab$species<-"crab"

Data_total<-rbind(Data_Crab,Data_Shrimp)

rm(list=setdiff(ls(), c("Data","Data_total")))


# Calculate total biomass per cell (10 km x 10 km)

Data_total$cell_pred<-(Data_total$predicted*100)
Data_total$cell_low<-(Data_total$low*100)
Data_total$cell_hi<-(Data_total$hi*100)

Data_total[species=="crab"&(region=="Newfoundland_fall"|region=="Newfoundland_spring"),sum(cell_pred),by=c('variable','year_surv')]
