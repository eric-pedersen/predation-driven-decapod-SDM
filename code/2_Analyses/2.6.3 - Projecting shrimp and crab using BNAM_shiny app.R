### Projecting shrimp and crab models using BNAM ----
# Purpose: Show shrimp and crab distributions based on BNAM outputs
# Author: S. Zabihi-Seissan

# Load packages
library(data.table)
library(mgcv)
library(ggplot2)
library(viridis)
library(dplyr)
library(tidyr)

#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
#### Load Data ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

# Load BNAM and depth data
BNAM_Data<-fread('code/outputs/data/BNAM_Data_Output_shiny_app.csv')
BNAM_Data$region<-as.factor(BNAM_Data$region)

#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
#### Formatting data for projections ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
Data<-BNAM_Data
Data$dummy<-1
Data<-Data[depth<0]
Data$depth<-(-(Data$depth))


#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
#### Projecting climate only models ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

# Load climate only SDMs
models <- list.files("code/outputs/models/climate", pattern="*.rda", full.names=TRUE)
lapply(models,load,.GlobalEnv)

Data$pred_shrimp<-predict(climate_shrimpmodel,newdata = Data,type="response")
Data$pred_crab<-predict(climate_crabmodel,newdata= Data,type="response")

# Store climate only shrimp and crab outputs
Data$shrimp_climate<-Data$pred_shrimp
Data$crab_climate<-Data$pred_crab

rm(list=setdiff(ls(), "Data"))


#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
#### Projecting climate+predator density models ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

# Load climate only and diet models for lagged predator density projection
models <- list.files("code/outputs/models/climate", pattern="*.rda", full.names=TRUE)
lapply(models,load,.GlobalEnv)


# Replace year_surv from current to previous year (lag)
Data$year_current<-Data$year_surv
Data$year_current<-as.character(Data$year_current)
Data$year_current<-as.integer(Data$year_current)
Data$year_surv<-as.character(Data$year_surv)
Data$year_surv<-as.integer(Data$year_surv)
Data$year_surv<-(Data$year_current-1)
Data$year_surv<-as.factor(Data$year_surv)

# Predict 1-year lagged predator density
Data$pred_cod<-predict(climate_codmodel,newdata = Data,type="response")
Data$pred_turbot<-predict(climate_turbotmodel,newdata = Data,type="response")
Data$pred_redfish<-predict(climate_redfishmodel,newdata = Data,type="response")

rm(list=setdiff(ls(), "Data"))

# Load climate+predator SDMs
models <- list.files("code/outputs/models/climate/Climate_Predator_Density", pattern="*.rda", full.names=TRUE)
lapply(models,load,.GlobalEnv)

# Un-lag year
Data$year_surv<-Data$year_current
Data$year_surv<-as.factor(Data$year_surv)

Data$pred_shrimp<-predict(climate_shrimpmodel,newdata = Data,type="response")
Data$pred_crab<-predict(climate_crabmodel,newdata= Data,type="response")

# Store climate only shrimp and crab outputs
Data$shrimp_climatepred<-Data$pred_shrimp
Data$crab_climatepred<-Data$pred_crab

rm(list=setdiff(ls(), "Data"))


#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
#### Projecting climate+predator index models ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

# Load climate only for lagged predator density projection
models <- list.files("code/outputs/models/climate", pattern="*.rda", full.names=TRUE)
lapply(models,load,.GlobalEnv)
models_diet <- list.files("code/outputs/models/diet", pattern="*.rda", full.names=TRUE)
lapply(models_diet,load,.GlobalEnv)

# Replace year_surv from current to previous year (lag)
Data$year_current<-Data$year_surv
Data$year_current<-as.character(Data$year_current)
Data$year_current<-as.integer(Data$year_current)
Data$year_surv<-as.character(Data$year_surv)
Data$year_surv<-as.integer(Data$year_surv)
Data$year_surv<-(Data$year_current-1)
Data$year_surv<-as.factor(Data$year_surv)

# Predict species density based on climate only (1 year lagged)
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

# Revert years back to current
Data$year_surv<-Data$year_current
Data$year_surv<-as.factor(Data$year_surv)

rm(list=setdiff(ls(), "Data"))

# Load climate + predator index models
models <- list.files("code/outputs/models/climate/Climate_Predator_Index", pattern="*.rda", full.names=TRUE)
lapply(models,load,.GlobalEnv)

# Predict models
Data$shrimp_climateind<-predict(climate_shrimpmodel,newdata = Data,type="response")
Data$crab_climateind<-predict(climate_crabmodel,newdata = Data,type="response")

rm(list=setdiff(ls(), "Data"))

# Remove NA regions
data<-Data[!region==""]

# Remove years that had no survey or data by region for shrimp and crab
data[region=="Newfoundland_fall"&(year_surv=="1998"|year_surv=="1999"|year_surv=="2000"|year_surv=="2001"|
                                    year_surv=="2002"|year_surv=="2003"|year_surv=="2004"|year_surv=="2005"|
                                    year_surv=="2006"|year_surv=="2007"|year_surv=="2008"|year_surv=="2009"|
                                    year_surv=="2010"|year_surv=="2011"|year_surv=="2012"),shrimp_climateind:=NA]
data[region=="Newfoundland_fall"&(year_surv=="1998"|year_surv=="1999"|year_surv=="2000"|year_surv=="2001"|
                                    year_surv=="2002"|year_surv=="2003"|year_surv=="2004"|year_surv=="2005"|
                                    year_surv=="2006"|year_surv=="2007"|year_surv=="2008"|year_surv=="2009"|
                                    year_surv=="2010"|year_surv=="2011"|year_surv=="2012"),crab_climateind:=NA]
data[region=="Maritimes"&(year_surv=="2003"|year_surv=="2004"|year_surv=="2017"|year_surv=="2018"),shrimp_climateind:=NA]
data[region=="Maritimes"&(year_surv=="2003"|year_surv=="2004"|year_surv=="2017"|year_surv=="2018"),crab_climateind:=NA]
data[region=="Quebec"&(year_surv=="2010"|year_surv=="2011"|year_surv=="2012"|year_surv=="2013"|
                         year_surv=="2014"),shrimp_climateind:=NA]
data[region=="Quebec"&(year_surv=="2010"|year_surv=="2011"|year_surv=="2012"|year_surv=="2013"|
                         year_surv=="2014"),crab_climateind:=NA]
data[region=="Gulf"&(year_surv=="2018"),shrimp_climate:=NA]
data[region=="Gulf"&(year_surv=="2018"),crab_climate:=NA]
data[region=="Gulf"&(year_surv=="2018"),shrimp_climatepred:=NA]
data[region=="Gulf"&(year_surv=="2018"),crab_climatepred:=NA]
data[region=="Gulf"&!(year_surv=="2004"|year_surv=="2005"|year_surv=="2006"),shrimp_climateind:=NA]
data[region=="Gulf"&!(year_surv=="2004"|year_surv=="2005"|year_surv=="2006"),crab_climateind:=NA]
data[region=="Arctic"&!(year_surv=="2005"|year_surv=="2006"|year_surv=="2007"|year_surv=="2008"|year_surv=="2009"|
                          year_surv=="2010"|year_surv=="2011"|year_surv=="2012"|year_surv=="2013"|year_surv=="2014"|
                          year_surv=="2015"|year_surv=="2016"|year_surv=="2017"|year_surv=="2018"),shrimp_climate:=NA]
data[region=="Arctic",crab_climate:=NA]
data[region=="Arctic"&!(year_surv=="2005"|year_surv=="2006"|year_surv=="2007"|year_surv=="2008"|year_surv=="2009"|
                          year_surv=="2010"|year_surv=="2011"|year_surv=="2012"|year_surv=="2013"|year_surv=="2014"|
                          year_surv=="2015"|year_surv=="2016"|year_surv=="2017"|year_surv=="2018"),shrimp_climatepred:=NA]
data[region=="Arctic",crab_climatepred:=NA]
data[region=="Arctic"&!year_surv=="2018",shrimp_climateind:=NA]
data[region=="Arctic",crab_climateind:=NA]


#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
#### Format projection data for shiny app ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

# Shrimp
data_shrimp<-melt(data, id.vars = c("x","y","year_surv"), measure.vars = c("shrimp_climate","shrimp_climatepred","shrimp_climateind"))
data_shrimp[variable=="shrimp_climate",variable:="model1"]
data_shrimp[variable=="shrimp_climatepred",variable:="model2"]
data_shrimp[variable=="shrimp_climateind",variable:="model3"]
data_shrimp$variable<-paste(data_shrimp$year_surv,data_shrimp$variable)
data_shrimp$year_surv<-NULL
data_shrimp_cast<-data_shrimp %>%
  spread(variable,value)

# Crab
data_crab<-melt(data, id.vars = c("x","y","year_surv"), measure.vars = c("crab_climate","crab_climatepred","crab_climateind"))
data_crab[variable=="crab_climate",variable:="model1"]
data_crab[variable=="crab_climatepred",variable:="model2"]
data_crab[variable=="crab_climateind",variable:="model3"]
data_crab$variable<-paste(data_crab$year_surv,data_crab$variable)
data_crab$year_surv<-NULL
data_crab_cast<-data_crab %>%
  spread(variable,value)


#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
#### Export data for shiny app ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

fwrite(data_shrimp_cast,file="code/outputs/data/shrimp_data_1995-2018_shiny_app.csv")

fwrite(data_crab_cast,file="code/outputs/data/crab_data_1995-2018_shiny_app.csv")





















#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
#### Plotting projections ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

#Format data
Data_Shrimp_Plot<-melt(Data, id.vars = c("x","y","depth","temperature_at_bottom","year_surv","region","dummy"), measure.vars = c("shrimp_climate","shrimp_climatepred","shrimp_climateind"))
Data_Crab_Plot<-melt(Data, id.vars = c("x","y","depth","temperature_at_bottom","year_surv","region","dummy"), measure.vars = c("crab_climate","crab_climatepred","crab_climateind"))
Data_Shrimp_Plot<-Data_Shrimp_Plot[year_surv=="1996"|year_surv=="2006"|year_surv=="2016"]
Data_Crab_Plot<-Data_Crab_Plot[year_surv=="1996"|year_surv=="2006"|year_surv=="2016"]

labels <- c(shrimp_climate = "Climate only", shrimp_climatepred = "Climate + Pred dens", shrimp_climateind = "Climate + Pred ind")

Plot_Shrimp<-ggplot((subset(Data_Shrimp_Plot,region=="Arctic"|region=="Maritimes"|region=="Gulf"|region=="Newfoundland_fall"|region=="Quebec"|region=="Newfoundland_spring")))+
  geom_tile(aes(x,y,fill=value))+
  facet_wrap(variable~year_surv,labeller = labeller(variable = labels))+
  coord_equal()+
  ylab("Latitude")+
  xlab("Longitude")+
  scale_fill_viridis(name="Shrimp density (kg/km2)")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 20))
Plot_Shrimp

labels <- c(crab_climate = "Climate only", crab_climatepred = "Climate + Pred dens", crab_climateind = "Climate + Pred ind")

Plot_Crab<-ggplot((subset(Data_Crab_Plot,region=="Maritimes"|region=="Gulf"|region=="Newfoundland_fall"|region=="Quebec")))+
  geom_tile(aes(x,y,fill=value))+
  facet_wrap(variable~year_surv,labeller = labeller(variable = labels))+
  coord_equal()+
  ylab("Latitude")+
  xlab("Longitude")+
  scale_fill_viridis(name="Crab density (kg/km2)")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 20))
Plot_Crab
