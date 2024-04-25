### MODEL PROJECTIONS FOR CLIMATE SPECIES DISTRIBUTION MODELS ----
# Purpose: Produce plots to visualize the climate SDMs
# Author: S. Zabihi-Seissan

#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
### Packages ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

library(data.table)
library(ggplot2)
library(googledrive)
library(mgcv)
library(sp)
library(ggpubr)
library(grid)


#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
### climate ONLY SDMs ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

# Load models (From 2.2 script)
models <- list.files("code/outputs/models/climate", pattern="*.rda", full.names=TRUE)
lapply(models,load,.GlobalEnv)


#<><><><><>><>><><><><>><><><>><><><><><><>><>><><><><>><><><>><><><><><><>><>><><><><>><><><>><><><><><><>><>><><><
### Plot predictions ----
#<><><><><>><>><><><><>><><><>><><><><><><>><>><><><><>><><><>><><><><><><>><>><><><><>><><><>><><><><><><>><>><><><
#Data[,quantile(temperature_at_bottom,0.98,na.rm=TRUE),by="region_large"]
## Northern shrimp

# Predict effect of temperature (Climate only model)
fam <- family(climate_shrimpmodel)
ilink <- fam$linkinv
NewData = expand.grid(temperature_at_bottom = seq(from = -1.9, to = 17.6, by = 0.1),
                      depth =  276.1,
                      year_surv= seq(from= 1990, to = 2018, by=1),
                      region=c("Maritimes","Quebec","Gulf","Newfoundland_fall","Newfoundland_spring","Arctic"),
                      region_large=c("NL_shelf","SL_shelf","SC_shelf"),
                      dummy=0)
NewData$pred<-predict(climate_shrimpmodel,newdata = NewData,exclude="s(region,year_surv)")
NewData$fit<-ilink(NewData$pred)
NewData$se.fit<-predict(climate_shrimpmodel,newdata = NewData,se.fit=TRUE)$se.fit
NewData$cilow <- ilink(NewData$pred - (1.96 * NewData$se.fit))
NewData$cihi <- ilink(NewData$pred + (1.96 * NewData$se.fit))
NewData<-as.data.table(NewData)
NewData<-NewData[!(region_large=="ART_shelf"&(temperature_at_bottom>4.36))]
NewData<-NewData[!(region_large=="SL_shelf"&(temperature_at_bottom>12.7))]
NewData<-NewData[!(region_large=="SC_shelf"&(temperature_at_bottom>10.98))]
NewData<-NewData[!(region_large=="NL_shelf"&(temperature_at_bottom>7.1))]
Data_Shrimp_Cli_Temp<-NewData
Data_Shrimp_Cli_Temp$type<-"Climate only"
Data_Shrimp_Cli_Temp$variable<-"Bottom temperature"

# Predict effect of depth (Climate only model)
fam <- family(climate_shrimpmodel)
ilink <- fam$linkinv
NewData = as.data.table(expand.grid(depth = seq(from = 18, to = 1504, by = 10),
                      temperature_at_bottom =  2.7,
                      year_surv= seq(from= 1990, to = 2018, by=1),
                      region=c("Maritimes","Quebec","Gulf","Newfoundland_fall","Newfoundland_spring","Arctic"),
                      region_large=c("NL_shelf","SL_shelf","SC_shelf"),
                      dummy=0))
NewData$pred<-predict(climate_shrimpmodel,newdata = NewData,exclude="s(region,year_surv)")
NewData$fit<-ilink(NewData$pred)
NewData$se.fit<-predict(climate_shrimpmodel,newdata = NewData,se.fit=TRUE)$se.fit
NewData$cilow <- ilink(NewData$pred - (1.96 * NewData$se.fit))
NewData$cihi <- ilink(NewData$pred + (1.96 * NewData$se.fit))
NewData<-as.data.table(NewData)
Data_Shrimp_Cli_Depth<-NewData
Data_Shrimp_Cli_Depth$type<-"Climate only"
Data_Shrimp_Cli_Depth$variable<-"Depth"


## Snow crab

# Predict effect of temperature (Climate only model)
fam <- family(climate_crabmodel)
ilink <- fam$linkinv
NewData = expand.grid(temperature_at_bottom = seq(from = -1.9, to = 18.6, by = 0.1),
                      depth =  184,
                      year_surv= seq(from= 1990, to = 2018, by=1),
                      region=c("Maritimes","Quebec","Gulf","Newfoundland_fall","Newfoundland_spring"),
                      region_large=c("NL_shelf","SL_shelf","SC_shelf"),
                      dummy=0)
NewData$pred<-predict(climate_crabmodel,newdata = NewData,exclude="s(region,year_surv)")
NewData$fit<-ilink(NewData$pred)
NewData$se.fit<-predict(climate_crabmodel,newdata = NewData,se.fit=TRUE)$se.fit
NewData$cilow <- ilink(NewData$pred - (1.96 * NewData$se.fit))
NewData$cihi <- ilink(NewData$pred + (1.96 * NewData$se.fit))
NewData<-as.data.table(NewData)
NewData<-NewData[!(region_large=="ART_shelf"&(temperature_at_bottom>4.36))]
NewData<-NewData[!(region_large=="SL_shelf"&(temperature_at_bottom>12.7))]
NewData<-NewData[!(region_large=="SC_shelf"&(temperature_at_bottom>10.98))]
NewData<-NewData[!(region_large=="NL_shelf"&(temperature_at_bottom>7.1))]
Data_Crab_Cli_Temp<-NewData
Data_Crab_Cli_Temp$type<-"Climate only"
Data_Crab_Cli_Temp$variable<-"Bottom temperature"

# Predict effect of depth (Climate only model)
fam <- family(climate_crabmodel)
ilink <- fam$linkinv
NewData = as.data.table(expand.grid(depth = seq(from = 17.5, to = 1504, by = 10),
                      temperature_at_bottom =  1.6,
                      year_surv= seq(from= 1990, to = 2018, by=1),
                      region=c("Maritimes","Quebec","Gulf","Newfoundland_fall","Newfoundland_spring"),
                      region_large=c("NL_shelf","SL_shelf","SC_shelf"),
                      dummy=0))
NewData<-NewData[!(region_large=="ART_shelf"&(depth<100|depth>968))]
NewData<-NewData[!(region_large=="SL_shelf"&(depth<15|depth>524.5))]
NewData<-NewData[!(region_large=="SC_shelf"&(depth<20.1168|depth>515.7216))]
NewData<-NewData[!(region_large=="NL_shelf"&(depth<32|depth>1504))]
NewData$pred<-predict(climate_crabmodel,newdata = NewData,exclude="s(region,year_surv)")
NewData$fit<-ilink(NewData$pred)
NewData$se.fit<-predict(climate_crabmodel,newdata = NewData,se.fit=TRUE)$se.fit
NewData$cilow <- ilink(NewData$pred - (1.96 * NewData$se.fit))
NewData$cihi <- ilink(NewData$pred + (1.96 * NewData$se.fit))
NewData<-as.data.table(NewData)
Data_Crab_Cli_Depth<-NewData
Data_Crab_Cli_Depth$type<-"Climate only"
Data_Crab_Cli_Depth$variable<-"Depth"

## Clean slate
rm(list=setdiff(ls(), c("Data_Shrimp_Cli_Temp","Data_Shrimp_Cli_Depth","Data_Crab_Cli_Temp","Data_Crab_Cli_Depth","Data")))


#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
### climate + predator density SDMs ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

# Load models (From 2.2 script)
models <- list.files("code/outputs/models/climate/Climate_Predator_Density", pattern="*.rda", full.names=TRUE)
lapply(models,load,.GlobalEnv)

## Northern shrimp

# Effect of temperature (predator density model)
fam <- family(climate_shrimpmodel)
ilink <- fam$linkinv
NewData = expand.grid(temperature_at_bottom = seq(from = -1.9, to = 17.6, by = 0.1),depth = 276.1,
                      pred_cod =  339.4, pred_turbot = 627.7, pred_redfish = 1473.557,
                      year_surv= seq(from= 1990, to = 2018, by=1),
                      region=c("Maritimes","Quebec","Gulf","Newfoundland_fall","Newfoundland_spring","Arctic"),
                      region_large=c("NL_shelf","SL_shelf","SC_shelf"),
                      dummy=0)
NewData$pred<-predict(climate_shrimpmodel,newdata = NewData)
NewData$fit<-ilink(NewData$pred)
NewData$se.fit<-predict(climate_shrimpmodel,newdata = NewData,se.fit=TRUE)$se.fit
NewData$cilow <- ilink(NewData$pred - (1.96 * NewData$se.fit))
NewData$cihi <- ilink(NewData$pred + (1.96 * NewData$se.fit))
NewData<-as.data.table(NewData)
NewData<-NewData[!(region_large=="ART_shelf"&(temperature_at_bottom>4.36))]
NewData<-NewData[!(region_large=="SL_shelf"&(temperature_at_bottom>12.7))]
NewData<-NewData[!(region_large=="SC_shelf"&(temperature_at_bottom>10.98))]
NewData<-NewData[!(region_large=="NL_shelf"&(temperature_at_bottom>7.1))]
Data_Shrimp_CliPred_Temp<-NewData
Data_Shrimp_CliPred_Temp$type<-"Climate + Predator density"
Data_Shrimp_CliPred_Temp$variable<-"Bottom temperature"

# Effect of depth (predator density model)
fam <- family(climate_shrimpmodel)
ilink <- fam$linkinv
NewData = as.data.table(expand.grid(depth = seq(from = 18, to = 1504, by = 10),
                      temperature_at_bottom =  2.7,
                      pred_cod =  339.4, pred_turbot = 627.7, pred_redfish = 1473.557,
                      year_surv= seq(from= 1990, to = 2018, by=1),
                      region=c("Maritimes","Quebec","Gulf","Newfoundland_fall","Newfoundland_spring","Arctic"),
                      region_large=c("NL_shelf","SL_shelf","SC_shelf","ART_shelf"),
                      dummy=0))
NewData<-NewData[!(region_large=="ART_shelf"&(depth<100|depth>968))]
NewData<-NewData[!(region_large=="SL_shelf"&(depth<15|depth>524.5))]
NewData<-NewData[!(region_large=="SC_shelf"&(depth<20.1168|depth>515.7216))]
NewData<-NewData[!(region_large=="NL_shelf"&(depth<32|depth>1504))]
NewData$pred<-predict(climate_shrimpmodel,newdata = NewData)
NewData$fit<-ilink(NewData$pred)
NewData$se.fit<-predict(climate_shrimpmodel,newdata = NewData,se.fit=TRUE)$se.fit
NewData$cilow <- ilink(NewData$pred - (1.96 * NewData$se.fit))
NewData$cihi <- ilink(NewData$pred + (1.96 * NewData$se.fit))
Data_Shrimp_CliPred_Depth<-NewData
Data_Shrimp_CliPred_Depth$type<-"Climate + Predator density"
Data_Shrimp_CliPred_Depth$variable<-"Depth"

## Effect of cod density on shrimp (predator density model)
fam <- family(climate_shrimpmodel)
ilink <- fam$linkinv
NewData = expand.grid(pred_cod = seq(from = 0, to = 3.998284e+03, by = 100),
                      temperature_at_bottom =  2.765848, depth = 275.8, pred_turbot = 376.2773,pred_redfish = 2048.639,
                      year_surv= seq(from= 1990, to = 2018, by=1),
                      region=c("Maritimes","Quebec","Gulf","Newfoundland_fall","Newfoundland_spring","Arctic"),
                      region_large=c("NL_shelf","SL_shelf","SC_shelf"),
                      dummy=0)
NewData$pred<-predict(climate_shrimpmodel,newdata = NewData)
NewData$fit<-ilink(NewData$pred)
NewData$se.fit<-predict(climate_shrimpmodel,newdata = NewData,se.fit=TRUE)$se.fit
NewData$cilow <- ilink(NewData$pred - (1.96 * NewData$se.fit))
NewData$cihi <- ilink(NewData$pred + (1.96 * NewData$se.fit))
Data_Shrimp_CliPred_Cod<-NewData
Data_Shrimp_CliPred_Cod$type<-"Climate + Predator density"
Data_Shrimp_CliPred_Cod$variable<-"Cod"

## Effect of turbot density on shrimp (predator density model)
fam <- family(climate_shrimpmodel)
ilink <- fam$linkinv
NewData = expand.grid(pred_turbot = seq(from = 0, to = 7.661129e+03, by = 100),
                      temperature_at_bottom =  2.765848, depth = 275.8, pred_cod = 389.6292,pred_redfish = 2048.639,
                      year_surv= seq(from= 1990, to = 2018, by=1),
                      region=c("Maritimes","Quebec","Gulf","Newfoundland_fall","Newfoundland_spring","Arctic"),
                      region_large=c("NL_shelf","SL_shelf","SC_shelf"),
                      dummy=0)
NewData$pred<-predict(climate_shrimpmodel,newdata = NewData)
NewData$fit<-ilink(NewData$pred)
NewData$se.fit<-predict(climate_shrimpmodel,newdata = NewData,se.fit=TRUE)$se.fit
NewData$cilow <- ilink(NewData$pred - (1.96 * NewData$se.fit))
NewData$cihi <- ilink(NewData$pred + (1.96 * NewData$se.fit))
Data_Shrimp_CliPred_Turbot<-NewData
Data_Shrimp_CliPred_Turbot$type<-"Climate + Predator density"
Data_Shrimp_CliPred_Turbot$variable<-"Greenland halibut"

## Effect of redfish density on shrimp (predator density model)
fam <- family(climate_shrimpmodel)
ilink <- fam$linkinv
NewData = expand.grid(pred_redfish = seq(from = 0, to = 5.692335e+04, by = 100),
                      temperature_at_bottom =  2.765848, depth = 275.8, pred_cod = 389.6292,pred_turbot = 376.2773,
                      year_surv= seq(from= 1990, to = 2018, by=1),
                      region=c("Maritimes","Quebec","Gulf","Newfoundland_fall","Newfoundland_spring","Arctic"),
                      region_large=c("NL_shelf","SL_shelf","SC_shelf"),
                      dummy=0)
NewData$pred<-predict(climate_shrimpmodel,newdata = NewData)
NewData$fit<-ilink(NewData$pred)
NewData$se.fit<-predict(climate_shrimpmodel,newdata = NewData,se.fit=TRUE)$se.fit
NewData$cilow <- ilink(NewData$pred - (1.96 * NewData$se.fit))
NewData$cihi <- ilink(NewData$pred + (1.96 * NewData$se.fit))
Data_Shrimp_CliPred_Redfish<-NewData
Data_Shrimp_CliPred_Redfish$type<-"Climate + Predator density"
Data_Shrimp_CliPred_Redfish$variable<-"Redfish"

## Snow crab

## Effect of temperature on crab (Predator density model)
fam <- family(climate_crabmodel)
ilink <- fam$linkinv
NewData = expand.grid(temperature_at_bottom = seq(from = -1.9, to = 18.6, by = 0.1),depth=184,
                      pred_cod =  395.9,
                      year_surv= seq(from= 1990, to = 2018, by=1),
                      region=c("Maritimes","Quebec","Gulf","Newfoundland_fall","Newfoundland_spring"),
                      region_large=c("NL_shelf","SL_shelf","SC_shelf"),
                      dummy=0)
NewData$pred<-predict(climate_crabmodel,newdata = NewData)
NewData$fit<-ilink(NewData$pred)
NewData$se.fit<-predict(climate_crabmodel,newdata = NewData,se.fit=TRUE)$se.fit
NewData$cilow <- ilink(NewData$pred - (1.96 * NewData$se.fit))
NewData$cihi <- ilink(NewData$pred + (1.96 * NewData$se.fit))
NewData<-as.data.table(NewData)
NewData<-NewData[!(region_large=="ART_shelf"&(temperature_at_bottom>4.36))]
NewData<-NewData[!(region_large=="SL_shelf"&(temperature_at_bottom>12.7))]
NewData<-NewData[!(region_large=="SC_shelf"&(temperature_at_bottom>10.98))]
NewData<-NewData[!(region_large=="NL_shelf"&(temperature_at_bottom>7.1))]
Data_Crab_CliPred_Temp<-NewData
Data_Crab_CliPred_Temp$type<-"Climate + Predator density"
Data_Crab_CliPred_Temp$variable<-"Bottom temperature"

## Effect of depth on crab (Predator density model)
fam <- family(climate_crabmodel)
ilink <- fam$linkinv
NewData = as.data.table(expand.grid(depth = seq(from = 17.5, to = 1504, by = 10),
                      temperature_at_bottom =  1.6,
                      pred_cod =  395.9,
                      year_surv= seq(from= 1990, to = 2018, by=1),
                      region=c("Maritimes","Quebec","Gulf","Newfoundland_fall","Newfoundland_spring"),
                      region_large=c("NL_shelf","SL_shelf","SC_shelf"),
                      dummy=0))
NewData<-NewData[!(region_large=="SL_shelf"&(depth<15|depth>524.5))]
NewData<-NewData[!(region_large=="SC_shelf"&(depth<20.1168|depth>515.7216))]
NewData<-NewData[!(region_large=="NL_shelf"&(depth<32|depth>1504))]
NewData$pred<-predict(climate_crabmodel,newdata = NewData)
NewData$fit<-ilink(NewData$pred)
NewData$se.fit<-predict(climate_crabmodel,newdata = NewData,se.fit=TRUE)$se.fit
NewData$cilow <- ilink(NewData$pred - (1.96 * NewData$se.fit))
NewData$cihi <- ilink(NewData$pred + (1.96 * NewData$se.fit))
Data_Crab_CliPred_Depth<-NewData
Data_Crab_CliPred_Depth$type<-"Climate + Predator density"
Data_Crab_CliPred_Depth$variable<-"Depth"

## Effect of cod density on crab density (Predator density model)
fam <- family(climate_crabmodel)
ilink <- fam$linkinv
NewData = expand.grid(pred_cod = seq(from = 17.5, to = 3.998284e+03, by = 100),
                      temperature_at_bottom =  1.6,
                      depth =  184,
                      year_surv= seq(from= 1990, to = 2018, by=1),
                      region=c("Maritimes","Quebec","Gulf","Newfoundland_fall","Newfoundland_spring"),
                      region_large=c("NL_shelf","SL_shelf","SC_shelf"),
                      dummy=0)
NewData$pred<-predict(climate_crabmodel,newdata = NewData)
NewData$fit<-ilink(NewData$pred)
NewData$se.fit<-predict(climate_crabmodel,newdata = NewData,se.fit=TRUE)$se.fit
NewData$cilow <- ilink(NewData$pred - (1.96 * NewData$se.fit))
NewData$cihi <- ilink(NewData$pred + (1.96 * NewData$se.fit))
Data_Crab_CliPred_Cod<-NewData
Data_Crab_CliPred_Cod$type<-"Climate + Predator density"
Data_Crab_CliPred_Cod$variable<-"Cod"

## Clean slate
rm(list=setdiff(ls(), c("Data_Shrimp_Cli_Temp","Data_Shrimp_Cli_Depth","Data_Crab_Cli_Temp","Data_Crab_Cli_Depth",
                        "Data_Shrimp_CliPred_Temp","Data_Shrimp_CliPred_Depth","Data_Shrimp_CliPred_Cod",
                        "Data_Shrimp_CliPred_Turbot","Data_Shrimp_CliPred_Redfish","Data_Crab_CliPred_Temp",
                        "Data_Crab_CliPred_Depth","Data_Crab_CliPred_Cod","Data")))


#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
### climate + predation index SDMs ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

# Load models (From 2.2 script)
models <- list.files("code/outputs/models/climate/Climate_Predator_Index", pattern="*.rda", full.names=TRUE)
lapply(models,load,.GlobalEnv)

## Northern shrimp

# Effect of temperature on shrimp (predation index model)
fam <- family(climate_shrimpmodel)
ilink <- fam$linkinv
NewData = expand.grid(temperature_at_bottom = seq(from = -1.9, to = 17.6, by = 0.1),depth = 276.1,
                      pred_index_shrimp=837.5253,
                      year_surv= seq(from= 1990, to = 2018, by=1),
                      region=c("Maritimes","Quebec","Gulf","Newfoundland_fall","Newfoundland_spring","Arctic"),
                      region_large=c("NL_shelf","SL_shelf","SC_shelf"),
                      dummy=0)
NewData<-as.data.table(NewData)
NewData<-NewData[!(year_surv==1990|year_surv==1991|year_surv==1992|year_surv==1993|year_surv==1999|
                     year_surv==2001|year_surv==2002|year_surv==2003|year_surv==2004|year_surv==2008)]
NewData$pred<-predict(climate_shrimpmodel,newdata = NewData)
NewData$fit<-ilink(NewData$pred)
NewData$se.fit<-predict(climate_shrimpmodel,newdata = NewData,se.fit=TRUE)$se.fit
NewData$cilow <- ilink(NewData$pred - (1.96 * NewData$se.fit))
NewData$cihi <- ilink(NewData$pred + (1.96 * NewData$se.fit))
NewData<-as.data.table(NewData)
NewData<-NewData[!(region_large=="ART_shelf"&(temperature_at_bottom>4.36))]
NewData<-NewData[!(region_large=="SL_shelf"&(temperature_at_bottom>12.7))]
NewData<-NewData[!(region_large=="SC_shelf"&(temperature_at_bottom>10.98))]
NewData<-NewData[!(region_large=="NL_shelf"&(temperature_at_bottom>7.1))]
Data_Shrimp_CliPredInd_Temp<-NewData
Data_Shrimp_CliPredInd_Temp$type<-"Climate + Predation index"
Data_Shrimp_CliPredInd_Temp$variable<-"Bottom temperature"

#Effect of depth on shrimp (predation index model)
fam <- family(climate_shrimpmodel)
ilink <- fam$linkinv
NewData = as.data.table(expand.grid(depth = seq(from = 18, to = 1504, by = 10),
                      temperature_at_bottom =  2.7,
                      pred_index_shrimp=837.5253,
                      year_surv= seq(from= 1990, to = 2018, by=1),
                      region=c("Maritimes","Quebec","Gulf","Newfoundland_fall","Newfoundland_spring","Arctic"),
                      region_large=c("NL_shelf","SL_shelf","SC_shelf"),
                      dummy=0))
NewData<-as.data.table(NewData)
NewData<-NewData[!(year_surv==1990|year_surv==1991|year_surv==1992|year_surv==1993|year_surv==1999|
                     year_surv==2001|year_surv==2002|year_surv==2003|year_surv==2004|year_surv==2008)]
NewData<-NewData[!(region_large=="ART_shelf"&(depth<100|depth>968))]
NewData<-NewData[!(region_large=="SL_shelf"&(depth<15|depth>524.5))]
NewData<-NewData[!(region_large=="SC_shelf"&(depth<20.1168|depth>515.7216))]
NewData<-NewData[!(region_large=="NL_shelf"&(depth<32|depth>1504))]
NewData$pred<-predict(climate_shrimpmodel,newdata = NewData)
NewData$fit<-ilink(NewData$pred)
NewData$se.fit<-predict(climate_shrimpmodel,newdata = NewData,se.fit=TRUE)$se.fit
NewData$cilow <- ilink(NewData$pred - (1.96 * NewData$se.fit))
NewData$cihi <- ilink(NewData$pred + (1.96 * NewData$se.fit))
Data_Shrimp_CliPredInd_Depth<-NewData
Data_Shrimp_CliPredInd_Depth$type<-"Climate + Predation index"
Data_Shrimp_CliPredInd_Depth$variable<-"Depth"

## Effect of predation index on shrimp
fam <- family(climate_shrimpmodel)
ilink <- fam$linkinv
NewData = expand.grid(pred_index_shrimp = seq(from = 18, to = 4.557369e+03, by = 100),
                      temperature_at_bottom =  2.7,
                      depth=276.1,
                      year_surv= seq(from= 1990, to = 2018, by=1),
                      region=c("Maritimes","Quebec","Gulf","Newfoundland_fall","Newfoundland_spring","Arctic"),
                      region_large=c("NL_shelf","SL_shelf","SC_shelf"),
                      dummy=0)
NewData<-as.data.table(NewData)
NewData<-NewData[!(year_surv==1990|year_surv==1991|year_surv==1992|year_surv==1993|year_surv==1999|
                     year_surv==2001|year_surv==2002|year_surv==2003|year_surv==2004|year_surv==2008)]
NewData$pred<-predict(climate_shrimpmodel,newdata = NewData)
NewData$fit<-ilink(NewData$pred)
NewData$se.fit<-predict(climate_shrimpmodel,newdata = NewData,se.fit=TRUE)$se.fit
NewData$cilow <- ilink(NewData$pred - (1.96 * NewData$se.fit))
NewData$cihi <- ilink(NewData$pred + (1.96 * NewData$se.fit))
Data_Shrimp_CliPredInd_Index<-NewData
Data_Shrimp_CliPredInd_Index$type<-"Climate + Predation index"
Data_Shrimp_CliPredInd_Index$variable<-"Predation index"

## Snow Crab

# Effect of temperature on crab (Predation index model)
fam <- family(climate_crabmodel)
ilink <- fam$linkinv
NewData = expand.grid(temperature_at_bottom = seq(from = -1.9, to = 18.6, by = 0.1),depth=184,
                      pred_index_crab = 43.5,
                      year_surv= seq(from= 1994, to = 2018, by=1),
                      region=c("Maritimes","Quebec","Gulf","Newfoundland_fall","Newfoundland_spring"),
                      region_large=c("NL_shelf","SL_shelf","SC_shelf"),
                      dummy=0)
NewData$pred<-predict(climate_crabmodel,newdata = NewData)
NewData$fit<-ilink(NewData$pred)
NewData$se.fit<-predict(climate_crabmodel,newdata = NewData,se.fit=TRUE)$se.fit
NewData$cilow <- ilink(NewData$pred - (1.96 * NewData$se.fit))
NewData$cihi <- ilink(NewData$pred + (1.96 * NewData$se.fit))
NewData<-as.data.table(NewData)
NewData<-NewData[!(region_large=="ART_shelf"&(temperature_at_bottom>4.36))]
NewData<-NewData[!(region_large=="SL_shelf"&(temperature_at_bottom>12.7))]
NewData<-NewData[!(region_large=="SC_shelf"&(temperature_at_bottom>10.98))]
NewData<-NewData[!(region_large=="NL_shelf"&(temperature_at_bottom>7.1))]
Data_Crab_CliPredInd_Temp<-NewData
Data_Crab_CliPredInd_Temp$type<-"Climate + Predation index"
Data_Crab_CliPredInd_Temp$variable<-"Bottom temperature"

# Effect of depth on crab (Predation index model)
fam <- family(climate_crabmodel)
ilink <- fam$linkinv
NewData = as.data.table(expand.grid(depth = seq(from = 17.5, to = 1504, by = 10),
                      temperature_at_bottom =  1.6,
                      pred_index_crab = 43.5,
                      year_surv= seq(from= 1994, to = 2018, by=1),
                      region=c("Maritimes","Quebec","Gulf","Newfoundland_fall","Newfoundland_spring"),
                      region_large=c("NL_shelf","SL_shelf","SC_shelf"),
                      dummy=0))
NewData<-NewData[!(region_large=="SL_shelf"&(depth<15|depth>524.5))]
NewData<-NewData[!(region_large=="SC_shelf"&(depth<20.1168|depth>515.7216))]
NewData<-NewData[!(region_large=="NL_shelf"&(depth<32|depth>1504))]
NewData$pred<-predict(climate_crabmodel,newdata = NewData)
NewData$fit<-ilink(NewData$pred)
NewData$se.fit<-predict(climate_crabmodel,newdata = NewData,se.fit=TRUE)$se.fit
NewData$cilow <- ilink(NewData$pred - (1.96 * NewData$se.fit))
NewData$cihi <- ilink(NewData$pred + (1.96 * NewData$se.fit))
Data_Crab_CliPredInd_Depth<-NewData
Data_Crab_CliPredInd_Depth$type<-"Climate + Predation index"
Data_Crab_CliPredInd_Depth$variable<-"Depth"

# Effect of predation index on crab (Predation index model)
fam <- family(climate_crabmodel)
ilink <- fam$linkinv
NewData = expand.grid(pred_index_crab = seq(from = 0, to = 4.191788e+02, by = 10),
                      temperature_at_bottom =  1.6,
                      depth = 184,
                      year_surv= seq(from= 1994, to = 2018, by=1),
                      region=c("Maritimes","Quebec","Gulf","Newfoundland_fall","Newfoundland_spring"),
                      region_large=c("NL_shelf","SL_shelf","SC_shelf"),
                      dummy=0)
NewData$pred<-predict(climate_crabmodel,newdata = NewData)
NewData$fit<-ilink(NewData$pred)
NewData$se.fit<-predict(climate_crabmodel,newdata = NewData,se.fit=TRUE)$se.fit
NewData$cilow <- ilink(NewData$pred - (1.96 * NewData$se.fit))
NewData$cihi <- ilink(NewData$pred + (1.96 * NewData$se.fit))
Data_Crab_CliPredInd_Index<-NewData
Data_Crab_CliPredInd_Index$type<-"Climate + Predation index"
Data_Crab_CliPredInd_Index$variable<-"Predation index"


#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
### Format predicted datasets ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

# Bind all predict datasets
Data_Shrimp<-rbind(Data_Shrimp_Cli_Depth,Data_Shrimp_Cli_Temp,Data_Shrimp_CliPred_Cod,Data_Shrimp_CliPred_Depth,
                   Data_Shrimp_CliPred_Redfish,Data_Shrimp_CliPred_Temp,Data_Shrimp_CliPred_Turbot,Data_Shrimp_CliPredInd_Depth,
                   Data_Shrimp_CliPredInd_Index,Data_Shrimp_CliPredInd_Temp,fill=TRUE)

Data_Crab<-rbind(Data_Crab_Cli_Depth,Data_Crab_Cli_Temp,Data_Crab_CliPred_Cod,Data_Crab_CliPred_Depth,
                 Data_Crab_CliPred_Temp,Data_Crab_CliPredInd_Depth,
                 Data_Crab_CliPredInd_Index,Data_Crab_CliPredInd_Temp,fill=TRUE)

# Clean slate
rm(list=setdiff(ls(), c("Data_Shrimp","Data_Crab","Data")))



#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
### Plotting ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

## Shrimp ~ Temperature and Depth effects for all three models
Data_Shrimp_Temp<-Data_Shrimp[variable=="Bottom temperature"]
Data_Shrimp_Temp[type=="Climate only",type:="C-SDM"]
Data_Shrimp_Temp[type=="Climate + Predator density",type:="P-SDM"]
Data_Shrimp_Temp[type=="Climate + Predation index",type:="PI-SDM"]
colnames(Data_Shrimp_Temp)[colnames(Data_Shrimp_Temp)=="type"] <- "Model"
Data_Shrimp_Temp[region_large=="NL_shelf",region_large:="Newfoundland & Labrador Shelf"]
Data_Shrimp_Temp[region_large=="SL_shelf",region_large:="St. Laurent Shelf"]
Data_Shrimp_Temp[region_large=="SC_shelf",region_large:="Scotian Shelf"]
Data_Shrimp_Temp[region_large=="ART_shelf",region_large:="Arctic Shelf"]
colnames(Data_Shrimp_Temp)[colnames(Data_Shrimp_Temp)=="region_large"] <- "Region"

Plot_Temp_Shrimp<-ggplot(transform(Data_Shrimp_Temp,Model=factor(Model,levels=c("C-SDM","P-SDM","PI-SDM"))))+
  geom_ribbon(aes(temperature_at_bottom,ymin=log(cilow),ymax=log(cihi),fill=Model),alpha=0.25)+
  geom_line(aes(temperature_at_bottom,log(fit),color=Model),size=0.05)+
  scale_colour_manual(values=c("#D81A60","#004D41","#3D83C4"))+
  scale_fill_manual(values=c("#D81A60","#004D41","#3D83C4"))+
  ylab(expression(paste("log(Northern shrimp density)",(kg*"/"*km^{2}))))+xlab("Temperature at bottom (°C)")+
  ylim(-8,10)+
  theme(axis.line = element_line(colour = "black",size=0.05),
        axis.ticks = element_line(size=0.05),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key=element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.box.spacing = unit(0.02, 'cm'),
        legend.key.width = unit(0.3, 'cm'),
        legend.key.height = unit(0.1, 'cm'),
        text = element_text(size = 4))
Plot_Temp_Shrimp

# Shrimp ~ Depth
Data_Shrimp_Depth<-Data_Shrimp[variable=="Depth"]
Data_Shrimp_Depth[type=="Climate only",type:="C-SDM"]
Data_Shrimp_Depth[type=="Climate + Predator density",type:="P-SDM"]
Data_Shrimp_Depth[type=="Climate + Predation index",type:="PI-SDM"]
colnames(Data_Shrimp_Depth)[colnames(Data_Shrimp_Depth)=="type"] <- "Model"
Data_Shrimp_Depth[region_large=="NL_shelf",region_large:="Newfoundland & Labrador Shelf"]
Data_Shrimp_Depth[region_large=="SL_shelf",region_large:="St. Laurent Shelf"]
Data_Shrimp_Depth[region_large=="SC_shelf",region_large:="Scotian Shelf"]
Data_Shrimp_Depth[region_large=="ART_shelf",region_large:="Arctic Shelf"]
colnames(Data_Shrimp_Depth)[colnames(Data_Shrimp_Depth)=="region_large"] <- "Region"

Plot_Depth_Shrimp<-ggplot(transform(Data_Shrimp_Depth,Model=factor(Model,levels=c("C-SDM","P-SDM","PI-SDM"))))+
  geom_ribbon(aes(depth,ymin=log(cilow),ymax=log(cihi),fill=Model),alpha=0.25)+
  geom_line(aes(depth,log(fit),color=Model),size=0.05)+
  scale_colour_manual(values=c("#D81A60","#004D41","#3D83C4"))+
  scale_fill_manual(values=c("#D81A60","#004D41","#3D83C4"))+
  ylab(expression(paste("log(Northern shrimp density)",(kg*"/"*km^{2}))))+xlab("Depth (m)")+
  ylim(-8,10)+
  theme(axis.line = element_line(colour = "black",size=0.05),
        axis.ticks = element_line(size=0.05),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key=element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.box.spacing = unit(0.02, 'cm'),
        legend.key.width = unit(0.3, 'cm'),
        legend.key.height = unit(0.1, 'cm'),
        text = element_text(size = 4))
Plot_Depth_Shrimp

## Crab ~ Temperature and Depth effects for all three models

# Crab ~ Temperature
Data_Crab_Temp<-Data_Crab[variable=="Bottom temperature"]
Data_Crab_Temp[type=="Climate only",type:="C-SDM"]
Data_Crab_Temp[type=="Climate + Predator density",type:="P-SDM"]
Data_Crab_Temp[type=="Climate + Predation index",type:="PI-SDM"]
colnames(Data_Crab_Temp)[colnames(Data_Crab_Temp)=="type"] <- "Model"
Data_Crab_Temp[region_large=="NL_shelf",region_large:="Newfoundland & Labrador Shelf"]
Data_Crab_Temp[region_large=="SL_shelf",region_large:="St. Lawrence Shelf"]
Data_Crab_Temp[region_large=="SC_shelf",region_large:="Scotian Shelf"]
Data_Crab_Temp[region_large=="ART_shelf",region_large:="Arctic Shelf"]
colnames(Data_Crab_Temp)[colnames(Data_Crab_Temp)=="region_large"] <- "Region"
Plot_Temp_Crab<-ggplot(transform(Data_Crab_Temp,Model=factor(Model,levels=c("C-SDM","P-SDM","PI-SDM"))))+
  facet_wrap(~Region)+
  geom_ribbon(aes(temperature_at_bottom,ymin=log(cilow),ymax=log(cihi),fill=Model),alpha=0.25)+
  geom_line(aes(temperature_at_bottom,log(fit),group=interaction(Model,Region),color=Model),size=0.05)+
  scale_colour_manual(values=c("#D81A60","#004D41","#3D83C4"))+
  scale_fill_manual(values=c("#D81A60","#004D41","#3D83C4"))+
  ylab(expression(paste("log(Snow crab density)",(kg*"/"*km^{2}))))+xlab("Temperature at bottom (°C)")+
  ylim(-9,5)+
  theme(axis.line = element_line(colour = "black",size=0.05),
        axis.ticks = element_line(size=0.05),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key=element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        strip.text.x = element_text(margin = margin(0.1,0,0.1,0, "cm")),
        strip.background=element_rect(colour="black",fill="white",size=0.05),
        legend.box.spacing = unit(0.02, 'cm'),
        legend.key.width = unit(0.3, 'cm'),
        legend.key.height = unit(0.05, 'cm'),
        text = element_text(size = 4))
Plot_Temp_Crab

# Crab ~ Depth
Data_Crab_Depth<-Data_Crab[variable=="Depth"]
Data_Crab_Depth[type=="Climate only",type:="C-SDM"]
Data_Crab_Depth[type=="Climate + Predator density",type:="P-SDM"]
Data_Crab_Depth[type=="Climate + Predation index",type:="PI-SDM"]
colnames(Data_Crab_Depth)[colnames(Data_Crab_Depth)=="type"] <- "Model"
Data_Crab_Depth[region_large=="NL_shelf",region_large:="Newfoundland & Labrador Shelf"]
Data_Crab_Depth[region_large=="SL_shelf",region_large:="St. Lawrence Shelf"]
Data_Crab_Depth[region_large=="SC_shelf",region_large:="Scotian Shelf"]
Data_Crab_Depth[region_large=="ART_shelf",region_large:="Arctic Shelf"]
colnames(Data_Crab_Depth)[colnames(Data_Crab_Depth)=="region_large"] <- "Region"
Plot_Depth_Crab<-ggplot(transform(Data_Crab_Depth,Model=factor(Model,levels=c("C-SDM","P-SDM","PI-SDM"))))+
  facet_wrap(~Region)+
  geom_ribbon(aes(depth,ymin=log(cilow),ymax=log(cihi),fill=Model),alpha=0.25)+
  geom_line(aes(depth,log(fit),group=interaction(Model,Region),color=Model),size=0.05)+
  scale_colour_manual(values=c("#D81A60","#004D41","#3D83C4"))+
  scale_fill_manual(values=c("#D81A60","#004D41","#3D83C4"))+
  ylab(expression(paste("log(Snow crab density)",(kg*"/"*km^{2}))))+xlab("Depth (m)")+
   ylim(-9,5)+
  theme(axis.line = element_line(colour = "black",size=0.05),
        axis.ticks = element_line(size=0.05),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key=element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        strip.text.x = element_text(margin = margin(0.1,0,0.1,0, "cm")),
        strip.background=element_rect(colour="black",fill="white",size=0.05),
        legend.box.spacing = unit(0.02, 'cm'),
        legend.key.width = unit(0.3, 'cm'),
        legend.key.height = unit(0.05, 'cm'),
        text = element_text(size = 4))
Plot_Depth_Crab

# Combine all shrimp and crab temperature and depth
Plot_Temp_Depth_Shrimp<-ggarrange(Plot_Temp_Shrimp,Plot_Depth_Shrimp,ncol=2,nrow=1,common.legend = TRUE,legend="top",
                           labels=c("(a)","(b)"),label.x=0.2,label.y=0.95,font.label = list(size = 3))
Plot_Temp_Depth_Crab<-ggarrange(Plot_Temp_Crab,Plot_Depth_Crab,ncol=1,nrow=2,common.legend=TRUE,legend="top",
                            labels=c("(a)","(b)"),label.x=0.1,label.y=0.85,font.label = list(size = 3))
Plot_Temp_Depth_Shrimp
Plot_Temp_Depth_Crab
ggsave("code/outputs/figures/Figure3_shrimp_depth_temp.jpeg",plot=Plot_Temp_Depth_Shrimp,dpi=600, width = 1961, height = 980.5, units = "px")
ggsave("code/outputs/figures/Figure4_crab_depth_temp.jpeg",plot=Plot_Temp_Depth_Crab,dpi=600, width = 1961, height = 1659.31, units = "px")

### PREDATOR PLOTS ----
rm(list=setdiff(ls(), c("Data_Shrimp","Data_Crab","Data")))

## Shrimp ~ Predator effects plot
Data_Pred_Shrimp<-Data_Shrimp[!(variable=="Depth"|variable=="Bottom temperature")&!(type=="Climate only")]
Data_Pred_Shrimp$type<-paste(Data_Pred_Shrimp$type,Data_Pred_Shrimp$variable)
Data_Pred_Shrimp<-melt(Data_Pred_Shrimp, id.vars = c("pred","fit","se.fit","cilow","cihi","type"), measure.vars = c("pred_cod","pred_turbot","pred_redfish","pred_index_shrimp"))
Data_Pred_Shrimp<-unique(Data_Pred_Shrimp)
Data_Pred_Shrimp<-Data_Pred_Shrimp[!(type=="Climate + Predator density Cod"&variable=="pred_turbot")]
Data_Pred_Shrimp<-Data_Pred_Shrimp[!(type=="Climate + Predator density Redfish"&variable=="pred_turbot")]
Data_Pred_Shrimp<-Data_Pred_Shrimp[!(type=="Climate + Predator density Redfish"&variable=="pred_cod")]
Data_Pred_Shrimp<-Data_Pred_Shrimp[!(type=="Climate + Predator density Greenland halibut"&variable=="pred_cod")]
Data_Pred_Shrimp<-Data_Pred_Shrimp[!(type=="Climate + Predator density Cod"&variable=="pred_redfish")]
Data_Pred_Shrimp<-Data_Pred_Shrimp[!(type=="Climate + Predator density Greenland halibut"&variable=="pred_redfish")]

Data_Pred_Shrimp[type=="Climate + Predation index Predation index",type:='Climate + Predation index']
Data_Pred_Shrimp[type=="Climate + Predator density Cod",type:="Climate + Predator density (Cod)"]
Data_Pred_Shrimp[type=="Climate + Predator density Greenland halibut",type:="Climate + Predator density (Greenland halibut)"]
Data_Pred_Shrimp[type=="Climate + Predator density Redfish",type:="Climate + Predator density (Redfish)"]
Data_Pred_Shrimp$type <- factor(Data_Pred_Shrimp$type,      # Reordering group factor levels
                         levels = c("Climate + Predator density (Cod)", "Climate + Predator density (Greenland halibut)", "Climate + Predator density (Redfish)", "Climate + Predation index"))

# Add P-value labels and facet labels
cod_text <- data.frame(x=2500,y=5800,lab = "p = 0.15",
                       type = factor("Climate + Predator density (Cod)",levels = c("Climate + Predator density (Cod)", "Climate + Predator density (Greenland halibut)", "Climate + Predator density (Redfish)", "Climate + Predation index")))
turbot_text <- data.frame(x=2500,y=4000,lab = "p < 0.001",
                          type = factor("Climate + Predator density (Greenland halibut)",levels = c("Climate + Predator density (Cod)", "Climate + Predator density (Greenland halibut)", "Climate + Predator density (Redfish)", "Climate + Predation index")))
redfish_text <- data.frame(x=2500,y=6000,lab = "p < 0.001",
                       type = factor("Climate + Predator density (Redfish)",levels = c("Climate + Predator density (Cod)", "Climate + Predator density (Greenland halibut)", "Climate + Predator density (Redfish)", "Climate + Predation index")))
index_text <- data.frame(x=2500,y=3500,lab = "p < 0.001",
                       type = factor("Climate + Predation index",levels = c("Climate + Predator density (Cod)", "Climate + Predator density (Greenland halibut)", "Climate + Predator density (Redfish)", "Climate + Predation index")))
cod_panel<-data.frame(x=0,y=6200,lab="(a)",
                      type = factor("Climate + Predator density (Cod)",levels = c("Climate + Predator density (Cod)", "Climate + Predator density (Greenland halibut)", "Climate + Predator density (Redfish)", "Climate + Predation index")))
turbot_panel <- data.frame(x=0,y=6200,lab = "(b)",
                          type = factor("Climate + Predator density (Greenland halibut)",levels = c("Climate + Predator density (Cod)", "Climate + Predator density (Greenland halibut)", "Climate + Predator density (Redfish)", "Climate + Predation index")))
redfish_panel <- data.frame(x=2500,y=6000,lab = "(c)",
                           type = factor("Climate + Predator density (Redfish)",levels = c("Climate + Predator density (Cod)", "Climate + Predator density (Greenland halibut)", "Climate + Predator density (Redfish)", "Climate + Predation index")))
index_panel <- data.frame(x=2500,y=3500,lab = "(d)",
                         type = factor("Climate + Predation index",levels = c("Climate + Predator density (Cod)", "Climate + Predator density (Greenland halibut)", "Climate + Predator density (Redfish)", "Climate + Predation index")))

#Shrimp predator effect plot
Plot_Pred_Shrimp<-ggplot((subset(Data_Pred_Shrimp)))+
  geom_ribbon(data=Data_Pred_Shrimp, aes(value,ymin=cilow,ymax=cihi),fill="grey35",alpha=0.25)+
  geom_line(data=Data_Pred_Shrimp, aes(value,fit),size=0.05)+
  facet_wrap(~type)+
  geom_text(data = cod_text,label = "p = 0.15",x=2500,y=5800,size=1)+
  geom_text(data = turbot_text,label = "p < 0.001",x=2500,y=4000,size=1)+
  geom_text(data = redfish_text,label = "p < 0.001",x=2500,y=6000,size=1)+
  geom_text(data = index_text,label = "p < 0.001",x=2500,y=3500,size=1)+
  geom_text(data = cod_panel,label = "(a)",x=100,y=6500,size=1)+
  geom_text(data = turbot_panel,label = "(b)",x=100,y=6500,size=1)+
  geom_text(data = redfish_panel,label = "(c)",x=100,y=6500,size=1)+
  geom_text(data = index_panel,label = "(d)",x=100,y=6500,size=1)+
  xlim(0,4000)+
  ylab(expression(paste("Northern shrimp density",(kg*"/"*km^{2}))))+xlab(expression(paste("Lagged predation index or predator density",(kg*"/"*km^{2}))))+
  theme(axis.line = element_line(colour = "black",size=0.05),
        axis.ticks = element_line(size=0.05),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key=element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        strip.text.x = element_text(margin = margin(0.1,0,0.1,0, "cm")),
        strip.background=element_rect(colour="black",size=0.05),
        strip.text = element_text(colour = 'white',size=4),
        legend.box.spacing = unit(0.02, 'cm'),
        legend.key.width = unit(0.3, 'cm'),
        legend.key.height = unit(0.05, 'cm'),
        text = element_text(size = 4))
Plot_Pred_Shrimp

g <- ggplot_gtable(ggplot_build(Plot_Pred_Shrimp))
strip_both <- which(grepl('strip-', g$layout$name))
fills <- c("#004D41","#3D83C4","#004D41","#004D41")
k <- 1

for (i in strip_both) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

ggsave("code/outputs/figures/Figure5_Shrimp_Predators.jpeg",plot=g,dpi=600, width = 1961, height = 1961, units = "px")

rm(list=setdiff(ls(), c("Data_Shrimp","Data_Crab","Data")))
### Snow crab ~ Predator effects plot

Data_Pred_Crab<-Data_Crab[!(variable=="Depth"|variable=="Bottom temperature")&!(type=="Climate only")]
Data_Pred_Crab$type<-paste(Data_Pred_Crab$type,Data_Pred_Crab$variable)
Data_Pred_Crab<-melt(Data_Pred_Crab, id.vars = c("pred","fit","se.fit","cilow","cihi","type","region_large"), measure.vars = c("pred_cod","pred_index_crab"))
Data_Pred_Crab<-unique(Data_Pred_Crab)

Data_Pred_Crab[type=="Climate + Predation index Predation index",type:='Climate + Predation index']
Data_Pred_Crab[type=="Climate + Predator density Cod",type:="Climate + Predator density (Cod)"]
Data_Pred_Crab$type <- factor(Data_Pred_Crab$type,      # Reordering group factor levels
                                levels = c("Climate + Predator density (Cod)", "Climate + Predation index"))
Data_Pred_Crab[region_large=="NL_shelf",region_large:="NL Shelf"]
Data_Pred_Crab[region_large=="SL_shelf",region_large:="St. Lawrence Shelf"]
Data_Pred_Crab[region_large=="SC_shelf",region_large:="Scotian Shelf"]
Data_Pred_Crab[region_large=="ART_shelf",region_large:="Arctic Shelf"]
colnames(Data_Pred_Crab)[colnames(Data_Pred_Crab)=="region_large"] <- "Region"

# Add P-value labels and panels
cod_text <- data.frame(x=250,y=10,lab = "p = 0.99",
                       type = factor("Climate + Predator density (Cod)",levels = c("Climate + Predator density (Cod)","Climate + Predation index")))
index_text <- data.frame(x=250,y=10,lab = "p = 0.006",
                         type = factor("Climate + Predation index",levels = c("Climate + Predator density (Cod)", "Climate + Predation index")))
cod_panel <- data.frame(x=250,y=10,lab = "(a)",
                       type = factor("Climate + Predator density (Cod)",levels = c("Climate + Predator density (Cod)","Climate + Predation index")),
                       Region=factor("NL Shelf",levels= c("NL Shelf","St. Lawrence Shelf","Scotian Shelf")))
index_panel <- data.frame(x=250,y=10,lab = "(b)",
                         type = factor("Climate + Predation index",levels = c("Climate + Predator density (Cod)", "Climate + Predation index")),
                         Region=factor("NL Shelf",levels= c("NL Shelf","St. Lawrence Shelf","Scotian Shelf")))

#Crab predator effect plot
Plot_Pred_Crab<-ggplot((subset(Data_Pred_Crab)))+
  geom_ribbon(data=Data_Pred_Crab, aes(value,ymin=cilow,ymax=cihi),fill="grey35",alpha=0.25)+
  geom_line(data=Data_Pred_Crab, aes(value,fit),size=0.05)+
  facet_wrap(type~Region)+
  geom_text(data = cod_text,label = "p = 0.99",x=250,y=10,size=1)+
  geom_text(data = index_text,label = "p = 0.006",x=250,y=10,size=1)+
  geom_text(data = cod_panel,label = "(a)",x=10,y=26.5,size=1)+
  geom_text(data = index_panel,label = "(b)",x=10,y=26.5,size=1)+
  xlim(0,450)+
  ylab(expression(paste("Snow crab density",(kg*"/"*km^{2}))))+xlab(expression(paste("Lagged predation index or predator density",(kg*"/"*km^{2}))))+
  theme(axis.line = element_line(colour = "black",size=0.05),
        axis.ticks = element_line(size=0.05),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key=element_blank(),
        panel.border = element_blank(),
        strip.background = element_rect(colour="black",fill="grey",size=0.05),
        panel.background = element_blank(),
        strip.text.x = element_text(margin = margin(0.1,0,0.1,0, "cm"),colour = 'white',size=4),
        legend.box.spacing = unit(0.02, 'cm'),
        legend.key.width = unit(0.3, 'cm'),
        legend.key.height = unit(0.05, 'cm'),
        text = element_text(size = 4))
Plot_Pred_Crab

g <- ggplot_gtable(ggplot_build(Plot_Pred_Crab))
strip_both <- which(grepl('strip-', g$layout$name))
fills <- c("#3D83C4","#3D83C4","#3D83C4","#004D41","#004D41","#004D41")
k <- 1

for (i in strip_both) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

ggsave("code/outputs/figures/Figure6_Crab_Predators.jpeg",plot=g,dpi=600, width = 1961, height = 1659.31, units = "px")


