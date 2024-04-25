### MODEL Effects for Diet models ----
# Purpose: Produce plots to visualize the effect of variables on diet models
# Author: S. Zabihi-Seissan

#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
### Packages ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

library(data.table)
library(googledrive)
library(ggplot2)
library(mgcv)
library(sp)
library(ggpubr)


#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
### Load models and data ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

# Load Data
Diet_Data<-fread('code/outputs/data/Diet_Data_Projected.csv')

# Load models
models_diet <- list.files("code/outputs/models/diet", pattern="*.rda", full.names=TRUE)
lapply(models_diet,load,.GlobalEnv)


#<><><><><>><>><><><><>><><><>><><><><><><>><>><><><><>><><><>><><><><><><>><>><><><><>><><><>><><><><><><>><>><><><
### Plot predictions ----
#<><><><><>><>><><><><>><><><>><><><><><><>><>><><><><>><><><>><><><><><><>><>><><><><>><><><>><><><><><><>><>><><><

## Cod_shrimp model

# Predict effect of temperature (Cod~shrimp model)
#fam <- family(cod_shrimp_presence)
#ilink <- fam$linkinv
NewData = expand.grid(temperature_at_bottom = seq(from = -1.4, to = 12, by = 0.1),
                      pred_shrimp =  753.0552,
                      pred_planktivores = 279.7577,
                      year_surv= 2000, #Doesn't matter (place holder)
                      region=c("maritimes","quebec","gulf","newfoundland_fall","newfoundland_spring"), # Doesnt matter (place holder)
                      dummy=0)
NewData$fit<-predict(cod_shrimp_presence,newdata = NewData,exclude="s(region,year_surv)")
#NewData$fit<-ilink(NewData$pred)
NewData$se.fit<-predict(cod_shrimp_presence,newdata = NewData,se.fit=TRUE)$se.fit
#NewData$cilow <- ilink(NewData$pred - (1.96 * NewData$se.fit))
NewData$cilow <- (NewData$fit - (1.96 * NewData$se.fit))
#NewData$cihi <- ilink(NewData$pred + (1.96 * NewData$se.fit))
NewData$cihi <- (NewData$fit + (1.96 * NewData$se.fit))
NewData<-as.data.table(NewData)
Data_Cod_Shrimp_Temp<-NewData
Data_Cod_Shrimp_Temp$type<-"Cod_shrimp"
Data_Cod_Shrimp_Temp$variable<-"Bottom temperature"

# Predict effect of predicted shrimp (Cod~shrimp model)
#fam <- family(cod_shrimp_presence)
#ilink <- fam$linkinv
NewData = expand.grid(pred_shrimp = seq(from = 0, to = 12593.18, by = 10),
                      temperature_at_bottom =  2.391031,
                      pred_planktivores = 279.7577,
                      year_surv= 2000, #Doesn't matter (place holder)
                      region=c("maritimes","quebec","gulf","newfoundland_fall","newfoundland_spring"), # Doesnt matter (place holder)
                      dummy=0)
NewData$fit<-predict(cod_shrimp_presence,newdata = NewData,exclude="s(region,year_surv)")
#NewData$fit<-ilink(NewData$pred)
NewData$se.fit<-predict(cod_shrimp_presence,newdata = NewData,se.fit=TRUE)$se.fit
#NewData$cilow <- ilink(NewData$pred - (1.96 * NewData$se.fit))
NewData$cilow <- (NewData$fit - (1.96 * NewData$se.fit))
#NewData$cihi <- ilink(NewData$pred + (1.96 * NewData$se.fit))
NewData$cihi <- (NewData$fit + (1.96 * NewData$se.fit))
NewData<-as.data.table(NewData)
Data_Cod_Shrimp_Shrimp<-NewData
Data_Cod_Shrimp_Shrimp$type<-"Cod_shrimp"
Data_Cod_Shrimp_Shrimp$variable<-"Predicted shrimp"

# Predict effect of predicted planktivore (Cod~shrimp model)
#fam <- family(cod_shrimp_presence)
#ilink <- fam$linkinv
NewData = expand.grid(pred_planktivores = seq(from = 0, to = 7439.869, by = 10),
                      temperature_at_bottom =  2.391031,
                      pred_shrimp = 753.0552,
                      year_surv= 2000, #Doesn't matter (place holder)
                      region=c("maritimes","quebec","gulf","newfoundland_fall","newfoundland_spring"), # Doesnt matter (place holder)
                      dummy=0)
NewData$fit<-predict(cod_shrimp_presence,newdata = NewData,exclude="s(region,year_surv)")
#NewData$fit<-ilink(NewData$pred)
NewData$se.fit<-predict(cod_shrimp_presence,newdata = NewData,se.fit=TRUE)$se.fit
#NewData$cilow <- ilink(NewData$pred - (1.96 * NewData$se.fit))
NewData$cilow <- (NewData$fit - (1.96 * NewData$se.fit))
#NewData$cihi <- ilink(NewData$pred + (1.96 * NewData$se.fit))
NewData$cihi <- (NewData$fit + (1.96 * NewData$se.fit))
NewData<-as.data.table(NewData)
Data_Cod_Shrimp_Plank<-NewData
Data_Cod_Shrimp_Plank$type<-"Cod_shrimp"
Data_Cod_Shrimp_Plank$variable<-"Predicted planktivores"


## Turbot_shrimp model

# Predict effect of temperature (turbot~shrimp model)
#fam <- family(turbot_shrimp_presence)
#ilink <- fam$linkinv
NewData = expand.grid(temperature_at_bottom = seq(from = -1.1, to = 6.95, by = 0.1),
                      pred_shrimp =  2135.291,
                      pred_planktivores = 213.0426,
                      year_surv= 2000, #Doesn't matter (place holder)
                      region=c("maritimes","quebec","gulf","newfoundland_fall","newfoundland_spring"), # Doesnt matter (place holder)
                      dummy=0)
NewData$fit<-predict(turbot_shrimp_presence,newdata = NewData,exclude="s(region,year_surv)")
#NewData$fit<-ilink(NewData$pred)
NewData$se.fit<-predict(turbot_shrimp_presence,newdata = NewData,se.fit=TRUE)$se.fit
#NewData$cilow <- ilink(NewData$pred - (1.96 * NewData$se.fit))
NewData$cilow <- (NewData$fit - (1.96 * NewData$se.fit))
#NewData$cihi <- ilink(NewData$pred + (1.96 * NewData$se.fit))
NewData$cihi <- (NewData$fit + (1.96 * NewData$se.fit))
NewData<-as.data.table(NewData)
Data_Turbot_Shrimp_Temp<-NewData
Data_Turbot_Shrimp_Temp$type<-"Turbot_shrimp"
Data_Turbot_Shrimp_Temp$variable<-"Bottom temperature"

# Predict effect of predicted shrimp (Turbot~shrimp model)
#fam <- family(turbot_shrimp_presence)
#ilink <- fam$linkinv
NewData = expand.grid(pred_shrimp = seq(from = 0, to = 18242.4, by = 10),
                      temperature_at_bottom =  3.1695,
                      pred_planktivores = 213.0426,
                      year_surv= 2000, #Doesn't matter (place holder)
                      region=c("maritimes","quebec","gulf","newfoundland_fall","newfoundland_spring"), # Doesnt matter (place holder)
                      dummy=0)
NewData$fit<-predict(turbot_shrimp_presence,newdata = NewData,exclude="s(region,year_surv)")
#NewData$fit<-ilink(NewData$pred)
NewData$se.fit<-predict(turbot_shrimp_presence,newdata = NewData,se.fit=TRUE)$se.fit
#NewData$cilow <- ilink(NewData$pred - (1.96 * NewData$se.fit))
NewData$cilow <- (NewData$fit - (1.96 * NewData$se.fit))
#NewData$cihi <- ilink(NewData$pred + (1.96 * NewData$se.fit))
NewData$cihi <- (NewData$fit + (1.96 * NewData$se.fit))
NewData<-as.data.table(NewData)
Data_Turbot_Shrimp_Shrimp<-NewData
Data_Turbot_Shrimp_Shrimp$type<-"Turbot_shrimp"
Data_Turbot_Shrimp_Shrimp$variable<-"Predicted shrimp"

# Predict effect of predicted planktivore (Turbot~shrimp model)
#fam <- family(turbot_shrimp_presence)
#ilink <- fam$linkinv
NewData = expand.grid(pred_planktivores = seq(from = 0, to = 9928.016, by = 10),
                      temperature_at_bottom =  3.340275,
                      pred_shrimp = 2135.291,
                      year_surv= 2000, #Doesn't matter (place holder)
                      region=c("maritimes","quebec","gulf","newfoundland_fall","newfoundland_spring"), # Doesnt matter (place holder)
                      dummy=0)
NewData$fit<-predict(turbot_shrimp_presence,newdata = NewData,exclude="s(region,year_surv)")
#NewData$fit<-ilink(NewData$pred)
NewData$se.fit<-predict(turbot_shrimp_presence,newdata = NewData,se.fit=TRUE)$se.fit
#NewData$cilow <- ilink(NewData$pred - (1.96 * NewData$se.fit))
NewData$cilow <- (NewData$fit - (1.96 * NewData$se.fit))
#NewData$cihi <- ilink(NewData$pred + (1.96 * NewData$se.fit))
NewData$cihi <- (NewData$fit + (1.96 * NewData$se.fit))
NewData<-as.data.table(NewData)
Data_Turbot_Shrimp_Plank<-NewData
Data_Turbot_Shrimp_Plank$type<-"Turbot_shrimp"
Data_Turbot_Shrimp_Plank$variable<-"Predicted planktivores"


## Redfish_shrimp model

# Predict effect of temperature (Redfish~shrimp model)
#fam <- family(redfish_shrimp_presence)
#ilink <- fam$linkinv
NewData = expand.grid(temperature_at_bottom = seq(from = -0.27, to = 11.05608, by = 0.1),
                      pred_shrimp =  702.4442,
                      pred_planktivores = 134.6003,
                      year_surv= 2000, #Doesn't matter (place holder)
                      region=c("maritimes","quebec","gulf","newfoundland_fall","newfoundland_spring"), # Doesnt matter (place holder)
                      dummy=0)
NewData$fit<-predict(redfish_shrimp_presence,newdata = NewData,exclude="s(region,year_surv)")
#NewData$fit<-ilink(NewData$pred)
NewData$se.fit<-predict(redfish_shrimp_presence,newdata = NewData,se.fit=TRUE)$se.fit
#NewData$cilow <- ilink(NewData$pred - (1.96 * NewData$se.fit))
NewData$cilow <- (NewData$fit - (1.96 * NewData$se.fit))
#NewData$cihi <- ilink(NewData$pred + (1.96 * NewData$se.fit))
NewData$cihi <- (NewData$fit + (1.96 * NewData$se.fit))
NewData<-as.data.table(NewData)
Data_Redfish_Shrimp_Temp<-NewData
Data_Redfish_Shrimp_Temp$type<-"Redfish_shrimp"
Data_Redfish_Shrimp_Temp$variable<-"Bottom temperature"

# Predict effect of predicted shrimp (Redfish~shrimp model)
#fam <- family(redfish_shrimp_presence)
#ilink <- fam$linkinv
NewData = expand.grid(pred_shrimp = seq(from = 0, to = 9449.961, by = 10),
                      temperature_at_bottom =  3.1695,
                      pred_planktivores = 134.6003,
                      year_surv= 2000, #Doesn't matter (place holder)
                      region=c("maritimes","quebec","gulf","newfoundland_fall","newfoundland_spring"), # Doesnt matter (place holder)
                      dummy=0)
NewData$fit<-predict(redfish_shrimp_presence,newdata = NewData,exclude="s(region,year_surv)")
#NewData$fit<-ilink(NewData$pred)
NewData$se.fit<-predict(redfish_shrimp_presence,newdata = NewData,se.fit=TRUE)$se.fit
#NewData$cilow <- ilink(NewData$pred - (1.96 * NewData$se.fit))
NewData$cilow <- (NewData$fit - (1.96 * NewData$se.fit))
#NewData$cihi <- ilink(NewData$pred + (1.96 * NewData$se.fit))
NewData$cihi <- (NewData$fit + (1.96 * NewData$se.fit))
NewData<-as.data.table(NewData)
Data_Redfish_Shrimp_Shrimp<-NewData
Data_Redfish_Shrimp_Shrimp$type<-"Redfish_shrimp"
Data_Redfish_Shrimp_Shrimp$variable<-"Predicted shrimp"

# Predict effect of predicted planktivore (Redfish~shrimp model)
#fam <- family(redfish_shrimp_presence)
#ilink <- fam$linkinv
NewData = expand.grid(pred_planktivores = seq(from = 0, to = 3539.228, by = 10),
                      temperature_at_bottom =  4.987519,
                      pred_shrimp = 702.4442,
                      year_surv= 2000, #Doesn't matter (place holder)
                      region=c("maritimes","quebec","gulf","newfoundland_fall","newfoundland_spring"), # Doesnt matter (place holder)
                      dummy=0)
NewData$fit<-predict(redfish_shrimp_presence,newdata = NewData,exclude="s(region,year_surv)")
#NewData$fit<-ilink(NewData$pred)
NewData$se.fit<-predict(redfish_shrimp_presence,newdata = NewData,se.fit=TRUE)$se.fit
#NewData$cilow <- ilink(NewData$pred - (1.96 * NewData$se.fit))
NewData$cilow <- (NewData$fit - (1.96 * NewData$se.fit))
#NewData$cihi <- ilink(NewData$pred + (1.96 * NewData$se.fit))
NewData$cihi <- (NewData$fit + (1.96 * NewData$se.fit))
NewData<-as.data.table(NewData)
Data_Redfish_Shrimp_Plank<-NewData
Data_Redfish_Shrimp_Plank$type<-"Redfish_shrimp"
Data_Redfish_Shrimp_Plank$variable<-"Predicted planktivores"


## Cod_crab model

# Predict effect of temperature (Cod~crab model)
#fam <- family(cod_crab_presence)
#ilink <- fam$linkinv
NewData = expand.grid(temperature_at_bottom = seq(from = -1.4, to = 12, by = 0.1),
                      pred_crab =  0.1577779,
                      pred_planktivores = 279.7577,
                      year_surv= 2000, #Doesn't matter (place holder)
                      region=c("maritimes","quebec","gulf","newfoundland_fall","newfoundland_spring"), # Doesnt matter (place holder)
                      dummy=0)
NewData$fit<-predict(cod_crab_presence,newdata = NewData,exclude="s(region,year_surv)")
#NewData$fit<-ilink(NewData$pred)
NewData$se.fit<-predict(cod_crab_presence,newdata = NewData,se.fit=TRUE)$se.fit
#NewData$cilow <- ilink(NewData$pred - (1.96 * NewData$se.fit))
NewData$cilow <- (NewData$fit - (1.96 * NewData$se.fit))
#NewData$cihi <- ilink(NewData$pred + (1.96 * NewData$se.fit))
NewData$cihi <- (NewData$fit + (1.96 * NewData$se.fit))
NewData<-as.data.table(NewData)
Data_Cod_Crab_Temp<-NewData
Data_Cod_Crab_Temp$type<-"Cod_crab"
Data_Cod_Crab_Temp$variable<-"Bottom temperature"

# Predict effect of predicted crab (Cod~crab model)
#fam <- family(cod_crab_presence)
#ilink <- fam$linkinv
NewData = expand.grid(pred_crab = seq(from = 0, to = 4.05435, by = 0.5),
                      temperature_at_bottom =  2.391031,
                      pred_planktivores = 279.7577,
                      year_surv= 2000, #Doesn't matter (place holder)
                      region=c("maritimes","quebec","gulf","newfoundland_fall","newfoundland_spring"), # Doesnt matter (place holder)
                      dummy=0)
NewData$fit<-predict(cod_crab_presence,newdata = NewData,exclude="s(region,year_surv)")
#NewData$fit<-ilink(NewData$pred)
NewData$se.fit<-predict(cod_crab_presence,newdata = NewData,se.fit=TRUE)$se.fit
#NewData$cilow <- ilink(NewData$pred - (1.96 * NewData$se.fit))
NewData$cilow <- (NewData$fit - (1.96 * NewData$se.fit))
#NewData$cihi <- ilink(NewData$pred + (1.96 * NewData$se.fit))
NewData$cihi <- (NewData$fit + (1.96 * NewData$se.fit))
NewData<-as.data.table(NewData)
Data_Cod_Crab_Crab<-NewData
Data_Cod_Crab_Crab$type<-"Cod_crab"
Data_Cod_Crab_Crab$variable<-"Predicted crab"

# Predict effect of predicted planktivore (Cod~crab model)
#fam <- family(cod_crab_presence)
#ilink <- fam$linkinv
NewData = expand.grid(pred_planktivores = seq(from = 0, to = 7439.869, by = 10),
                      temperature_at_bottom =  2.391031,
                      pred_crab = 0.1577779,
                      year_surv= 2000, #Doesn't matter (place holder)
                      region=c("maritimes","quebec","gulf","newfoundland_fall","newfoundland_spring"), # Doesnt matter (place holder)
                      dummy=0)
NewData$fit<-predict(cod_crab_presence,newdata = NewData,exclude="s(region,year_surv)")
#NewData$fit<-ilink(NewData$pred)
NewData$se.fit<-predict(cod_crab_presence,newdata = NewData,se.fit=TRUE)$se.fit
#NewData$cilow <- ilink(NewData$pred - (1.96 * NewData$se.fit))
NewData$cilow <- (NewData$fit - (1.96 * NewData$se.fit))
#NewData$cihi <- ilink(NewData$pred + (1.96 * NewData$se.fit))
NewData$cihi <- (NewData$fit + (1.96 * NewData$se.fit))
NewData<-as.data.table(NewData)
Data_Cod_Crab_Plank<-NewData
Data_Cod_Crab_Plank$type<-"Cod_crab"
Data_Cod_Crab_Plank$variable<-"Predicted planktivores"

## Clean slate
rm(list=setdiff(ls(), c("Data_Cod_Crab_Temp","Data_Cod_Crab_Plank","Data_Cod_Crab_Crab",
                        "Data_Cod_Shrimp_Temp","Data_Cod_Shrimp_Plank","Data_Cod_Shrimp_Shrimp",
                        "Data_Turbot_Shrimp_Temp","Data_Turbot_Shrimp_Plank","Data_Turbot_Shrimp_Shrimp",
                        "Data_Redfish_Shrimp_Temp","Data_Redfish_Shrimp_Plank","Data_Redfish_Shrimp_Shrimp","Diet_Data")))

# Bind data together
Data<-rbind(Data_Cod_Crab_Temp,Data_Cod_Crab_Plank,Data_Cod_Crab_Crab,
            Data_Cod_Shrimp_Temp,Data_Cod_Shrimp_Plank,Data_Cod_Shrimp_Shrimp,
            Data_Turbot_Shrimp_Temp,Data_Turbot_Shrimp_Plank,Data_Turbot_Shrimp_Shrimp,
            Data_Redfish_Shrimp_Temp,Data_Redfish_Shrimp_Plank,Data_Redfish_Shrimp_Shrimp,fill=TRUE)

# Clean slate
rm(list=setdiff(ls(), c("Data","Diet_Data")))


#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
### Plotting ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

# Add raw diet data for geom_rug
Data_Diet<-Diet_Data
Data_Diet$Type<-Data_Diet$predator_species
Data_Diet[Type=="gadus_morhua",Type:="Atlantic cod diet ~ northern shrimp presence"]
Data_Diet[Type=="reinhardtius_hippoglossoides",Type:="Greenland halibut diet ~ northern shrimp presence"]
Data_Diet[Type=="sebastes_mentella",Type:="Redfish diet ~ northern shrimp presence"]
# Add another set of cod stomach data for crab plots
Data_Diet2<-Data_Diet[Type=="Atlantic cod diet ~ northern shrimp presence"]
Data_Diet2[Type=="Atlantic cod diet ~ northern shrimp presence",Type:="Atlantic cod diet ~ snow crab presence"]
Data_Diet2$pred_shrimp<-Data_Diet2$pred_crab
Data_Diet2$pandalus_borealis<-Data_Diet2$chionoecetes_opilio_small
Data_Diet<-rbind(Data_Diet,Data_Diet2)
#Remove points beyond 99.9 percentile
Data_Diet[Type=="Atlantic cod diet ~ northern shrimp presence"&temperature_at_bottom>12,temperature_at_bottom:=NA]
Data_Diet[Type=="Atlantic cod diet ~ northern shrimp presence"&pred_shrimp>12593.18,pred_shrimp:=NA]
Data_Diet[Type=="Atlantic cod diet ~ northern shrimp presence"&pred_planktivores>7439.869,pred_planktivores:=NA]
Data_Diet[Type=="Greenland halibut diet ~ northern shrimp presence"&temperature_at_bottom>6.95,temperature_at_bottom:=NA]
Data_Diet[Type=="Greenland halibut diet ~ northern shrimp presence"&pred_shrimp>18242.4,pred_shrimp:=NA]
Data_Diet[Type=="Greenland halibut diet ~ northern shrimp presence"&pred_planktivores>9928.016,pred_planktivores:=NA]
Data_Diet[Type=="Redfish diet ~ northern shrimp presence"&temperature_at_bottom>11.05608,temperature_at_bottom:=NA]
Data_Diet[Type=="Redfish diet ~ northern shrimp presence"&pred_shrimp>9449.961,pred_shrimp:=NA]
Data_Diet[Type=="Redfish diet ~ northern shrimp presence"&pred_planktivores>3539.228,pred_planktivores:=NA]
Data_Diet[Type=="Atlantic cod diet ~ snow crab presence"&temperature_at_bottom>12,temperature_at_bottom:=NA]
Data_Diet[Type=="Atlantic cod diet ~ snow crab presence"&pred_shrimp>4.05435,pred_shrimp:=NA]
Data_Diet[Type=="Atlantic cod diet ~ snow crab presence"&pred_planktivores>7439.869,pred_planktivores:=NA]

rm(list=setdiff(ls(), c("Data","Diet_Data","Data_Diet")))

## Presence ~ Temperature effects for all four models
cod_shrimp_panel<-data.frame(Type=factor("Atlantic cod diet ~ northern shrimp presence"))
cod_crab_panel<-data.frame(Type=factor("Atlantic cod diet ~ snow crab presence"))
turbot_shrimp_panel<-data.frame(Type=factor("Greenland halibut diet ~ northern shrimp presence"))
redfish_shrimp_panel<-data.frame(Type=factor("Redfish diet ~ northern shrimp presence"))
Data_Temp<-Data[variable=="Bottom temperature"]
Data_Temp[type=="Cod_crab",type:="Atlantic cod diet ~ snow crab presence"]
Data_Temp[type=="Cod_shrimp",type:="Atlantic cod diet ~ northern shrimp presence"]
Data_Temp[type=="Turbot_shrimp",type:="Greenland halibut diet ~ northern shrimp presence"]
Data_Temp[type=="Redfish_shrimp",type:="Redfish diet ~ northern shrimp presence"]
colnames(Data_Temp)[colnames(Data_Temp)=="type"] <- "Type"
Plot_Temp<-ggplot(transform(Data_Temp,Type=factor(Type,levels=c("Atlantic cod diet ~ northern shrimp presence","Atlantic cod diet ~ snow crab presence","Greenland halibut diet ~ northern shrimp presence","Redfish diet ~ northern shrimp presence"))))+
  geom_ribbon(aes(temperature_at_bottom,ymin=cilow,ymax=cihi),alpha=0.25)+
  geom_line(aes(temperature_at_bottom,fit),size=0.05)+
  geom_rug(data=Data_Diet,aes(temperature_at_bottom),sides="b",size=0.05)+
  geom_text(data = cod_shrimp_panel,label = "(a)",x=-2,y=-2.5,size=1)+
  geom_text(data = cod_crab_panel,label = "(b)",x=-2,y=-2.5,size=1)+
  geom_text(data = turbot_shrimp_panel,label = "(c)",x=-1.6,y=-2.5,size=1)+
  geom_text(data = redfish_shrimp_panel,label = "(d)",x=-1.3,y=-2.5,size=1)+
  facet_wrap(~Type,scales="free_x")+
  ylab(expression("Logit-scale likelihood of presence in stomach" ))+
  xlab("Temperature at bottom (°C)")+
  theme(axis.line = element_line(colour = "black",size=0.05),
        axis.ticks = element_line(size=0.05),
        strip.background = element_rect(
          color="black", fill="white", size=0.05, linetype="solid"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key=element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 4))
Plot_Temp

## Presence ~ Shrimp/crab effects for four model
Data_Prey<-Data[variable=="Predicted shrimp"|variable=="Predicted crab"]
Data_Prey[variable=="Predicted crab",pred_shrimp:=pred_crab]
Data_Prey[type=="Cod_shrimp",type:="Atlantic cod diet ~ northern shrimp presence"]
Data_Prey[type=="Cod_crab",type:="Atlantic cod diet ~ snow crab presence"]
Data_Prey[type=="Turbot_shrimp",type:="Greenland halibut diet ~ northern shrimp presence"]
Data_Prey[type=="Redfish_shrimp",type:="Redfish diet ~ northern shrimp presence"]
colnames(Data_Prey)[colnames(Data_Prey)=="type"] <- "Type"
Plot_Prey<-ggplot(transform(Data_Prey,Type=factor(Type,levels=c("Atlantic cod diet ~ northern shrimp presence","Atlantic cod diet ~ snow crab presence","Greenland halibut diet ~ northern shrimp presence","Redfish diet ~ northern shrimp presence"))))+
  geom_ribbon(aes(pred_shrimp,ymin=cilow,ymax=cihi),alpha=0.25)+
  geom_line(aes(pred_shrimp,fit),size=0.05)+
  geom_rug(data=Data_Diet,aes(pred_shrimp),sides="b",size=0.05)+
  geom_text(data = cod_shrimp_panel,label = "(a)",x=-0,y=0.70,size=1)+
  geom_text(data = cod_crab_panel,label = "(b)",x=0,y=0.70,size=1)+
  geom_text(data = turbot_shrimp_panel,label = "(c)",x=0,y=0.70,size=1)+
  geom_text(data = redfish_shrimp_panel,label = "(d)",x=0,y=0.70,size=1)+
  facet_wrap(~Type,scales="free_x")+
  ylab(expression("Logit-scale likelihood of presence in stomach" ))+
  xlab((expression(paste("Predicted prey density",(kg*"/"*km^{2})))))+
  theme(axis.line = element_line(colour = "black",size=0.05),
        axis.ticks = element_line(size=0.05),
        strip.background = element_rect(
          color="black", fill="white", size=0.05, linetype="solid"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key=element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 4))
Plot_Prey

## Presence ~ Planktivore effects
Data_Plank<-Data[variable=="Predicted planktivores"]
Data_Plank[type=="Cod_shrimp",type:="Atlantic cod diet ~ northern shrimp presence"]
Data_Plank[type=="Cod_crab",type:="Atlantic cod diet ~ snow crab presence"]
Data_Plank[type=="Turbot_shrimp",type:="Greenland halibut diet ~ northern shrimp presence"]
Data_Plank[type=="Redfish_shrimp",type:="Redfish diet ~ northern shrimp presence"]
colnames(Data_Plank)[colnames(Data_Plank)=="type"] <- "Type"
Plot_Plank<-ggplot(transform(Data_Plank,Type=factor(Type,levels=c("Atlantic cod diet ~ northern shrimp presence","Atlantic cod diet ~ snow crab presence","Greenland halibut diet ~ northern shrimp presence","Redfish diet ~ northern shrimp presence"))))+
  geom_ribbon(aes(pred_planktivores,ymin=cilow,ymax=cihi),alpha=0.25)+
  geom_line(aes(pred_planktivores,fit),size=0.05)+
  geom_rug(data=Data_Diet,aes(pred_planktivores),sides="b",size=0.05)+
  geom_text(data = cod_shrimp_panel,label = "(a)",x=0,y=-1.5,size=1)+
  geom_text(data = cod_crab_panel,label = "(b)",x=0,y=-1.5,size=1)+
  geom_text(data = turbot_shrimp_panel,label = "(c)",x=0,y=-1.5,size=1)+
  geom_text(data = redfish_shrimp_panel,label = "(d)",x=0,y=-1.5,size=1)+
  facet_wrap(~Type,scales="free_x")+
  ylim(-15,-1)+
  ylab(expression("Logit-scale likelihood of presence in stomach" ))+
  xlab((expression(paste("Predicted planktivore density",(kg*"/"*km^{2})))))+
  theme(axis.line = element_line(colour = "black",size=0.05),
        axis.ticks = element_line(size=0.05),
        strip.background = element_rect(
          color="black", fill="white", size=0.05, linetype="solid"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key=element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 4))
Plot_Plank

# Save plots

ggsave("code/outputs/figures/FigureS3_diet_temp.jpeg",plot=Plot_Temp,dpi=600, width = 1961, height = 1176.6, units = "px")
ggsave("code/outputs/figures/FigureS1_diet_prey.jpeg",plot=Plot_Prey,dpi=600, width = 1961, height = 1176.6, units = "px")
ggsave("code/outputs/figures/FigureS2_diet_plank.jpeg",plot=Plot_Plank,dpi=600, width = 1961, height = 1176.6, units = "px")
