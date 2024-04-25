### K-Fold Cross validation for predator diet models ----
# Purpose: Validated diet models using method by #CITE PAPER. Validation done by NAFO divisions.
# Author: S. Zabihi-Seissan

#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
### Packages ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

library(data.table)
library(mgcv)
library(sp)
library(rgdal)


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

# Only keep predator species of interest
Diet_Data<-Diet_Data[predator_species=="reinhardtius_hippoglossoides"|predator_species=="gadus_morhua"|predator_species=="sebastes_mentella"]


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
rm(list=setdiff(ls(), c("Diet_Data")))

#Calculate presence/absence of shrimp and crab in diet
Diet_Data[pandalus_borealis_weight==0,shrimp_pres:=0]
Diet_Data[pandalus_borealis_weight>0,shrimp_pres:=1]
Diet_Data[chionoecetes_opilio_weight==0,crab_pres:=0]
Diet_Data[chionoecetes_opilio_weight>0,crab_pres:=1]

Diet_Data$dummy<-1
#Presence absence models
Diet_Data$region<-as.factor(Diet_Data$region)
Diet_Data$year_surv<-as.factor(Diet_Data$year_surv)


#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
### K-fold Models ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

get_deviance <- function(model, y_pred, y_obs, weights = NULL){
  stopifnot(length(y_obs)==length(y_pred))
  if(is.null(weights)) weights = rep(1, times= length(y_obs))
  dev_residuals = model$family$dev.resids(y_obs, y_pred, weights)
  return(sum(dev_residuals))
}

### Atlantic cod ~ Northern shrimp ----

Data_Cod<-Diet_Data[predator_species=="gadus_morhua"]

#Randomly bin data
set.seed(5)
Data_Cod$rand.vec <- sample(1:5,nrow(Data_Cod),replace=TRUE)

# Fit the model in all folds but one.
gam_model_group1 <- gam(shrimp_pres~s(temperature_at_bottom,k=6)+
                          pred_shrimp+pred_planktivores+
                          s(region,year_surv,bs="re",by=dummy),family=binomial,data=Data_Cod[rand.vec!=1],method="REML")
gam_model_group2 <- gam(shrimp_pres~s(temperature_at_bottom,k=6)+
                          pred_shrimp+pred_planktivores+
                          s(region,year_surv,bs="re",by=dummy),family=binomial,data=Data_Cod[rand.vec!=2],method="REML")
gam_model_group3 <- gam(shrimp_pres~s(temperature_at_bottom,k=6)+
                          pred_shrimp+pred_planktivores+
                          s(region,year_surv,bs="re",by=dummy),family=binomial,data=Data_Cod[rand.vec!=3],method="REML")
gam_model_group4 <- gam(shrimp_pres~s(temperature_at_bottom,k=6)+
                          pred_shrimp+pred_planktivores+
                          s(region,year_surv,bs="re",by=dummy),family=binomial,data=Data_Cod[rand.vec!=4],method="REML")
gam_model_group5 <- gam(shrimp_pres~s(temperature_at_bottom,k=6)+
                          pred_shrimp+pred_planktivores+
                          s(region,year_surv,bs="re",by=dummy),family=binomial,data=Data_Cod[rand.vec!=5],method="REML")

#Finds the score for the held-out subset of data.
attach(Data_Cod)
Data_Cod$scores[rand.vec ==1] = predict(gam_model_group1, newdata = Data_Cod[rand.vec ==1], type = "response")
Data_Cod$scores[rand.vec ==2] = predict(gam_model_group2, newdata = Data_Cod[rand.vec ==2], type = "response")
Data_Cod$scores[rand.vec ==3] = predict(gam_model_group3, newdata = Data_Cod[rand.vec ==3], type = "response")
Data_Cod$scores[rand.vec ==4] = predict(gam_model_group4, newdata = Data_Cod[rand.vec ==4], type = "response")
Data_Cod$scores[rand.vec ==5] = predict(gam_model_group5, newdata = Data_Cod[rand.vec ==5], type = "response")
detach(Data_Cod)

# Run the k-fold CV evaluation sensu Boyce et al. 2002
dataset <- Data_Cod[complete.cases(Data_Cod[,"scores"]),]
dataset<-dataset[!is.na(shrimp_pres)]

cod_shrimp_model_deviance <- numeric(5)

dataset$rand.vec<-as.factor(dataset$rand.vec)

attach(dataset)
cod_shrimp_model_deviance[1]<-get_deviance(gam_model_group1,dataset$scores[rand.vec=="1"],dataset$shrimp_pres[rand.vec=="1"],weights=NULL)
cod_shrimp_model_deviance[2]<-get_deviance(gam_model_group2,dataset$scores[rand.vec=="2"],dataset$shrimp_pres[rand.vec=="2"],weights=NULL)
cod_shrimp_model_deviance[3]<-get_deviance(gam_model_group3,dataset$scores[rand.vec=="3"],dataset$shrimp_pres[rand.vec=="3"],weights=NULL)
cod_shrimp_model_deviance[4]<-get_deviance(gam_model_group4,dataset$scores[rand.vec=="4"],dataset$shrimp_pres[rand.vec=="4"],weights=NULL)
cod_shrimp_model_deviance[5]<-get_deviance(gam_model_group5,dataset$scores[rand.vec=="5"],dataset$shrimp_pres[rand.vec=="5"],weights=NULL)
detach(dataset)

cod_shrimp_model_deviance<-as.data.table(cod_shrimp_model_deviance)

rm(list=setdiff(ls(), c("Diet_Data","cod_shrimp_model_deviance","get_deviance")))


### Greenland halibut ~ Northern shrimp ----

Data_Turbot<-Diet_Data[predator_species=="reinhardtius_hippoglossoides"]

#Randomly bin data
set.seed(5)
Data_Turbot$rand.vec <- sample(1:5,nrow(Data_Turbot),replace=TRUE)

# Fit the model in all folds but one.
gam_model_group1 <- gam(shrimp_pres~s(temperature_at_bottom,k=6)+
                          pred_shrimp+pred_planktivores+
                          s(region,year_surv,bs="re",by=dummy),family=binomial,data=Data_Turbot[rand.vec!=1],method="REML")
gam_model_group2 <- gam(shrimp_pres~s(temperature_at_bottom,k=6)+
                          pred_shrimp+pred_planktivores+
                          s(region,year_surv,bs="re",by=dummy),family=binomial,data=Data_Turbot[rand.vec!=2],method="REML")
gam_model_group3 <- gam(shrimp_pres~s(temperature_at_bottom,k=6)+
                          pred_shrimp+pred_planktivores+
                          s(region,year_surv,bs="re",by=dummy),family=binomial,data=Data_Turbot[rand.vec!=3],method="REML")
gam_model_group4 <- gam(shrimp_pres~s(temperature_at_bottom,k=6)+
                          pred_shrimp+pred_planktivores+
                          s(region,year_surv,bs="re",by=dummy),family=binomial,data=Data_Turbot[rand.vec!=4],method="REML")
gam_model_group5 <- gam(shrimp_pres~s(temperature_at_bottom,k=6)+
                          pred_shrimp+pred_planktivores+
                          s(region,year_surv,bs="re",by=dummy),family=binomial,data=Data_Turbot[rand.vec!=5],method="REML")

#Finds the score for the held-out subset of data.
attach(Data_Turbot)
Data_Turbot$scores[rand.vec ==1] = predict(gam_model_group1, newdata = Data_Turbot[rand.vec ==1], type = "response")
Data_Turbot$scores[rand.vec ==2] = predict(gam_model_group2, newdata = Data_Turbot[rand.vec ==2], type = "response")
Data_Turbot$scores[rand.vec ==3] = predict(gam_model_group3, newdata = Data_Turbot[rand.vec ==3], type = "response")
Data_Turbot$scores[rand.vec ==4] = predict(gam_model_group4, newdata = Data_Turbot[rand.vec ==4], type = "response")
Data_Turbot$scores[rand.vec ==5] = predict(gam_model_group5, newdata = Data_Turbot[rand.vec ==5], type = "response")
detach(Data_Turbot)

# Run the k-fold CV evaluation sensu Boyce et al. 2002
dataset <- Data_Turbot[complete.cases(Data_Turbot[,"scores"]),]
dataset<-dataset[!is.na(shrimp_pres)]

turbot_shrimp_model_deviance <- numeric(5)

dataset$rand.vec<-as.factor(dataset$rand.vec)

attach(dataset)
turbot_shrimp_model_deviance[1]<-get_deviance(gam_model_group1,dataset$scores[rand.vec=="1"],dataset$shrimp_pres[rand.vec=="1"],weights=NULL)
turbot_shrimp_model_deviance[2]<-get_deviance(gam_model_group2,dataset$scores[rand.vec=="2"],dataset$shrimp_pres[rand.vec=="2"],weights=NULL)
turbot_shrimp_model_deviance[3]<-get_deviance(gam_model_group3,dataset$scores[rand.vec=="3"],dataset$shrimp_pres[rand.vec=="3"],weights=NULL)
turbot_shrimp_model_deviance[4]<-get_deviance(gam_model_group4,dataset$scores[rand.vec=="4"],dataset$shrimp_pres[rand.vec=="4"],weights=NULL)
turbot_shrimp_model_deviance[5]<-get_deviance(gam_model_group5,dataset$scores[rand.vec=="5"],dataset$shrimp_pres[rand.vec=="5"],weights=NULL)
detach(dataset)

turbot_shrimp_model_deviance<-as.data.table(turbot_shrimp_model_deviance)

rm(list=setdiff(ls(), c("Diet_Data","cod_shrimp_model_deviance","turbot_shrimp_model_deviance","get_deviance")))


### Redfish ~ Northern shrimp ----

Data_Redfish<-Diet_Data[predator_species=="sebastes_mentella"]

#Randomly bin data
set.seed(5)
Data_Redfish$rand.vec <- sample(1:5,nrow(Data_Redfish),replace=TRUE)
Data_Redfish<-Data_Redfish[!year_surv==1999]

# Fit the model in all folds but one.
gam_model_group1 <- gam(shrimp_pres~s(temperature_at_bottom,k=6)+
                          pred_shrimp+pred_planktivores+
                          s(region,year_surv,bs="re",by=dummy),family=binomial,data=Data_Redfish[rand.vec!=1],method="REML")
gam_model_group2 <- gam(shrimp_pres~s(temperature_at_bottom,k=6)+
                          pred_shrimp+pred_planktivores+
                          s(region,year_surv,bs="re",by=dummy),family=binomial,data=Data_Redfish[rand.vec!=2],method="REML")
gam_model_group3 <- gam(shrimp_pres~s(temperature_at_bottom,k=6)+
                          pred_shrimp+pred_planktivores+
                          s(region,year_surv,bs="re",by=dummy),family=binomial,data=Data_Redfish[rand.vec!=3],method="REML")
gam_model_group4 <- gam(shrimp_pres~s(temperature_at_bottom,k=6)+
                          pred_shrimp+pred_planktivores+
                          s(region,year_surv,bs="re",by=dummy),family=binomial,data=Data_Redfish[rand.vec!=4],method="REML")
gam_model_group5 <- gam(shrimp_pres~s(temperature_at_bottom,k=6)+
                          pred_shrimp+pred_planktivores+
                          s(region,year_surv,bs="re",by=dummy),family=binomial,data=Data_Redfish[rand.vec!=5],method="REML")

#Finds the score for the held-out subset of data.
attach(Data_Redfish)
Data_Redfish$scores[rand.vec ==1] = predict(gam_model_group1, newdata = Data_Redfish[rand.vec ==1], type = "response")
Data_Redfish$scores[rand.vec ==2] = predict(gam_model_group2, newdata = Data_Redfish[rand.vec ==2], type = "response")
Data_Redfish$scores[rand.vec ==3] = predict(gam_model_group3, newdata = Data_Redfish[rand.vec ==3], type = "response")
Data_Redfish$scores[rand.vec ==4] = predict(gam_model_group4, newdata = Data_Redfish[rand.vec ==4], type = "response")
Data_Redfish$scores[rand.vec ==5] = predict(gam_model_group5, newdata = Data_Redfish[rand.vec ==5], type = "response")
detach(Data_Redfish)

# Run the k-fold CV evaluation sensu Boyce et al. 2002
dataset <- Data_Redfish[complete.cases(Data_Redfish[,"scores"]),]
dataset<-dataset[!is.na(shrimp_pres)]

redfish_shrimp_model_deviance <- numeric(5)

dataset$rand.vec<-as.factor(dataset$rand.vec)

attach(dataset)
redfish_shrimp_model_deviance[1]<-get_deviance(gam_model_group1,dataset$scores[rand.vec=="1"],dataset$shrimp_pres[rand.vec=="1"],weights=NULL)
redfish_shrimp_model_deviance[2]<-get_deviance(gam_model_group2,dataset$scores[rand.vec=="2"],dataset$shrimp_pres[rand.vec=="2"],weights=NULL)
redfish_shrimp_model_deviance[3]<-get_deviance(gam_model_group3,dataset$scores[rand.vec=="3"],dataset$shrimp_pres[rand.vec=="3"],weights=NULL)
redfish_shrimp_model_deviance[4]<-get_deviance(gam_model_group4,dataset$scores[rand.vec=="4"],dataset$shrimp_pres[rand.vec=="4"],weights=NULL)
redfish_shrimp_model_deviance[5]<-get_deviance(gam_model_group5,dataset$scores[rand.vec=="5"],dataset$shrimp_pres[rand.vec=="5"],weights=NULL)
detach(dataset)

redfish_shrimp_model_deviance<-as.data.table(redfish_shrimp_model_deviance)

rm(list=setdiff(ls(), c("Diet_Data","cod_shrimp_model_deviance","turbot_shrimp_model_deviance","redfish_shrimp_model_deviance","get_deviance")))


### Atlantic cod ~ Snow crab ----

Data_Cod<-Diet_Data[predator_species=="gadus_morhua"]

#Randomly bin data
set.seed(5)
Data_Cod$rand.vec <- sample(1:5,nrow(Data_Cod),replace=TRUE)

# Fit the model in all folds but one.
gam_model_group1 <- gam(crab_pres~s(temperature_at_bottom,k=6)+
                          pred_crab+pred_planktivores+
                          s(region,year_surv,bs="re",by=dummy),family=binomial,data=Data_Cod[rand.vec!=1],method="REML")
gam_model_group2 <- gam(crab_pres~s(temperature_at_bottom,k=6)+
                          pred_crab+pred_planktivores+
                          s(region,year_surv,bs="re",by=dummy),family=binomial,data=Data_Cod[rand.vec!=2],method="REML")
gam_model_group3 <- gam(crab_pres~s(temperature_at_bottom,k=6)+
                          pred_crab+pred_planktivores+
                          s(region,year_surv,bs="re",by=dummy),family=binomial,data=Data_Cod[rand.vec!=3],method="REML")
gam_model_group4 <- gam(crab_pres~s(temperature_at_bottom,k=6)+
                          pred_crab+pred_planktivores+
                          s(region,year_surv,bs="re",by=dummy),family=binomial,data=Data_Cod[rand.vec!=4],method="REML")
gam_model_group5 <- gam(crab_pres~s(temperature_at_bottom,k=6)+
                          pred_crab+pred_planktivores+
                          s(region,year_surv,bs="re",by=dummy),family=binomial,data=Data_Cod[rand.vec!=5],method="REML")

#Finds the score for the held-out subset of data.
attach(Data_Cod)
Data_Cod$scores[rand.vec ==1] = predict(gam_model_group1, newdata = Data_Cod[rand.vec ==1], type = "response")
Data_Cod$scores[rand.vec ==2] = predict(gam_model_group2, newdata = Data_Cod[rand.vec ==2], type = "response")
Data_Cod$scores[rand.vec ==3] = predict(gam_model_group3, newdata = Data_Cod[rand.vec ==3], type = "response")
Data_Cod$scores[rand.vec ==4] = predict(gam_model_group4, newdata = Data_Cod[rand.vec ==4], type = "response")
Data_Cod$scores[rand.vec ==5] = predict(gam_model_group5, newdata = Data_Cod[rand.vec ==5], type = "response")
detach(Data_Cod)

# Run the k-fold CV evaluation sensu Boyce et al. 2002
dataset <- Data_Cod[complete.cases(Data_Cod[,"scores"]),]
dataset<-dataset[!is.na(crab_pres)]

cod_crab_model_deviance <- numeric(5)

dataset$rand.vec<-as.factor(dataset$rand.vec)

attach(dataset)
cod_crab_model_deviance[1]<-get_deviance(gam_model_group1,dataset$scores[rand.vec=="1"],dataset$crab_pres[rand.vec=="1"],weights=NULL)
cod_crab_model_deviance[2]<-get_deviance(gam_model_group2,dataset$scores[rand.vec=="2"],dataset$crab_pres[rand.vec=="2"],weights=NULL)
cod_crab_model_deviance[3]<-get_deviance(gam_model_group3,dataset$scores[rand.vec=="3"],dataset$crab_pres[rand.vec=="3"],weights=NULL)
cod_crab_model_deviance[4]<-get_deviance(gam_model_group4,dataset$scores[rand.vec=="4"],dataset$crab_pres[rand.vec=="4"],weights=NULL)
cod_crab_model_deviance[5]<-get_deviance(gam_model_group5,dataset$scores[rand.vec=="5"],dataset$crab_pres[rand.vec=="5"],weights=NULL)
detach(dataset)

cod_crab_model_deviance<-as.data.table(cod_crab_model_deviance)

rm(list=setdiff(ls(), c("Diet_Data","cod_shrimp_model_deviance","turbot_shrimp_model_deviance","redfish_shrimp_model_deviance","cod_crab_model_deviance","get_deviance")))
