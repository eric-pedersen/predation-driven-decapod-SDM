### K-Fold Cross validation for Abundance SDMs ----
# Purpose: Validated Lat/Long SDMs using method by #CITE PAPER
# Author: S. Zabihi-Seissan

#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
### Packages ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
library(data.table)
library(mgcv)
library(sp)
library(rgdal)


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
Data$chionoecetes_opilio_smal<-(Data$chionoecetes_opilio_small/Data$tow_area)
Data$gadus_morhua<-(Data$gadus_morhua/Data$tow_area)
Data$reinhardtius_hippoglossoides<-(Data$reinhardtius_hippoglossoides/Data$tow_area)
Data$sebastes_mentella<-(Data$sebastes_mentella/Data$tow_area)
Data$planktivores<-(Data$planktivores/Data$tow_area)

# Fix Newfoundland region into fall and spring surveys
Data[region=="Newfoundland"&(month==9|month==10|month==11|month==12|month==1|month==2),region:="Newfoundland_fall"]
Data[region=="Newfoundland"&(month==3|month==4|month==5|month==6|month==7),region:="Newfoundland_spring"]

rm(list=setdiff(ls(), c("Data")))
Data<-as.data.table(Data)

# Load models (From 2.1 script)
models <- list.files("code/outputs/models/spatial", pattern="*.rda", full.names=TRUE)
lapply(models,load,.GlobalEnv)


#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
### Newfoundland Fall Models ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

### Northern Cod (Change species as needed)

# Subset data for Newfoundland fall only
Data_NL_Fall<-Data[region=="Newfoundland_fall"]

#Randomly bin data
set.seed(5)
Data_NL_Fall$rand.vec <- sample(1:5,nrow(Data_NL_Fall),replace=TRUE)

# Fit the model in all folds but one.
gam_model_group1 <- gam(gadus_morhua~te(longitude,latitude,year_surv,d= c(2,1), k = c(60,15)),family=tw(),data=Data_NL_Fall[rand.vec!=1],method="REML")
gam_model_group2 <- gam(gadus_morhua~te(longitude,latitude,year_surv,d= c(2,1), k = c(60,15)),family=tw(),data=Data_NL_Fall[rand.vec!=2],method="REML")
gam_model_group3 <- gam(gadus_morhua~te(longitude,latitude,year_surv,d= c(2,1), k = c(60,15)),family=tw(),data=Data_NL_Fall[rand.vec!=3],method="REML")
gam_model_group4 <- gam(gadus_morhua~te(longitude,latitude,year_surv,d= c(2,1), k = c(60,15)),family=tw(),data=Data_NL_Fall[rand.vec!=4],method="REML")
gam_model_group5 <- gam(gadus_morhua~te(longitude,latitude,year_surv,d= c(2,1), k = c(60,15)),family=tw(),data=Data_NL_Fall[rand.vec!=5],method="REML")

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

