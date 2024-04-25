### K-Fold Cross validation for Climate + Predator Index SDMs ----
# Purpose: Validated Climate SDMs using method by #CITE PAPER. Validation done by NAFO divisions.
# Author: S. Zabihi-Seissan

#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
### Packages ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

library(data.table)
library(mgcv)
library(sp)
library(rgdal)


#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
### Download data from GDrive ----
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
models_diet <- list.files("code/outputs/models/diet", pattern="*.rda", full.names=TRUE)
lapply(models_diet,load,.GlobalEnv)


# Replace year_surv from current to previous year (lag)
Data$year_current<-Data$year_surv
Data$year_surv<-(Data$year_current-1)
Data$year_surv<-as.integer(Data$year_surv)
Data$dummy<-1

## Predict values by region (Climate models)
# Keep years where diets were sampled
Data<-Data[(region=="Arctic"&year_surv==2018)|
             (region=="Gulf"&(year_surv==2004|year_surv==2005|year_surv==2006))|
             (region=="Maritimes"&(year_surv>2007&year_surv<2017))|
             (region=="Quebec"&((year_surv>1992&year_surv<2010)|(year_surv>2014&year_surv<2019)))|
             (region=="Newfoundland_fall"&((year_surv>1994&year_surv<2002)|(year_surv>2002&year_surv<2018)))|
             (region=="Newfoundland_spring"&((year_surv>1995&year_surv<1998)|(year_surv>2012&year_surv<2019)))]

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
Data_Arctic$pred_crab<-NA

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

# Keep years where diets were sampled
Data<-Data[(region=="Arctic"&year_surv==2018)|
             (region=="Gulf"&(year_surv==2004|year_surv==2005|year_surv==2006))|
             (region=="Maritimes"&(year_surv>2007&year_surv<2017))|
             (region=="Quebec"&((year_surv>1992&year_surv<2010)|(year_surv>2014&year_surv<2019)))|
             (region=="Newfoundland_fall"&((year_surv>1994&year_surv<2002)|(year_surv>2002&year_surv<2018)))|
             (region=="Newfoundland_spring"&((year_surv>1995&year_surv<1998)|(year_surv>2012&year_surv<2019)))]

# Predict diet data by species
Data$dummy<-1
Data$cod_shrimp<-predict(cod_shrimp_presence,newdata=Data,type="response")
Data$turbot_shrimp<-predict(turbot_shrimp_presence,newdata=Data,type="response")
Data$cod_crab<-predict(cod_crab_presence,newdata=Data,type="response")
Data_redfish<-Data[!(year_surv==1998|year_surv==2001|year_surv==2002|year_surv==2003|year_surv==2007|year_surv==2000)]
Data_redfish$redfish_shrimp<-predict(redfish_shrimp_presence,newdata=Data_redfish,type="response")
Data_redfish[,c(2:47)]<-NULL
Data<-merge(Data,Data_redfish, by ="ID",all.x=TRUE)

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
### K-fold Models ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

get_deviance <- function(model, y_pred, y_obs, weights = NULL){
  stopifnot(length(y_obs)==length(y_pred))
  if(is.null(weights)) weights = rep(1, times= length(y_obs))
  dev_residuals = model$family$dev.resids(y_obs, y_pred, weights)
  return(sum(dev_residuals))
}

### Northern shrimp ----

Data_Shrimp<-Data
Data_Shrimp<-Data_Shrimp[!(year_surv=="1970"|year_surv=="1971"|year_surv=="1972"|year_surv=="1973"|year_surv=="1974"
                           |year_surv=="1975"|year_surv=="1976"|year_surv=="1977"|year_surv=="1978"|year_surv=="1979"
                           |year_surv=="1980"|year_surv=="1981"|year_surv=="1982"|year_surv=="1983"|year_surv=="1984"
                           |year_surv=="1985"|year_surv=="1986"|year_surv=="1987"|year_surv=="1988"|year_surv=="1989")]
Data_Shrimp<-Data_Shrimp[!is.na(pred_index_shrimp)&!is.na(pandalus_borealis)]

# Get deviance explained
climate_shrimpmodel<-gam(pandalus_borealis~s(temperature_at_bottom,k=6)+
                           s(sqrt(depth),k=6) +
                           pred_index_shrimp+
                           s(region,year_surv, bs="re",by=dummy), #year-by-region random effects
                         family=tw(),
                         data=Data_Shrimp,
                         method="REML")

summary(climate_shrimpmodel)

#Randomly bin data
set.seed(5)
Data_Shrimp$rand.vec <- sample(1:5,nrow(Data_Shrimp),replace=TRUE)

# Fit the model in all folds but one.
gam_model_group1 <- gam(pandalus_borealis~s(temperature_at_bottom,k=6)+
                          s(sqrt(depth),k=6) +
                          pred_index_shrimp+
                          s(region,year_surv, bs="re",by=dummy),family=tw(),data=Data_Shrimp[rand.vec!=1],method="REML")
gam_model_group2 <- gam(pandalus_borealis~s(temperature_at_bottom,k=6)+
                          s(sqrt(depth),k=6) +
                          pred_index_shrimp+
                          s(region,year_surv, bs="re",by=dummy),family=tw(),data=Data_Shrimp[rand.vec!=2],method="REML")
gam_model_group3 <- gam(pandalus_borealis~s(temperature_at_bottom,k=6)+
                          s(sqrt(depth),k=6) +
                          pred_index_shrimp+
                          s(region,year_surv, bs="re",by=dummy),family=tw(),data=Data_Shrimp[rand.vec!=3],method="REML")
gam_model_group4 <- gam(pandalus_borealis~s(temperature_at_bottom,k=6)+
                          s(sqrt(depth),k=6) +
                          pred_index_shrimp+
                          s(region,year_surv, bs="re",by=dummy),family=tw(),data=Data_Shrimp[rand.vec!=4],method="REML")
gam_model_group5 <- gam(pandalus_borealis~s(temperature_at_bottom,k=6)+
                          s(sqrt(depth),k=6) +
                          pred_index_shrimp+
                          s(region,year_surv, bs="re",by=dummy),family=tw(),data=Data_Shrimp[rand.vec!=5],method="REML")

#Finds the score for the held-out subset of data.
attach(Data_Shrimp)
Data_Shrimp$scores[rand.vec ==1] = predict(gam_model_group1, newdata = Data_Shrimp[rand.vec ==1], type = "response")
Data_Shrimp$scores[rand.vec ==2] = predict(gam_model_group2, newdata = Data_Shrimp[rand.vec ==2], type = "response")
Data_Shrimp$scores[rand.vec ==3] = predict(gam_model_group3, newdata = Data_Shrimp[rand.vec ==3], type = "response")
Data_Shrimp$scores[rand.vec ==4] = predict(gam_model_group4, newdata = Data_Shrimp[rand.vec ==4], type = "response")
Data_Shrimp$scores[rand.vec ==5] = predict(gam_model_group5, newdata = Data_Shrimp[rand.vec ==5], type = "response")
detach(Data_Shrimp)

# Run the k-fold CV evaluation sensu Boyce et al. 2002
dataset <- Data_Shrimp[complete.cases(Data_Shrimp[,"scores"]),]
dataset<-dataset[!is.na(pandalus_borealis)]

shrimp_model_deviance <- numeric(5)

dataset$rand.vec<-as.factor(dataset$rand.vec)

attach(dataset)
shrimp_model_deviance[1]<-get_deviance(gam_model_group1,dataset$scores[rand.vec=="1"],dataset$pandalus_borealis[rand.vec=="1"],weights=NULL)
shrimp_model_deviance[2]<-get_deviance(gam_model_group2,dataset$scores[rand.vec=="2"],dataset$pandalus_borealis[rand.vec=="2"],weights=NULL)
shrimp_model_deviance[3]<-get_deviance(gam_model_group3,dataset$scores[rand.vec=="3"],dataset$pandalus_borealis[rand.vec=="3"],weights=NULL)
shrimp_model_deviance[4]<-get_deviance(gam_model_group4,dataset$scores[rand.vec=="4"],dataset$pandalus_borealis[rand.vec=="4"],weights=NULL)
shrimp_model_deviance[5]<-get_deviance(gam_model_group5,dataset$scores[rand.vec=="5"],dataset$pandalus_borealis[rand.vec=="5"],weights=NULL)
detach(dataset)

shrimp_model_deviance<-as.data.table(shrimp_model_deviance)

rm(list=setdiff(ls(), c("Data","shrimp_model_deviance","get_deviance")))


# Snow crab (small)----

Data_Crab<-Data
Data_Crab<-Data_Crab[!is.na(pred_index_crab)&!is.na(chionoecetes_opilio_small)]
Data_Crab<-Data_Crab[!(year_surv=="1970"|year_surv=="1971"|year_surv=="1972"|year_surv=="1973"|year_surv=="1974"
                       |year_surv=="1975"|year_surv=="1976"|year_surv=="1977"|year_surv=="1978"|year_surv=="1979"
                       |year_surv=="1980"|year_surv=="1981"|year_surv=="1982"|year_surv=="1983"|year_surv=="1984"
                       |year_surv=="1985"|year_surv=="1986"|year_surv=="1987"|year_surv=="1988"|year_surv=="1989")]

# Determine variance explained
climate_crabmodel<-gam(chionoecetes_opilio_small~s(temperature_at_bottom,k=6)+
                         s(sqrt(depth), region_large,bs="fs", k= 6) +
                         pred_index_crab+
                         s(region,year_surv, bs="re",by=dummy), #year-by-region random effects
                       family=tw(),
                       data=Data,subset=(region=="Maritimes"|region=="Quebec"|region=="Gulf"|region=="Newfoundland_fall"|region=="Newfoundland_spring"),
                       method="REML")
summary(climate_crabmodel)

#Randomly bin data
set.seed(5)
Data_Crab$rand.vec <- sample(1:5,nrow(Data_Crab),replace=TRUE)

# Fit the model in all folds but one.
gam_model_group1 <- gam(chionoecetes_opilio_small~s(temperature_at_bottom,k=6)+
                          s(sqrt(depth), region_large,bs="fs", k= 6) +
                          pred_index_crab+
                          s(region,year_surv, bs="re",by=dummy),family=tw(),data=Data_Crab[rand.vec!=1],method="REML")
gam_model_group2 <- gam(chionoecetes_opilio_small~s(temperature_at_bottom,k=6)+
                          s(sqrt(depth), region_large,bs="fs", k= 6) +
                          pred_index_crab+
                          s(region,year_surv, bs="re",by=dummy),family=tw(),data=Data_Crab[rand.vec!=2],method="REML")
gam_model_group3 <- gam(chionoecetes_opilio_small~s(temperature_at_bottom,k=6)+
                          s(sqrt(depth), region_large,bs="fs", k= 6) +
                          pred_index_crab+
                          s(region,year_surv, bs="re",by=dummy),family=tw(),data=Data_Crab[rand.vec!=3],method="REML")
gam_model_group4 <- gam(chionoecetes_opilio_small~s(temperature_at_bottom,k=6)+
                          s(sqrt(depth), region_large,bs="fs", k= 6) +
                          pred_index_crab+
                          s(region,year_surv, bs="re",by=dummy),family=tw(),data=Data_Crab[rand.vec!=4],method="REML")
gam_model_group5 <- gam(chionoecetes_opilio_small~s(temperature_at_bottom,k=6)+
                          s(sqrt(depth), region_large,bs="fs", k= 6) +
                          pred_index_crab+
                          s(region,year_surv, bs="re",by=dummy),family=tw(),data=Data_Crab[rand.vec!=5],method="REML")

Data_Crab<-Data_Crab[!region=="Arctic"]

#Finds the score for the held-out subset of data.
attach(Data_Crab)
Data_Crab$scores[rand.vec ==1] = predict(gam_model_group1, newdata = Data_Crab[rand.vec ==1], type = "response")
Data_Crab$scores[rand.vec ==2] = predict(gam_model_group2, newdata = Data_Crab[rand.vec ==2], type = "response")
Data_Crab$scores[rand.vec ==3] = predict(gam_model_group3, newdata = Data_Crab[rand.vec ==3], type = "response")
Data_Crab$scores[rand.vec ==4] = predict(gam_model_group4, newdata = Data_Crab[rand.vec ==4], type = "response")
Data_Crab$scores[rand.vec ==5] = predict(gam_model_group5, newdata = Data_Crab[rand.vec ==5], type = "response")
detach(Data_Crab)

# Run the k-fold CV evaluation sensu Boyce et al. 2002
dataset <- Data_Crab[complete.cases(Data_Crab[,"scores"]),]
dataset<-dataset[!is.na(chionoecetes_opilio_small)]

crab_model_deviance <- numeric(5)

dataset$rand.vec<-as.factor(dataset$rand.vec)

attach(dataset)
crab_model_deviance[1]<-get_deviance(gam_model_group1,dataset$scores[rand.vec=="1"],dataset$chionoecetes_opilio_small[rand.vec=="1"],weights=NULL)
crab_model_deviance[2]<-get_deviance(gam_model_group2,dataset$scores[rand.vec=="2"],dataset$chionoecetes_opilio_small[rand.vec=="2"],weights=NULL)
crab_model_deviance[3]<-get_deviance(gam_model_group3,dataset$scores[rand.vec=="3"],dataset$chionoecetes_opilio_small[rand.vec=="3"],weights=NULL)
crab_model_deviance[4]<-get_deviance(gam_model_group4,dataset$scores[rand.vec=="4"],dataset$chionoecetes_opilio_small[rand.vec=="4"],weights=NULL)
crab_model_deviance[5]<-get_deviance(gam_model_group5,dataset$scores[rand.vec=="5"],dataset$chionoecetes_opilio_small[rand.vec=="5"],weights=NULL)
detach(dataset)

crab_model_deviance<-as.data.table(crab_model_deviance)

rm(list=setdiff(ls(), c("Data","crab_model_deviance","get_deviance","shrimp_model_deviance")))
