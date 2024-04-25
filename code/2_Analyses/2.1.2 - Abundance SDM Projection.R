### SMOOTHED ABUNDANCE SDM Projections ----
# Purpose: Project Smoother abundance SDM models to use outputs in diet models
# Author: S. Zabihi-Seissan


                                                          ### Load packages ----
library(data.table)
library(mgcv)
library(rgdal)
library(googledrive)


                                                      ### Load models and data ----

# Load models (From 2.1 script)
models <- list.files("code/outputs/models/spatial", pattern="*.rda", full.names=TRUE)
lapply(models,load,.GlobalEnv)

# Load shapefiles
Maritimes_Area <- readOGR('data/shapefiles/Maritimes_area.shp')
Newfoundland_Area<-readOGR('data/shapefiles/Newfoundland_area.shp')
Gulf_Area<-readOGR('data/shapefiles/Gulf_area.shp')
Quebec_Area<-readOGR('data/shapefiles/Quebec_area.shp')
Arctic_Area<-readOGR('data/shapefiles/Arctic_area.shp')

# Modify coordinate system of shapefiles to match that of original data of models
CRS.new <- CRS("+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs") #determined using proj4string(NL)
Newfoundland_Area<-spTransform(Newfoundland_Area,CRS.new)
Maritimes_Area<-spTransform(Maritimes_Area,CRS.new)
Gulf_Area<-spTransform(Gulf_Area,CRS.new)
Quebec_Area<-spTransform(Quebec_Area,CRS.new)
Arctic_Area<-spTransform(Arctic_Area,CRS.new)


                                                          ### Data projection ----

### Maritimes

# Create new dataframe
NewData = expand.grid(longitude = seq(from = 330000, to = 1190000, by = 10000),
                      latitude = seq(from = 4670000, to = 5220000, by = 10000),
                      year_surv = seq(from = 1970, to = 2018, by = 1))

# Define coordinate system
coordinates(NewData)<-~longitude + latitude
proj4string(NewData) <- CRS("+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs")

# Clip new dataframe to Maritimes study area
NewData <- NewData[Maritimes_Area, ]
NewData<-as.data.frame(NewData)

# Project model outputs
NewData$pred_shrimp<-predict(mar_shrimpmodel,newdata=NewData,type="response")
NewData$pred_crab<-predict(mar_crabmodel,newdata=NewData,type="response")
NewData$pred_smallcrab<-predict(mar_crabsmallmodel,newdata=NewData,type="response")
NewData$pred_cod<-predict(mar_codmodel,newdata=NewData,type="response")
NewData$pred_turbot<-predict(mar_turbotmodel,newdata=NewData,type="response")
NewData$pred_redfish<-predict(mar_redfishmodel,newdata=NewData,type="response")
NewData$pred_planktivore<-predict(mar_planktivoremodel,newdata=NewData,type="response")

# Remove years for shrimp and crab where they were not recorded
NewData<-as.data.table(NewData)
NewData[year_surv<1998,pred_shrimp:=NA]
NewData[year_surv<1981,pred_crab:=NA]
NewData[year_surv<1999,pred_smallcrab:=NA]

# Define as Maritimes data
Maritimes_NewData<-NewData
Maritimes_NewData$region<-"Maritimes"

### Newfoundland fall season

# Create new dataframe
NewData = as.data.table(expand.grid(longitude = seq(from = 740000, to = 2190000, by = 10000),
                                    latitude = seq(from = 4750000, to = 6760000, by = 10000),
                                    year_surv = seq(from = 1995, to = 2017, by = 1)))

# Define coordinate system
coordinates(NewData)<-~longitude + latitude
proj4string(NewData) <- CRS("+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs")

# Clip new dataframe to Maritimes study area
NewData <- NewData[Newfoundland_Area, ]
NewData<-as.data.frame(NewData)

# Project model outputs
NewData$pred_shrimp<-predict(nl_shrimpmodel_fall,newdata=NewData,type="response")
NewData$pred_crab<-predict(nl_crabmodel_fall,newdata=NewData,type="response")
NewData$pred_smallcrab<-predict(nl_crabsmallmodel_fall,newdata=NewData,type="response")
NewData$pred_cod<-predict(nl_codmodel_fall,newdata=NewData,type="response")
NewData$pred_turbot<-predict(nl_turbotmodel_fall,newdata=NewData,type="response")
NewData$pred_redfish<-predict(nl_redfishmodel_fall,newdata=NewData,type="response")
NewData$pred_planktivore<-predict(nl_planktivoremodel_fall,newdata=NewData,type="response")

# Define as Newfoundland data
Newfoundland_Fall_NewData<-NewData
Newfoundland_Fall_NewData$region<-"Newfoundland_fall"

### Newfoundland spring season

# Create new dataframe
NewData = as.data.table(expand.grid(longitude = seq(from = 990000, to = 1970000, by = 10000),
                                    latitude = seq(from = 4760000, to = 5490000, by = 10000),
                                    year_surv = seq(from = 1996, to = 2018, by = 1)))

# Define coordinate system
coordinates(NewData)<-~longitude + latitude
proj4string(NewData) <- CRS("+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs")

# Clip new dataframe to Maritimes study area
NewData <- NewData[Newfoundland_Area, ]
NewData<-as.data.frame(NewData)

# Project model outputs
NewData$pred_shrimp<-predict(nl_shrimpmodel_spring,newdata=NewData,type="response")
NewData$pred_crab<-predict(nl_crabmodel_spring,newdata=NewData,type="response")
NewData$pred_smallcrab<-predict(nl_crabsmallmodel_spring,newdata=NewData,type="response")
NewData$pred_cod<-predict(nl_codmodel_spring,newdata=NewData,type="response")
NewData$pred_turbot<-predict(nl_turbotmodel_spring,newdata=NewData,type="response")
NewData$pred_redfish<-predict(nl_redfishmodel_spring,newdata=NewData,type="response")
NewData$pred_planktivore<-predict(nl_planktivoremodel_spring,newdata=NewData,type="response")

# Define as Newfoundland data
Newfoundland_Spring_NewData<-NewData
Newfoundland_Spring_NewData$region<-"Newfoundland_spring"

### Gulf

# Create new dataframe
NewData = expand.grid(longitude = seq(from = 520000, to = 960000, by = 10000),
                      latitude = seq(from = 5060000, to = 5450000, by = 10000),
                      year_surv = seq(from = 1971, to = 2017, by = 1))

# Define coordinate system
coordinates(NewData)<-~longitude + latitude
proj4string(NewData) <- CRS("+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs")

# Clip new dataframe to Maritimes study area
NewData <- NewData[Gulf_Area, ]
NewData<-as.data.frame(NewData)

# Project model outputs
NewData$pred_shrimp<-predict(gul_shrimpmodel,newdata=NewData,type="response")
NewData$pred_crab<-predict(gul_crabmodel,newdata=NewData,type="response")
NewData$pred_smallcrab<-predict(gul_crabsmallmodel,newdata=NewData,type="response")
NewData$pred_cod<-predict(gul_codmodel,newdata=NewData,type="response")
NewData$pred_turbot<-predict(gul_turbotmodel,newdata=NewData,type="response")
NewData$pred_redfish<-predict(gul_redfishmodel,newdata=NewData,type="response")
NewData$pred_planktivore<-predict(gul_planktivoremodel,newdata=NewData,type="response")

# Remove years for shrimp and crab where they were not recorded
NewData<-as.data.table(NewData)
NewData[year_surv<2003|year_surv>2015,pred_shrimp:=NA]
NewData[year_surv<2001&(!year_surv==2003),pred_crab:=NA]
NewData[year_surv<2001&(!year_surv==2003),pred_smallcrab:=NA]

# Define as Gulf data
Gulf_NewData<-NewData
Gulf_NewData$region<-"Gulf"

### Quebec

# Create new dataframe
NewData = as.data.table(expand.grid(longitude = seq(from = 270000, to = 1290000, by = 10000),
                                    latitude = seq(from = 5180000, to = 5790000, by = 10000),
                                    year_surv = seq(from = 1990, to = 2018, by = 1)))

# Define coordinate system
coordinates(NewData)<-~longitude + latitude
proj4string(NewData) <- CRS("+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs")

# Clip new dataframe to Maritimes study area
NewData <- NewData[Quebec_Area, ]
NewData<-as.data.frame(NewData)

# Project model outputs
NewData$pred_shrimp<-predict(qc_shrimpmodel,newdata=NewData,type="response")
NewData$pred_crab<-predict(qc_crabmodel,newdata=NewData,type="response")
NewData$pred_smallcrab<-predict(qc_crabsmallmodel,newdata=NewData,type="response")
NewData$pred_cod<-predict(qc_codmodel,newdata=NewData,type="response")
NewData$pred_turbot<-predict(qc_turbotmodel,newdata=NewData,type="response")
NewData$pred_redfish<-predict(qc_redfishmodel,newdata=NewData,type="response")
NewData$pred_planktivore<-predict(qc_planktivoremodel,newdata=NewData,type="response")

# Define as Quebec data
Quebec_NewData<-NewData
Quebec_NewData$region<-"Quebec"

### Arctic

# Create new dataframe
NewData = expand.grid(longitude = seq(from = 450000, to = 1080000, by = 10000),
                      latitude = seq(from = 6610000, to = 7340000, by = 10000),
                      year_surv = 2018)

# Define coordinate system
coordinates(NewData)<-~longitude + latitude
proj4string(NewData) <- CRS("+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs")

# Clip new dataframe to Maritimes study area
NewData <- NewData[Arctic_Area, ]
NewData<-as.data.frame(NewData)

# Project model outputs
NewData$pred_shrimp<-predict(arc_shrimpmodel,newdata=NewData,type="response")
NewData$pred_cod<-predict(arc_codmodel,newdata=NewData,type="response")
NewData$pred_turbot<-predict(arc_turbotmodel,newdata=NewData,type="response")
NewData$pred_redfish<-predict(arc_redfishmodel,newdata=NewData,type="response")
NewData$pred_planktivore<-predict(arc_planktivoremodel,newdata=NewData,type="response")

# Define as Arctic data
Arctic_NewData<-NewData
Arctic_NewData$region<-"Arctic"

rm(list=setdiff(ls(), c("Maritimes_NewData","Newfoundland_Fall_NewData","Newfoundland_Spring_NewData","Gulf_NewData","Quebec_NewData","Arctic_NewData")))


                                                ### Format projected data ----

# Merge Predicted data
Predicted_Data<-as.data.table(rbind(Newfoundland_Fall_NewData,Newfoundland_Spring_NewData,
                                    Gulf_NewData,Quebec_NewData,Maritimes_NewData,Arctic_NewData,fill=TRUE))


                                            ### Save projected data ----

fwrite(Predicted_Data, "code/outputs/data/Projected_Spatial_Allregions.csv")

