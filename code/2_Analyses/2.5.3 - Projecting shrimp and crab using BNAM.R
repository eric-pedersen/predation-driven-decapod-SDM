### Projecting shrimp and crab models using BNAM ----
# Purpose: Show shrimp and crab distributions based on BNAM outputs
# Author: S. Zabihi-Seissan

# Load packages
library(data.table)
library(mgcv)
library(ggplot2)
library(viridis)
library(grid)
library(rgdal)
library(rnaturalearth)
library(rnaturalearthdata)
library(magrittr)
library(sf)


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
Data<-melt(BNAM_Data, id.vars = c("x","y","depth","region"), measure.vars = c("BNAM_1995_mean","BNAM_1996_mean","BNAM_2005_mean","BNAM_2006_mean","BNAM_2015_mean","BNAM_2016_mean","BNAM_2075_mean"))

# Rename groups and columns to match models
colnames(Data)[colnames(Data)=="value"] <- "temperature_at_bottom"
Data[variable=="BNAM_1995_mean",year_surv:="1995"]
Data[variable=="BNAM_1996_mean",year_surv:="1996"]
Data[variable=="BNAM_2005_mean",year_surv:="2005"]
Data[variable=="BNAM_2006_mean",year_surv:="2006"]
Data[variable=="BNAM_2015_mean",year_surv:="2015"]
Data[variable=="BNAM_2016_mean",year_surv:="2016"]
Data[variable=="BNAM_2075_mean",year_surv:="2075"]
Data$variable<-NULL
Data$year_surv<-as.factor(Data$year_surv)
Data<-Data[year_surv=="1995"|year_surv=="1996"|year_surv=="2005"|year_surv=="2006"|year_surv=="2015"|year_surv=="2016"]
Data$dummy<-1
Data<-Data[depth<0]
Data$depth<-(-(Data$depth))

# Add topographical large regions
Data[region=="Newfoundland_fall"|region=="Newfoundland_spring",region_large:="NL_shelf"]
Data[region=="Gulf"|region=="Quebec",region_large:="SL_shelf"]
Data[region=="Maritimes",region_large:="SC_shelf"]
Data[region=="Arctic",region_large:="ART_shelf"]



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
#### Projecting climate+predation index models ----
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

# Calculate difference between models
Data$shrimp_clim_climpred<-(Data$shrimp_climatepred-Data$shrimp_climate)
Data$shrimp_clim_climind<-(Data$shrimp_climateind-Data$shrimp_climate)
Data$crab_clim_climpred<-(Data$crab_climatepred-Data$crab_climate)
Data$crab_clim_climind<-(Data$crab_climateind-Data$crab_climate)


#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
#### Plotting projections ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

#Format data
#Data_Shrimp_Plot<-melt(Data, id.vars = c("x","y","depth","temperature_at_bottom","year_surv","region","dummy"), measure.vars = c("shrimp_climate","shrimp_climatepred","shrimp_climateind"))
Data_Shrimp_Plot<-melt(Data, id.vars = c("x","y","depth","temperature_at_bottom","year_surv","region","dummy"), measure.vars = c("shrimp_clim_climpred","shrimp_clim_climind"))
#Data_Crab_Plot<-melt(Data, id.vars = c("x","y","depth","temperature_at_bottom","year_surv","region","dummy"), measure.vars = c("crab_climate","crab_climatepred","crab_climateind"))
Data_Crab_Plot<-melt(Data, id.vars = c("x","y","depth","temperature_at_bottom","year_surv","region","region_large","dummy"), measure.vars = c("crab_clim_climpred","crab_clim_climind"))
Data_Shrimp_Plot<-Data_Shrimp_Plot[year_surv=="1996"|year_surv=="2006"|year_surv=="2016"]
Data_Crab_Plot<-Data_Crab_Plot[year_surv=="1996"|year_surv=="2006"|year_surv=="2016"]

Data_Shrimp_Plot[variable=="shrimp_clim_climpred",variable:='P-SDM - C-SDM']
Data_Shrimp_Plot[variable=="shrimp_clim_climind",variable:='PI-SDM - C-SDM']
#Data_Shrimp_Plot[variable=="shrimp_climateind",variable:='Climate + Predation index']
Data_Crab_Plot[variable=="crab_clim_climpred",variable:='P-SDM - C-SDM']
Data_Crab_Plot[variable=="crab_clim_climind",variable:='PI-SDM - C-SDM']
#Data_Crab_Plot[variable=="crab_climateind",variable:='Climate + Predation index']

# Import region shapefile and landmass
Regions<-readOGR('data/Survey_areas.shp')
Regions<-spTransform(Regions,CRS("+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs"))

CanProj <- "+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs"
Canada <- ne_states(country = "Canada",returnclass = "sf")%>%
  dplyr::select(latitude,longitude,geonunit,geometry)%>%
  st_union()%>% #group provinces + territories
  st_as_sf()%>%
  st_transform(CanProj)
US <- ne_states(country = "United states of america",returnclass = "sf")%>%
  dplyr::select(latitude,longitude,geonunit,geometry)%>%
  st_union()%>% #group provinces + territories
  st_as_sf()%>%
  st_transform(CanProj)
ggplot()+
  geom_sf(data=Canada)

Data_Shrimp_Plot[value<=-5000,value:=-5000]

##########
#Data_Shrimp_Plot<-Data_Shrimp_Plot[variable=="Climate + Predator density"]
Plot_Shrimp<-ggplot((subset(Data_Shrimp_Plot,region=="Arctic"|region=="Maritimes"|region=="Gulf"|region=="Newfoundland_fall"|region=="Quebec"|region=="Newfoundland_spring")))+
  geom_tile(aes(x,y,fill=value))+
  geom_polygon(data=Regions,aes(long,lat,group=group),fill=NA,color="black",size=0.05)+
  geom_sf(data=Canada,size=0.05)+
  geom_sf(data=US,size=0.05)+
  coord_sf(xlim = c(300000,2000000), ylim=c(4750000,7300000))+
  facet_wrap(variable~year_surv)+
  ylab("Latitude")+
  xlab("Longitude")+
  scale_fill_gradient2(low="blue",high="red",mid="white",midpoint=0,limits=c(-5000,5000),name=expression(paste("Difference in northern shrimp density", (kg*"/"*km^{2}))))+
  theme(axis.line = element_line(colour = "black",size=0.05),
        axis.ticks = element_line(size=0.05),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.background = element_rect(colour="black",fill="grey",size=0.05),
        panel.background = element_blank(),
        strip.text.x = element_text(margin = margin(0.1,0,0.1,0, "cm"),colour = 'white',size=3),
        legend.box.spacing = unit(0.02, 'cm'),
        legend.key.width = unit(0.3, 'cm'),
        legend.key.height = unit(0.05, 'cm'),
        legend.spacing.x = unit(0.3,"cm"),
        legend.position="bottom",
        text = element_text(size = 3))


g <- ggplot_gtable(ggplot_build(Plot_Shrimp))
strip_both <- which(grepl('strip-', g$layout$name))
fills <- c("#3D83C4","#3D83C4","#3D83C4","#004D41","#004D41","#004D41")
k <- 1

for (i in strip_both) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

ggsave("code/outputs/figures/FigureS5_Shrimp_projection_map.jpeg",plot=g,dpi=600, width = 1961, height = 1900, units = "px")


#######
Regions_crab<-fortify(Regions,region="region")
Regions_crab<-as.data.table(Regions_crab)
Regions_crab<-Regions_crab[!id=="Arctic"]
#Data_Crab_Plot<-Data_Crab_Plot[variable=="Climate + Predation index"]
Plot_Crab<-ggplot((subset(Data_Crab_Plot,region=="Maritimes"|region=="Gulf"|region=="Newfoundland_fall"|region=="Quebec"|region=="Newfoundland_spring")))+
  geom_tile(aes(x,y,fill=value))+
  geom_polygon(data=Regions_crab,aes(long,lat,group=group),fill=NA,color="black",size=0.05)+
  facet_wrap(variable~year_surv)+
  geom_sf(data=Canada,size=0.05)+
  geom_sf(data=US,size=0.05)+
  coord_sf(xlim = c(300000,2000000), ylim=c(4750000,6350000))+
  ylab("Latitude")+
  xlab("Longitude")+
  scale_fill_gradient2(low="blue",high="red",mid="white",midpoint=0,name=expression(paste("Difference in snow crab density", (kg*"/"*km^{2}))))+
  theme(axis.line = element_line(colour = "black",size=0.05),
        axis.ticks = element_line(size=0.05),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.background = element_rect(colour="black",fill="grey",size=0.05),
        panel.background = element_blank(),
        strip.text.x = element_text(margin = margin(0.1,0,0.1,0, "cm"),colour = 'white',size=3),
        legend.box.spacing = unit(0.02, 'cm'),
        legend.key.width = unit(0.3, 'cm'),
        legend.key.height = unit(0.05, 'cm'),
        legend.spacing.x = unit(0.3,"cm"),
        legend.position="bottom",
        text = element_text(size = 3))

g <- ggplot_gtable(ggplot_build(Plot_Crab))
strip_both <- which(grepl('strip-', g$layout$name))
fills <- c("#3D83C4","#3D83C4","#3D83C4","#004D41","#004D41","#004D41")
k <- 1

for (i in strip_both) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

ggsave("code/outputs/figures/FigureS6_Crab_projection_map.jpeg",plot=g,dpi=600, width = 1961, height = 1700, units = "px")
