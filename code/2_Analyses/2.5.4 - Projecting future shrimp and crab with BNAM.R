### Projecting shrimp and crab models using BNAM for 2075 ----
# Purpose: Show shrimp and crab distributions based on BNAM outputs for 2075
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
library(cowplot)
library(maptools)
library(devtools)
library(gpclib)


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

# Predict lagged predator density (Not actually lagged for 2075 predictions)
Data$pred_cod<-predict(climate_codmodel,newdata = Data,type="response")
Data$pred_turbot<-predict(climate_turbotmodel,newdata = Data,type="response")
Data$pred_redfish<-predict(climate_redfishmodel,newdata = Data,type="response")

rm(list=setdiff(ls(), "Data"))

# Load climate+predator SDMs
models <- list.files("code/outputs/models/climate/Climate_Predator_Density", pattern="*.rda", full.names=TRUE)
lapply(models,load,.GlobalEnv)

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
Data$shrimp_climateind<-predict(climate_shrimpmodel,newdata = Data,type="response")
Data$crab_climateind<-predict(climate_crabmodel,newdata = Data,type="response")

rm(list=setdiff(ls(), "Data"))

# Save for 2.5.5. script
fwrite(Data,file="code/outputs/data/predicted_data.csv")


#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
#### Plotting projections ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

#Format data
Data_Shrimp_Plot<-melt(Data, id.vars = c("x","y","depth","temperature_at_bottom","year_surv","region","dummy"), measure.vars = c("shrimp_climate","shrimp_climatepred","shrimp_climateind"))
Data_Crab_Plot<-melt(Data, id.vars = c("x","y","depth","temperature_at_bottom","year_surv","region","dummy"), measure.vars = c("crab_climate","crab_climatepred","crab_climateind"))
Data_Shrimp_Plot<-Data_Shrimp_Plot[year_surv=="2075"|year_surv=="0000"]
Data_Crab_Plot<-Data_Crab_Plot[year_surv=="2075"|year_surv=="0000"]


Data_Shrimp_Plot[variable=="shrimp_climate",variable:='C-SDM']
Data_Shrimp_Plot[variable=="shrimp_climatepred",variable:='P-SDM']
Data_Shrimp_Plot[variable=="shrimp_climateind",variable:='PI-SDM']
Data_Crab_Plot[variable=="crab_climate",variable:='C-SDM']
Data_Crab_Plot[variable=="crab_climatepred",variable:='P-SDM']
Data_Crab_Plot[variable=="crab_climateind",variable:='PI-SDM']

## Calculate anomaly for each model (2075 - mean)

# Split the two data sets
Data_Shrimp_2075<-Data_Shrimp_Plot[year_surv=="2075"]
Data_Shrimp_2075$value_2075<-Data_Shrimp_2075$value
Data_Shrimp_2075$value<-NULL
Data_Shrimp_2075$year_surv<-NULL
Data_Shrimp_0000<-Data_Shrimp_Plot[year_surv=="0000"]
Data_Shrimp_0000$value_0000<-Data_Shrimp_0000$value
Data_Shrimp_0000$value<-NULL
Data_Shrimp_0000$year_surv<-NULL
Data_Shrimp<-merge(Data_Shrimp_2075,Data_Shrimp_0000,by =c("x","y","depth",
                                                           "region","dummy","variable"),all=TRUE)
Data_Shrimp$value<-(Data_Shrimp$value_2075)-(Data_Shrimp$value_0000)
Data_Shrimp_Plot<-Data_Shrimp

Data_Crab_2075<-Data_Crab_Plot[year_surv=="2075"]
Data_Crab_2075$value_2075<-Data_Crab_2075$value
Data_Crab_2075$value<-NULL
Data_Crab_2075$year_surv<-NULL
Data_Crab_0000<-Data_Crab_Plot[year_surv=="0000"]
Data_Crab_0000$value_0000<-Data_Crab_0000$value
Data_Crab_0000$value<-NULL
Data_Crab_0000$year_surv<-NULL
Data_Crab<-merge(Data_Crab_2075,Data_Crab_0000,by =c("x","y","depth",
                                                           "region","dummy","variable"),all=TRUE)
Data_Crab$value<-(Data_Crab$value_2075)-(Data_Crab$value_0000)
Data_Crab_Plot<-Data_Crab

# Import region shapefile
Regions<-readOGR('data/shapefiles/Survey_areas.shp')
Regions<-spTransform(Regions,CRS("+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs"))

CanProj <- "+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs"
Canada <- ne_states(country = "Canada",returnclass = "sf")%>%
  dplyr::select(latitude,longitude,geonunit,geometry)%>%
  st_union()%>% #group provinces + territories
  st_as_sf()%>%
  st_transform(CanProj)
ggplot()+
  geom_sf(data=Canada)
US <- ne_states(country = "United states of america",returnclass = "sf")%>%
  dplyr::select(latitude,longitude,geonunit,geometry)%>%
  st_union()%>% #group provinces + territories
  st_as_sf()%>%
  st_transform(CanProj)


##########
Plot_Shrimp<-ggplot((subset(Data_Shrimp_Plot,region=="Arctic"|region=="Maritimes"|region=="Gulf"|region=="Newfoundland_fall"|region=="Quebec"|region=="Newfoundland_spring")))+
  geom_tile(aes(x,y,fill=value))+
  geom_polygon(data=Regions,aes(long,lat,group=group),fill=NA,color="black",size=0.05)+
  geom_sf(data=Canada,size=0.05)+
  geom_sf(data=US,size=0.05)+
  coord_sf(xlim = c(300000,2000000), ylim=c(4750000,7300000))+
  facet_wrap(~variable)+
  #coord_equal()+
  ylab("Latitude")+
  xlab("Longitude")+
  scale_fill_gradient2(limits=c(-1881.949, 3611.266),low="blue",high="red",mid="white",midpoint=0,name=expression(paste("Change in northern shrimp density from 2075 relative to baseline", (kg*"/"*km^{2}))))+
  theme(axis.line = element_line(colour = "black",size=0.05),
        axis.ticks = element_line(size=0.05),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        strip.background = element_rect(
          color="black", fill="white", size=0.05, linetype="solid"),
        legend.position="bottom",
        legend.box.spacing = unit(0.02, 'cm'),
        legend.key.width = unit(0.3, 'cm'),
        legend.key.height = unit(0.1, 'cm'),
        legend.title.align = 0.5,
        strip.text = element_text(colour = 'white',size=5),
        strip.text.x = element_text(margin = margin(0.1,0,0.1,0, "cm")),
        legend.background = element_blank(),
        plot.margin= margin(0.05,0.05,0.05,0.05,unit="cm"),
        legend.box.margin=margin(-0.04,-0.04,-0.04,-0.04, "cm"),
        legend.spacing.x = unit(0.15, 'cm'),
        text = element_text(size = 3))
Plot_Shrimp

p <- ggplot_gtable(ggplot_build(Plot_Shrimp))
strip_both <- which(grepl('strip-', p$layout$name))
fills <- c("#D81A60","#004D41","#3D83C4")
k <- 1

for (i in strip_both) {
  j <- which(grepl('rect', p$grobs[[i]]$grobs[[1]]$childrenOrder))
  p$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(p)

#ggsave("data/figures/Shrimp_projection_map_2075.jpeg",plot=p,dpi=320, width = 14, height = 10, units = "in")


#######
Regions_crab<-fortify(Regions,region="region")
Regions_crab<-as.data.table(Regions_crab)
Regions_crab<-Regions_crab[!id=="Arctic"]
#Data_Crab_Plot<-Data_Crab_Plot[variable=="Climate + Predation index"]
Plot_Crab<-ggplot((subset(Data_Crab_Plot,region=="Maritimes"|region=="Gulf"|region=="Newfoundland_fall"|region=="Quebec"|region=="Newfoundland_spring")))+
  geom_tile(aes(x,y,fill=value))+
  geom_polygon(data=Regions_crab,aes(long,lat,group=group),fill=NA,color="black",size=0.05)+
  facet_wrap(~variable)+
  geom_sf(data=Canada,size=0.05)+
  geom_sf(data=US,size=0.05)+
  coord_sf(xlim = c(300000,2000000), ylim=c(4750000,6350000))+
 # coord_equal()+
  ylab("Latitude")+
  xlab("Longitude")+
  scale_fill_gradient2(limits=c(-5.508195, 0.2079473),low="blue",high="red",mid="white",midpoint=0,name=expression(paste("Change in snow crab density from 2075 relative to baseline", (kg*"/"*km^{2}))))+
  theme(axis.line = element_line(colour = "black",size=0.05),
        axis.ticks = element_line(size=0.05),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        strip.background = element_rect(
          color="black", fill="white", size=0.05, linetype="solid"),
        legend.position="bottom",
        legend.box.spacing = unit(0.02, 'cm'),
        legend.key.width = unit(0.3, 'cm'),
        legend.key.height = unit(0.1, 'cm'),
        legend.title.align = 0.5,
        legend.background = element_blank(),
        strip.text.x = element_text(margin = margin(0.1,0,0.1,0, "cm")),
        strip.text = element_text(colour = 'white',size=5),
        plot.margin= margin(0.05,0.05,0.05,0.05,unit="cm"),
        legend.box.margin=margin(-0.04,-0.04,-0.04,-0.04, "cm"),
        legend.spacing.x = unit(0.15, 'cm'),
        text = element_text(size = 3))
Plot_Crab

g <- ggplot_gtable(ggplot_build(Plot_Crab))
strip_both <- which(grepl('strip-', g$layout$name))
fills <- c("#D81A60","#004D41","#3D83C4")
k <- 1

for (i in strip_both) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

#ggsave("data/figures/Crab_projection_map_2075.jpeg",plot=g,dpi=320, width = 13, height = 7, units = "in")

# Combine both plots into 1

final_plot<-ggarrange(p,g,labels=c("(a)","(b)"),ncol=1,nrow=2,widths=c(1,1),heights=c(1.37,1),label.x=0.01,label.y=0.95,font.label = list(size = 3))
final_plot
ggsave("code/outputs/figures/Figure7_projection_map_2075.jpeg",plot=final_plot,dpi=600 , width = 1961, height = 2100, units = "px")


rm(list=setdiff(ls(), "Data"))
2111.85
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
#### Plotting predator and planktivore densities ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

#Format data
Data_Predator_Plot<-melt(Data, id.vars = c("x","y","depth","temperature_at_bottom","year_surv","region","dummy"), measure.vars = c("pred_cod","pred_turbot","pred_redfish","pred_planktivores"))
Data_Predator_Plot<-Data_Predator_Plot[year_surv=="2075"|year_surv=="0000"]

## Calculate anomaly for each model (2075 - mean)

# Split the two data sets
Data_Predator_2075<-Data_Predator_Plot[year_surv=="2075"]
Data_Predator_2075$value_2075<-Data_Predator_2075$value
Data_Predator_2075$value<-NULL
Data_Predator_2075$year_surv<-NULL
Data_Predator_0000<-Data_Predator_Plot[year_surv=="0000"]
Data_Predator_0000$value_0000<-Data_Predator_0000$value
Data_Predator_0000$value<-NULL
Data_Predator_0000$year_surv<-NULL
Data_Predator<-merge(Data_Predator_2075,Data_Predator_0000,by =c("x","y","depth",
                                                           "region","dummy","variable"),all=TRUE)
Data_Predator$value<-(Data_Predator$value_2075)-(Data_Predator$value_0000)
Data_Predator_Plot<-Data_Predator

Data_Predator_Plot[variable=="pred_cod",variable:='Atlantic cod']
Data_Predator_Plot[variable=="pred_turbot",variable:='Greenland halibut']
Data_Predator_Plot[variable=="pred_redfish",variable:='Redfish']
Data_Predator_Plot[variable=="pred_planktivores",variable:='Planktivores']

# Import region shapefile
Regions<-readOGR('data/shapefiles/Survey_areas.shp')
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

# Plot

Plot_Predator<-ggplot((subset(Data_Predator_Plot,region=="Arctic"|region=="Maritimes"|region=="Gulf"|region=="Newfoundland_fall"|region=="Quebec"|region=="Newfoundland_spring")))+
  geom_tile(aes(x,y,fill=value))+
  geom_polygon(data=Regions,aes(long,lat,group=group),fill=NA,color="black",size=0.05)+
  geom_sf(data=Canada,size=0.05)+
  geom_sf(data=US,size=0.05)+
  coord_sf(xlim = c(300000,2000000), ylim=c(4750000,7300000))+
  facet_wrap(~variable)+
  #coord_equal()+
  ylab("Latitude")+
  xlab("Longitude")+
  scale_fill_gradient2(low="blue",high="red",mid="white",midpoint=0,name=expression(paste("Change in species density", (kg*"/"*km^{2}))))+
  theme(axis.line = element_line(colour = "black",size=0.05),
        axis.ticks = element_line(size=0.05),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        strip.background = element_rect(
          color="black", fill="white", size=0.05, linetype="solid"),
        legend.position="bottom",
        legend.box.spacing = unit(0.02, 'cm'),
        legend.key.width = unit(0.3, 'cm'),
        legend.key.height = unit(0.1, 'cm'),
        legend.title.align = 0.5,
        strip.text.x = element_text(margin = margin(0.1,0,0.1,0, "cm")),
        strip.text = element_text(colour = 'black',size=4),
        legend.background = element_blank(),
        plot.margin= margin(0.05,0.05,0.05,0.05,unit="cm"),
        legend.box.margin=margin(-0.04,-0.04,-0.04,-0.04, "cm"),
        text = element_text(size = 3))
Plot_Predator

ggsave("code/outputs/figures/FigureS7_Predator_projection_map_2075.jpeg",plot=Plot_Predator,dpi=600, width = 1961, height = 1961, units = "px")

#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
#### Plotting diet probabilities ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

#Format data
Data_Diet_Plot<-melt(Data, id.vars = c("x","y","depth","temperature_at_bottom","year_surv","region","dummy"), measure.vars = c("cod_shrimp","turbot_shrimp","redfish_shrimp","cod_crab"))
Data_Diet_Plot<-Data_Diet_Plot[year_surv=="2075"|year_surv=="0000"]

## Calculate anomaly for each model (2075 - mean)

# Split the two data sets
Data_Diet_2075<-Data_Diet_Plot[year_surv=="2075"]
Data_Diet_2075$value_2075<-Data_Diet_2075$value
Data_Diet_2075$value<-NULL
Data_Diet_2075$year_surv<-NULL
Data_Diet_0000<-Data_Diet_Plot[year_surv=="0000"]
Data_Diet_0000$value_0000<-Data_Diet_0000$value
Data_Diet_0000$value<-NULL
Data_Diet_0000$year_surv<-NULL
Data_Diet<-merge(Data_Diet_2075,Data_Diet_0000,by =c("x","y","depth",
                                                                 "region","dummy","variable"),all=TRUE)
Data_Diet$value<-(Data_Diet$value_2075)-(Data_Diet$value_0000)
Data_Diet_Plot<-Data_Diet

Data_Predator_Plot[variable=="pred_cod",variable:='Atlantic cod']
Data_Predator_Plot[variable=="pred_turbot",variable:='Greenland halibut']
Data_Predator_Plot[variable=="pred_redfish",variable:='Redfish']

# Import region shapefile
Regions<-readOGR('data/shapefiles/Survey_areas.shp')
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


# Plot
# Baseline
Plot_Diet_baseline_codshrimp<-ggplot((subset(Data_Diet_Plot,(region=="Arctic"|region=="Maritimes"|region=="Gulf"|region=="Newfoundland_fall"|region=="Quebec"|region=="Newfoundland_spring")&variable=="cod_shrimp")))+
  geom_tile(aes(x,y,fill=value_0000))+
  geom_polygon(data=Regions,aes(long,lat,group=group),fill=NA,color="black",size=0.05)+
  geom_sf(data=Canada,size=0.05)+
  geom_sf(data=US,size=0.05)+
  coord_sf(xlim = c(300000,2000000), ylim=c(4750000,7300000))+
  #coord_equal()+
  ylab("Latitude")+
  xlab("Longitude")+
  scale_fill_gradient(low="white",high="red",name=expression("Probability of prey\nin stomach (%)"))+
  theme(axis.line = element_line(colour = "black",size=0.05),
        axis.ticks = element_line(size=0.05),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        strip.background = element_rect(
          color="black", fill="white", size=0.05, linetype="solid"),
        #legend.position="none",
        legend.box.spacing = unit(0.02, 'cm'),
        legend.key.width = unit(0.08, 'cm'),
        legend.key.height = unit(0.15, "cm"),
        text = element_text(size = 3),
        plot.margin= margin(0.05,0.05,0.05,0.05,unit="cm"),
        legend.title=element_text(size=3))
Plot_Diet_baseline_codshrimp

Plot_Diet_baseline_turbotshrimp<-ggplot((subset(Data_Diet_Plot,(region=="Arctic"|region=="Maritimes"|region=="Gulf"|region=="Newfoundland_fall"|region=="Quebec"|region=="Newfoundland_spring")&variable=="turbot_shrimp")))+
  geom_tile(aes(x,y,fill=value_0000))+
  geom_polygon(data=Regions,aes(long,lat,group=group),fill=NA,color="black",size=0.05)+
  geom_sf(data=Canada,size=0.05)+
  geom_sf(data=US,size=0.05)+
  coord_sf(xlim = c(300000,2000000), ylim=c(4750000,7300000))+
  #coord_equal()+
  ylab("Latitude")+
  xlab("Longitude")+
  scale_fill_gradient(low="white",high="red",name=expression("Probability of prey\nin stomach (%)"))+
  theme(axis.line = element_line(colour = "black",size=0.05),
        axis.ticks = element_line(size=0.05),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        strip.background = element_rect(
          color="black", fill="white", size=1, linetype="solid"),
        #legend.position="none",
        legend.box.spacing = unit(0.02, 'cm'),
        legend.key.width = unit(0.08, 'cm'),
        legend.key.height = unit(0.15, "cm"),
        text = element_text(size = 3),
        plot.margin= margin(0.05,0.05,0.05,0.05,unit="cm"),
        legend.title=element_text(size=3))
Plot_Diet_baseline_turbotshrimp

Plot_Diet_baseline_redfishshrimp<-ggplot((subset(Data_Diet_Plot,(region=="Arctic"|region=="Maritimes"|region=="Gulf"|region=="Newfoundland_fall"|region=="Quebec"|region=="Newfoundland_spring")&variable=="redfish_shrimp")))+
  geom_tile(aes(x,y,fill=value_0000))+
  geom_polygon(data=Regions,aes(long,lat,group=group),fill=NA,color="black",size=0.05)+
  geom_sf(data=Canada,size=0.05)+
  geom_sf(data=US,size=0.05)+
  coord_sf(xlim = c(300000,2000000), ylim=c(4750000,7300000))+
  #coord_equal()+
  ylab("Latitude")+
  xlab("Longitude")+
  scale_fill_gradient(low="white",high="red",name=expression("Probability of prey\nin stomach (%)"))+
  theme(axis.line = element_line(colour = "black",size=0.05),
        axis.ticks = element_line(size=0.05),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        strip.background = element_rect(
          color="black", fill="white", size=1, linetype="solid"),
        #legend.position="bottom",
        legend.box.spacing = unit(0.02, 'cm'),
        legend.key.width = unit(0.08, 'cm'),
        legend.key.height = unit(0.15, "cm"),
        text = element_text(size = 3),
        plot.margin= margin(0.05,0.05,0.05,0.05,unit="cm"),
        legend.title=element_text(size=3))
Plot_Diet_baseline_redfishshrimp

if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit()
Regions_crab<-fortify(Regions,region="region")
Regions_crab<-as.data.table(Regions_crab)
Regions_crab<-Regions_crab[!id=="Arctic"]

Plot_Diet_baseline_codcrab<-ggplot((subset(Data_Diet_Plot,(region=="Maritimes"|region=="Gulf"|region=="Newfoundland_fall"|region=="Quebec"|region=="Newfoundland_spring")&variable=="cod_crab")))+
  geom_tile(aes(x,y,fill=value_0000))+
  geom_polygon(data=Regions_crab,aes(long,lat,group=group),fill=NA,color="black",size=0.05)+
  geom_sf(data=Canada,size=0.05)+
  geom_sf(data=US,size=0.05)+
  coord_sf(xlim = c(300000,2000000), ylim=c(4750000,7300000))+
  #coord_equal()+
  ylab("Latitude")+
  xlab("Longitude")+
  scale_fill_gradient(low="white",high="red",name=expression("Probability of prey\nin stomach (%)"))+
  theme(axis.line = element_line(colour = "black",size=0.05),
        axis.ticks = element_line(size=0.05),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        strip.background = element_rect(
          color="black", fill="white", size=1, linetype="solid"),
        #legend.position="bottom",
        legend.box.spacing = unit(0.02, 'cm'),
        legend.key.width = unit(0.08, 'cm'),
        legend.key.height = unit(0.15, "cm"),
        text = element_text(size = 3),
        plot.margin= margin(0.05,0.05,0.05,0.05,unit="cm"),
        legend.title=element_text(size=3))
Plot_Diet_baseline_codcrab

Plot_Diet_baseline<-ggarrange(Plot_Diet_baseline_codshrimp,Plot_Diet_baseline_turbotshrimp,Plot_Diet_baseline_redfishshrimp,Plot_Diet_baseline_codcrab,ncol=2,nrow=2,common.legend = FALSE,
                              labels=c("(a)","(b)","(c)","(d)"),label.x=0,label.y=0.98,font.label = list(size = 3))
Plot_Diet_baseline
#Plot_Diet_baseline<-ggplot((subset(Data_Diet_Plot,region=="Maritimes"|region=="Gulf"|region=="Newfoundland_fall"|region=="Quebec"|region=="Newfoundland_spring")))+
 # geom_tile(aes(x,y,fill=value_0000))+
  #geom_polygon(data=Regions,aes(long,lat,group=group),fill=NA,color="black",size=0.5)+
  #geom_sf(data=Canada)+
  #geom_sf(data=US)+
  #coord_sf(xlim = c(300000,2000000), ylim=c(4750000,7300000))+
  #facet_wrap(~variable)+
  #coord_equal()+
  #ylab("Latitude")+
  #xlab("Longitude")+
  #scale_fill_gradient(low="white",high="red",name=expression("Probability of prey in stomach (%)"))+
  #theme(axis.line = element_line(colour = "black"),
  #      panel.grid.major = element_blank(),
   #     panel.grid.minor = element_blank(),
    #    panel.border = element_blank(),
     #   panel.background = element_blank(),
      #  axis.text.x=element_blank(),
       # axis.text.y=element_blank(),
        #strip.background = element_rect(
         # color="black", fill="white", size=1, linetype="solid"),
        #legend.position="bottom",
        #legend.key.width = unit(2, 'cm'),
        #text = element_text(size = 15))
#Plot_Diet_baseline

ggsave("code/outputs/figures/FigureS8_Diet_projection_map_baseline.jpeg",plot=Plot_Diet_baseline,dpi=600, width = 1961, height = 1961, units = "px")

# 2075 map
Plot_Diet_2075_codshrimp<-ggplot((subset(Data_Diet_Plot,(region=="Arctic"|region=="Maritimes"|region=="Gulf"|region=="Newfoundland_fall"|region=="Quebec"|region=="Newfoundland_spring")&variable=="cod_shrimp")))+
  geom_tile(aes(x,y,fill=value_2075))+
  geom_polygon(data=Regions,aes(long,lat,group=group),fill=NA,color="black",size=0.05)+
  geom_sf(data=Canada,size=0.05)+
  geom_sf(data=US,size=0.05)+
  coord_sf(xlim = c(300000,2000000), ylim=c(4750000,7300000))+
  #coord_equal()+
  ylab("Latitude")+
  xlab("Longitude")+
  scale_fill_gradient(low="white",high="red",name=expression("Probability of prey\nin stomach (%)"))+
  theme(axis.line = element_line(colour = "black",size=0.05),
        axis.ticks = element_line(size=0.05),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        strip.background = element_rect(
          color="black", fill="white", size=1, linetype="solid"),
        #legend.position="bottom",
        legend.box.spacing = unit(0.02, 'cm'),
        legend.key.width = unit(0.08, 'cm'),
        legend.key.height = unit(0.15, "cm"),
        text = element_text(size = 3),
        plot.margin= margin(0.05,0.05,0.05,0.05,unit="cm"),
        legend.title=element_text(size=3))
Plot_Diet_2075_codshrimp

Plot_Diet_2075_turbotshrimp<-ggplot((subset(Data_Diet_Plot,(region=="Arctic"|region=="Maritimes"|region=="Gulf"|region=="Newfoundland_fall"|region=="Quebec"|region=="Newfoundland_spring")&variable=="turbot_shrimp")))+
  geom_tile(aes(x,y,fill=value_2075))+
  geom_polygon(data=Regions,aes(long,lat,group=group),fill=NA,color="black",size=0.05)+
  geom_sf(data=Canada,size=0.05)+
  geom_sf(data=US,size=0.05)+
  coord_sf(xlim = c(300000,2000000), ylim=c(4750000,7300000))+
  #coord_equal()+
  ylab("Latitude")+
  xlab("Longitude")+
  scale_fill_gradient(low="white",high="red",name=expression("Probability of prey\nin stomach (%)"))+
  theme(axis.line = element_line(colour = "black",size=0.05),
        axis.ticks = element_line(size=0.05),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        strip.background = element_rect(
          color="black", fill="white", size=1, linetype="solid"),
        #legend.position="bottom",
        legend.box.spacing = unit(0.02, 'cm'),
        legend.key.width = unit(0.08, 'cm'),
        legend.key.height = unit(0.15, "cm"),
        text = element_text(size = 3),
        plot.margin= margin(0.05,0.05,0.05,0.05,unit="cm"),
        legend.title=element_text(size=3))
Plot_Diet_2075_turbotshrimp

Plot_Diet_2075_redfishshrimp<-ggplot((subset(Data_Diet_Plot,(region=="Arctic"|region=="Maritimes"|region=="Gulf"|region=="Newfoundland_fall"|region=="Quebec"|region=="Newfoundland_spring")&variable=="redfish_shrimp")))+
  geom_tile(aes(x,y,fill=value_2075))+
  geom_polygon(data=Regions,aes(long,lat,group=group),fill=NA,color="black",size=0.05)+
  geom_sf(data=Canada,size=0.05)+
  geom_sf(data=US,size=0.05)+
  coord_sf(xlim = c(300000,2000000), ylim=c(4750000,7300000))+
  #coord_equal()+
  ylab("Latitude")+
  xlab("Longitude")+
  scale_fill_gradient(low="white",high="red",name=expression("Probability of prey\nin stomach (%)"))+
  theme(axis.line = element_line(colour = "black",size=0.05),
        axis.ticks = element_line(size=0.05),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        strip.background = element_rect(
          color="black", fill="white", size=1, linetype="solid"),
        #legend.position="bottom",
        legend.box.spacing = unit(0.02, 'cm'),
        legend.key.width = unit(0.08, 'cm'),
        legend.key.height = unit(0.15, "cm"),
        text = element_text(size = 3),
        plot.margin= margin(0.05,0.05,0.05,0.05,unit="cm"),
        legend.title=element_text(size=3))
Plot_Diet_2075_redfishshrimp

if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit()
Regions_crab<-fortify(Regions,region="region")
Regions_crab<-as.data.table(Regions_crab)
Regions_crab<-Regions_crab[!id=="Arctic"]

Plot_Diet_2075_codcrab<-ggplot((subset(Data_Diet_Plot,(region=="Maritimes"|region=="Gulf"|region=="Newfoundland_fall"|region=="Quebec"|region=="Newfoundland_spring")&variable=="cod_crab")))+
  geom_tile(aes(x,y,fill=value_2075))+
  geom_polygon(data=Regions_crab,aes(long,lat,group=group),fill=NA,color="black",size=0.05)+
  geom_sf(data=Canada,size=0.05)+
  geom_sf(data=US,size=0.05)+
  coord_sf(xlim = c(300000,2000000), ylim=c(4750000,7300000))+
  #coord_equal()+
  ylab("Latitude")+
  xlab("Longitude")+
  scale_fill_gradient(low="white",high="red",name=expression("Probability of prey\nin stomach (%)"))+
  theme(axis.line = element_line(colour = "black",size=0.05),
        axis.ticks = element_line(size=0.05),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        strip.background = element_rect(
          color="black", fill="white", size=1, linetype="solid"),
        #legend.position="bottom",
        legend.box.spacing = unit(0.02, 'cm'),
        legend.key.width = unit(0.08, 'cm'),
        legend.key.height = unit(0.15, "cm"),
        text = element_text(size = 3),
        plot.margin= margin(0.05,0.05,0.05,0.05,unit="cm"),
        legend.title=element_text(size=3))
Plot_Diet_2075_codcrab

Plot_Diet_2075<-ggarrange(Plot_Diet_2075_codshrimp,Plot_Diet_2075_turbotshrimp,Plot_Diet_2075_redfishshrimp,Plot_Diet_2075_codcrab,ncol=2,nrow=2,common.legend = FALSE,
                          labels=c("(a)","(b)","(c)","(d)"),label.x=0,label.y=0.98,font.label = list(size = 3))
Plot_Diet_2075

#Plot_Diet_2075<-ggplot((subset(Data_Diet_Plot,region=="Arctic"|region=="Maritimes"|region=="Gulf"|region=="Newfoundland_fall"|region=="Quebec"|region=="Newfoundland_spring")))+
 # geom_tile(aes(x,y,fill=value_2075))+
  #geom_polygon(data=Regions,aes(long,lat,group=group),fill=NA,color="black",size=0.5)+
  #geom_sf(data=Canada)+
  #geom_sf(data=US)+
  #coord_sf(xlim = c(300000,2000000), ylim=c(4750000,7300000))+
  #facet_wrap(~variable)+
  #coord_equal()+
  #ylab("Latitude")+
  #xlab("Longitude")+
  #scale_fill_gradient(low="white",high="red",name=expression("Probability of prey in stomach (%)"))+
  #theme(axis.line = element_line(colour = "black"),
   #     panel.grid.major = element_blank(),
    #    panel.grid.minor = element_blank(),
     #   panel.border = element_blank(),
      #  panel.background = element_blank(),
       # axis.text.x=element_blank(),
        #axis.text.y=element_blank(),
        #strip.background = element_rect(
        #  color="black", fill="white", size=1, linetype="solid"),
        #legend.position="bottom",
        #legend.key.width = unit(2, 'cm'),
        #text = element_text(size = 15))
#Plot_Diet_2075

ggsave("code/outputs/figures/FigureS9_Diet_projection_map_2075.jpeg",plot=Plot_Diet_2075,dpi=600, width = 1961, height = 1961, units = "px")
