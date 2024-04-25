### Projecting shrimp and crab models using BNAM for 2075 but holding predator variables at longterm average ----
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
Data<-melt(BNAM_Data, id.vars = c("x","y","depth","region"), measure.vars = c("BNAM_2075_mean","BNAM_total_mean"))

# Rename groups and columns to match models
colnames(Data)[colnames(Data)=="value"] <- "temperature_at_bottom"
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

Data_Temp<-Data

# Load climate only SDMs
models <- list.files("code/outputs/models/climate", pattern="*.rda", full.names=TRUE)
lapply(models,load,.GlobalEnv)

# Predict values
Data_Temp$shrimp_climate<-predict(climate_shrimpmodel,newdata = Data_Temp,type="response")
Data_Temp$crab_climate<-predict(climate_crabmodel,newdata= Data_Temp,type="response")

# Format data
Data_Shrimp<-melt(Data_Temp, id.vars = c("x","y","depth","temperature_at_bottom","year_surv","region","dummy"), measure.vars = c("shrimp_climate"))
Data_Crab<-melt(Data_Temp, id.vars = c("x","y","depth","temperature_at_bottom","year_surv","region","dummy"), measure.vars = c("crab_climate"))
Data_Shrimp<-Data_Shrimp[year_surv=="2075"|year_surv=="0000"]
Data_Crab<-Data_Crab[year_surv=="2075"|year_surv=="0000"]

# Calculate difference
Data_Shrimp_2075<-Data_Shrimp[year_surv=="2075"]
Data_Shrimp_2075$value_2075<-Data_Shrimp_2075$value
Data_Shrimp_2075$value<-NULL
Data_Shrimp_2075$year_surv<-NULL
Data_Shrimp_0000<-Data_Shrimp[year_surv=="0000"]
Data_Shrimp_0000$value_0000<-Data_Shrimp_0000$value
Data_Shrimp_0000$value<-NULL
Data_Shrimp_0000$year_surv<-NULL
Data_Shrimp<-merge(Data_Shrimp_2075,Data_Shrimp_0000,by =c("x","y","depth",
                                                           "region","dummy","variable"),all=TRUE)
Data_Shrimp$value<-(Data_Shrimp$value_2075)-(Data_Shrimp$value_0000)
Data_Shrimp_Climate<-Data_Shrimp

Data_Crab_2075<-Data_Crab[year_surv=="2075"]
Data_Crab_2075$value_2075<-Data_Crab_2075$value
Data_Crab_2075$value<-NULL
Data_Crab_2075$year_surv<-NULL
Data_Crab_0000<-Data_Crab[year_surv=="0000"]
Data_Crab_0000$value_0000<-Data_Crab_0000$value
Data_Crab_0000$value<-NULL
Data_Crab_0000$year_surv<-NULL
Data_Crab<-merge(Data_Crab_2075,Data_Crab_0000,by =c("x","y","depth",
                                                     "region","dummy","variable"),all=TRUE)
Data_Crab$value<-(Data_Crab$value_2075)-(Data_Crab$value_0000)
Data_Crab_Climate<-Data_Crab

rm(list=setdiff(ls(), c("Data","Data_Shrimp_Climate","Data_Crab_Climate")))


#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
#### Projecting climate+predator density models ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

# Load climate only and diet models for lagged predator density projection
models <- list.files("code/outputs/models/climate", pattern="*.rda", full.names=TRUE)
lapply(models,load,.GlobalEnv)

#Subset longterm average data and predict predator density for average and 2075 at long term average value
Data_0000<-Data[year_surv=="0000"]
Data_2075<-Data[year_surv=="2075"]
Data_0000$pred_cod<-predict(climate_codmodel,newdata = Data_0000,type="response")
Data_0000$pred_turbot<-predict(climate_turbotmodel,newdata = Data_0000,type="response")
Data_0000$pred_redfish<-predict(climate_redfishmodel,newdata = Data_0000,type="response")
Data_0000_merging<-Data_0000
Data_0000_merging[,c(3:8)]<-NULL
Data_2075<-merge(Data_2075,Data_0000_merging,by=c("x","y"))

#Bring data back together
Data_Temp<-rbind(Data_0000,Data_2075)

# Load climate+predator SDMs
models <- list.files("code/outputs/models/climate/Climate_Predator_Density", pattern="*.rda", full.names=TRUE)
lapply(models,load,.GlobalEnv)

Data_Temp$shrimp_climatepred<-predict(climate_shrimpmodel,newdata = Data_Temp,type="response")
Data_Temp$crab_climatepred<-predict(climate_crabmodel,newdata= Data_Temp,type="response")

# Format data
Data_Shrimp<-melt(Data_Temp, id.vars = c("x","y","depth","temperature_at_bottom","year_surv","region","dummy"), measure.vars = c("shrimp_climatepred"))
Data_Crab<-melt(Data_Temp, id.vars = c("x","y","depth","temperature_at_bottom","year_surv","region","dummy"), measure.vars = c("crab_climatepred"))
Data_Shrimp<-Data_Shrimp[year_surv=="2075"|year_surv=="0000"]
Data_Crab<-Data_Crab[year_surv=="2075"|year_surv=="0000"]

# Calculate difference
Data_Shrimp_2075<-Data_Shrimp[year_surv=="2075"]
Data_Shrimp_2075$value_2075<-Data_Shrimp_2075$value
Data_Shrimp_2075$value<-NULL
Data_Shrimp_2075$year_surv<-NULL
Data_Shrimp_0000<-Data_Shrimp[year_surv=="0000"]
Data_Shrimp_0000$value_0000<-Data_Shrimp_0000$value
Data_Shrimp_0000$value<-NULL
Data_Shrimp_0000$year_surv<-NULL
Data_Shrimp<-merge(Data_Shrimp_2075,Data_Shrimp_0000,by =c("x","y","depth",
                                                           "region","dummy","variable"),all=TRUE)
Data_Shrimp$value<-(Data_Shrimp$value_2075)-(Data_Shrimp$value_0000)
Data_Shrimp_Climatepred<-Data_Shrimp

Data_Crab_2075<-Data_Crab[year_surv=="2075"]
Data_Crab_2075$value_2075<-Data_Crab_2075$value
Data_Crab_2075$value<-NULL
Data_Crab_2075$year_surv<-NULL
Data_Crab_0000<-Data_Crab[year_surv=="0000"]
Data_Crab_0000$value_0000<-Data_Crab_0000$value
Data_Crab_0000$value<-NULL
Data_Crab_0000$year_surv<-NULL
Data_Crab<-merge(Data_Crab_2075,Data_Crab_0000,by =c("x","y","depth",
                                                     "region","dummy","variable"),all=TRUE)
Data_Crab$value<-(Data_Crab$value_2075)-(Data_Crab$value_0000)
Data_Crab_Climatepred<-Data_Crab

rm(list=setdiff(ls(), c("Data","Data_Shrimp_Climate","Data_Crab_Climate","Data_Shrimp_Climatepred","Data_Crab_Climatepred")))


#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
#### Projecting climate+predator index models ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

# Load climate only for lagged predator density projection
models <- list.files("code/outputs/models/climate", pattern="*.rda", full.names=TRUE)
lapply(models,load,.GlobalEnv)
models_diet <- list.files("code/outputs/models/diet", pattern="*.rda", full.names=TRUE)
lapply(models_diet,load,.GlobalEnv)

#Subset longterm average data and predict predator density for average and 2075 at long term average value
Data_0000<-Data[year_surv=="0000"]
Data_2075<-Data[year_surv=="2075"]
Data_0000$pred_cod<-predict(climate_codmodel,newdata = Data_0000,type="response")
Data_0000$pred_turbot<-predict(climate_turbotmodel,newdata = Data_0000,type="response")
Data_0000$pred_redfish<-predict(climate_redfishmodel,newdata = Data_0000,type="response")
Data_0000$pred_shrimp<-predict(climate_shrimpmodel,newdata = Data_0000,type="response")
Data_0000$pred_crab<-predict(climate_crabmodel,newdata = Data_0000,type="response")
Data_0000$pred_planktivores<-predict(climate_planktivoremodel,newdata = Data_0000,type="response")
Data_0000$cod_shrimp<-predict(cod_shrimp_presence,newdata=Data_0000,type="response")
Data_0000$turbot_shrimp<-predict(turbot_shrimp_presence,newdata=Data_0000,type="response")
Data_0000$redfish_shrimp<-predict(redfish_shrimp_presence,newdata=Data_0000,type="response")
Data_0000$cod_crab<-predict(cod_crab_presence,newdata=Data_0000,type="response")
Data_0000_merging<-Data_0000
Data_0000_merging[,c(3:8)]<-NULL
Data_2075<-merge(Data_2075,Data_0000_merging,by=c("x","y"))

#Bring data back together
Data_Temp<-rbind(Data_0000,Data_2075)

# Create predation index for each predator species
Data_Temp$pred_index_shrimp<-((Data_Temp$pred_cod*Data_Temp$cod_shrimp)+(Data_Temp$pred_turbot*Data_Temp$turbot_shrimp)+(Data_Temp$pred_redfish*Data_Temp$redfish_shrimp))
Data_Temp$pred_index_crab<-(Data_Temp$pred_cod*Data_Temp$cod_crab)

# Load climate + predator index models
models <- list.files("code/outputs/models/climate/Climate_Predator_Index", pattern="*.rda", full.names=TRUE)
lapply(models,load,.GlobalEnv)

# Predict models
Data_Temp$shrimp_climateind<-predict(climate_shrimpmodel,newdata = Data_Temp,type="response")
Data_Temp$crab_climateind<-predict(climate_crabmodel,newdata = Data_Temp,type="response")

# Format data
Data_Shrimp<-melt(Data_Temp, id.vars = c("x","y","depth","temperature_at_bottom","year_surv","region","dummy"), measure.vars = c("shrimp_climateind"))
Data_Crab<-melt(Data_Temp, id.vars = c("x","y","depth","temperature_at_bottom","year_surv","region","dummy"), measure.vars = c("crab_climateind"))
Data_Shrimp<-Data_Shrimp[year_surv=="2075"|year_surv=="0000"]
Data_Crab<-Data_Crab[year_surv=="2075"|year_surv=="0000"]

# Calculate difference
Data_Shrimp_2075<-Data_Shrimp[year_surv=="2075"]
Data_Shrimp_2075$value_2075<-Data_Shrimp_2075$value
Data_Shrimp_2075$value<-NULL
Data_Shrimp_2075$year_surv<-NULL
Data_Shrimp_0000<-Data_Shrimp[year_surv=="0000"]
Data_Shrimp_0000$value_0000<-Data_Shrimp_0000$value
Data_Shrimp_0000$value<-NULL
Data_Shrimp_0000$year_surv<-NULL
Data_Shrimp<-merge(Data_Shrimp_2075,Data_Shrimp_0000,by =c("x","y","depth",
                                                           "region","dummy","variable"),all=TRUE)
Data_Shrimp$value<-(Data_Shrimp$value_2075)-(Data_Shrimp$value_0000)
Data_Shrimp_Climateind<-Data_Shrimp

Data_Crab_2075<-Data_Crab[year_surv=="2075"]
Data_Crab_2075$value_2075<-Data_Crab_2075$value
Data_Crab_2075$value<-NULL
Data_Crab_2075$year_surv<-NULL
Data_Crab_0000<-Data_Crab[year_surv=="0000"]
Data_Crab_0000$value_0000<-Data_Crab_0000$value
Data_Crab_0000$value<-NULL
Data_Crab_0000$year_surv<-NULL
Data_Crab<-merge(Data_Crab_2075,Data_Crab_0000,by =c("x","y","depth",
                                                     "region","dummy","variable"),all=TRUE)
Data_Crab$value<-(Data_Crab$value_2075)-(Data_Crab$value_0000)
Data_Crab_Climateind<-Data_Crab

rm(list=setdiff(ls(), c("Data","Data_Shrimp_Climate","Data_Crab_Climate","Data_Shrimp_Climatepred","Data_Crab_Climatepred","Data_Shrimp_Climateind","Data_Crab_Climateind")))

# Merge all data together
Data_Shrimp_Plot<-rbind(Data_Shrimp_Climate,Data_Shrimp_Climatepred,Data_Shrimp_Climateind)
Data_Crab_Plot<-rbind(Data_Crab_Climate,Data_Crab_Climatepred,Data_Crab_Climateind)

rm(list=setdiff(ls(), c("Data","Data_Shrimp_Plot","Data_Crab_Plot")))

#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
#### Plotting projections (Static vs Dynamic predator density and predation index) ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

# Load dynamic data
Data_dynamic<-fread(file="code/outputs/data/predicted_data.csv")
Data_dynamic<-Data_dynamic[year_surv=="2075"]
Data_dynamic[,c(8:10,13:15,18:24)]<-NULL
Data_dynamic$year_surv<-as.factor(Data_dynamic$year_surv)

# Format static data
Data_static<-rbind(Data_Crab_Plot,Data_Shrimp_Plot)
Data_static<-dcast(Data_static,(x+y+depth+region~variable+dummy),value.var = "value_2075")

# Merge static and dynamic model outputs
Data_plot<-merge(Data_dynamic,Data_static,by=c("x","y","region","depth"),all=TRUE)

# Calculate difference between static and dynamic
Data_plot$shrimp_climate_df<-(Data_plot$shrimp_climate)-(Data_plot$shrimp_climate_0)
Data_plot$crab_climate_df<-(Data_plot$crab_climate)-(Data_plot$crab_climate_0)
Data_plot$shrimp_climatepred_df<-(Data_plot$shrimp_climatepred)-(Data_plot$shrimp_climatepred_0)
Data_plot$crab_climatepred_df<-(Data_plot$crab_climatepred)-(Data_plot$crab_climatepred_0)
Data_plot$shrimp_climateind_df<-(Data_plot$shrimp_climateind)-(Data_plot$shrimp_climateind_0)
Data_plot$crab_climateind_df<-(Data_plot$crab_climateind)-(Data_plot$crab_climateind_0)
Data_plot[,c(8:19)]<-NULL

# Format difference data
Data_plot<-melt(Data_plot, id.vars = c("x","y","depth","temperature_at_bottom","year_surv","region","dummy"), measure.vars = c("shrimp_climate_df","crab_climate_df","shrimp_climatepred_df","crab_climatepred_df","shrimp_climateind_df","crab_climateind_df"))

Data_Shrimp_Plot<-Data_plot[variable=="shrimp_climate_df"|variable=="shrimp_climatepred_df"|variable=="shrimp_climateind_df"]
Data_Crab_Plot<-Data_plot[variable=="crab_climate_df"|variable=="crab_climatepred_df"|variable=="crab_climateind_df"]

#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
#### Plotting projections ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

Data_Shrimp_Plot[variable=="shrimp_climate_df",variable:='C-SDM (Dynamic - Static)']
Data_Shrimp_Plot[variable=="shrimp_climatepred_df",variable:='P-SDM (Dynamic - Static)']
Data_Shrimp_Plot[variable=="shrimp_climateind_df",variable:='PI-SDM (Dynamic - Static)']
Data_Crab_Plot[variable=="crab_climate_df",variable:='C-SDM (Dynamic - Static)']
Data_Crab_Plot[variable=="crab_climatepred_df",variable:='P-SDM (Dynamic - Static)']
Data_Crab_Plot[variable=="crab_climateind_df",variable:='PI-SDM (Dynamic - Static)']

#Remove C-SDM for plotting
Data_Shrimp_Plot<-Data_Shrimp_Plot[!variable=='C-SDM (Dynamic - Static)']
Data_Crab_Plot<-Data_Crab_Plot[!variable=='C-SDM (Dynamic - Static)']

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
ggplot()+
  geom_sf(data=Canada)

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
  scale_fill_gradient2(low="blue",high="red",mid="white",midpoint=0,name=expression(paste("Difference in northern shrimp density between static and dynamic predator distributions", (kg*"/"*km^{2}))))+
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
        strip.text = element_text(colour = 'white',size=5),
        legend.background = element_blank(),
        plot.margin= margin(0.05,0.05,0.05,0.05,unit="cm"),
        legend.box.margin=margin(-0.04,-0.04,-0.04,-0.04, "cm"),
        legend.spacing.x = unit(0.15, 'cm'),
        text = element_text(size = 3))
Plot_Shrimp

p <- ggplot_gtable(ggplot_build(Plot_Shrimp))
strip_both <- which(grepl('strip-', p$layout$name))
fills <- c("#004D41","#3D83C4")
k <- 1

for (i in strip_both) {
  j <- which(grepl('rect', p$grobs[[i]]$grobs[[1]]$childrenOrder))
  p$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(p)


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
  scale_fill_gradient2(low="blue",high="red",mid="white",midpoint=0,name=expression(paste("Difference in snow crab density between static and dynamic predator distributions", (kg*"/"*km^{2}))))+
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
        strip.text = element_text(colour = 'white',size=5),
        legend.background = element_blank(),
        plot.margin= margin(0.05,0.05,0.05,0.05,unit="cm"),
        legend.box.margin=margin(-0.04,-0.04,-0.04,-0.04, "cm"),
        legend.spacing.x = unit(0.15, 'cm'),
        text = element_text(size = 3))
Plot_Crab

g <- ggplot_gtable(ggplot_build(Plot_Crab))
strip_both <- which(grepl('strip-', g$layout$name))
fills <- c("#004D41","#3D83C4")
k <- 1

for (i in strip_both) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)


# Combine both plots into 1

final_plot<-ggarrange(p,g,labels=c("(a)","(b)"),ncol=1,nrow=2,heights=c(1.37,1),label.x=0.15,label.y=0.95,font.label = list(size = 3))
final_plot
ggsave("code/figures/Figure8_static_vs_dynamic.jpeg",plot=final_plot,dpi=600, width = 1961, height = 2100, units = "px")
