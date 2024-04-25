### Figure 1 depth and region map ----
# Purpose: Generate map for paper that demonstrates depth and regions
# Author: S. Zabihi-Seissan, R. Stanley

# Load packages
library(rnaturalearth)
library(rnaturalearthdata)
library(data.table)
library(ggplot2)
library(rgdal)
library(ggpubr)
library(sf)
library(sp)
library(dplyr)
library(spatialEco) # devtools::install_github("jeffreyevans/spatialEco") #
library(cmocean)
library(latex2exp)
library(patchwork)
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
#### Load Data ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

# Load BNAM and depth data
BNAM_Data<-fread('code/outputs/data/BNAM_Data_Output.csv')
BNAM_Data$region<-as.factor(BNAM_Data$region)
BNAM_Data<-BNAM_Data[depth<0]
BNAM_Data$depth<-(-(BNAM_Data$depth))

# Load in regions shapefile
Regions<-readOGR('data/shapefiles/Survey_areas.shp')
Regions<-spTransform(Regions,CRS("+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs"))

# Load in land layer
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


#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
#### Plot ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

depth_map<-ggplot((subset(BNAM_Data,region=="Arctic"|region=="Maritimes"|region=="Gulf"|region=="Newfoundland_fall"|region=="Quebec"|region=="Newfoundland_spring")))+
  geom_raster(aes(x,y,fill=depth))+
  geom_polygon(data=Regions,aes(long,lat,group=group),fill=NA,color="black",size=0.05)+
  geom_sf(data=Canada,size=0.05)+
  geom_sf(data=US,size=0.05)+
  annotate("text",x=900000,y=7055000,label='AT',fontface="bold",size=0.75)+
  annotate("text",x=1700000,y=5200000,label='NLF',fontface="bold",size=0.75)+
  annotate("text",x=1250000,y=5100000,label='NLS',fontface="bold",size=0.75)+
  annotate("text",x=950000,y=5360000,label='QC',fontface="bold",size=0.75)+
  annotate("text",x=730000,y=5310000,label='SGSL',fontface="bold",size=0.75)+
  annotate("text",x=730000,y=4850000,label='MR',fontface="bold",size=0.75)+
  annotate("text",x=650000,y=6055000,label='Canada',size=0.75)+
  annotate("text",x=280000,y=5150000,label='USA',size=0.75)+
  coord_sf(xlim = c(280000,2000000), ylim=c(4750000,7300000))+
  ylab("Latitude")+
  xlab("Longitude")+
  scale_fill_gradient(low="lightblue2",high="navyblue",name=expression("Depth (m)"))+
  theme(axis.line = element_line(colour = "black",size=0.05),
        axis.ticks = element_line(size=0.05),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
       # axis.text.x=element_blank(),
      #  axis.text.y=element_blank(),
        strip.background = element_rect(
          color="black", fill="white", size=0.05, linetype="solid"),
        legend.position="bottom",
        legend.box.spacing = unit(0.02, 'cm'),
        legend.key.width = unit(0.3, 'cm'),
        legend.key.height = unit(0.1, 'cm'),
        text = element_text(size = 3))
print(depth_map)

temperature_avg_map <- ggplot((subset(BNAM_Data,region=="Arctic"|region=="Maritimes"|region=="Gulf"|region=="Newfoundland_fall"|region=="Quebec"|region=="Newfoundland_spring")))+
  geom_raster(aes(x,y,fill=BNAM_total_mean))+
  geom_polygon(data=Regions,aes(long,lat,group=group),fill=NA,color="black",size=0.05)+
  geom_sf(data=Canada,size=0.05)+
  geom_sf(data=US,size=0.05)+
 # labs(title = "Mean fall temperature\n1995 - 2018",size=2) + 
  annotate("text",x=900000,y=7055000,label='AT',fontface="bold",size=0.75)+
  annotate("text",x=1700000,y=5200000,label='NLF',fontface="bold",size=0.75)+
  annotate("text",x=1250000,y=5100000,label='NLS',fontface="bold",size=0.75)+
  annotate("text",x=950000,y=5360000,label='QC',fontface="bold",size=0.75)+
  annotate("text",x=730000,y=5310000,label='SGSL',fontface="bold",size=0.75)+
  annotate("text",x=730000,y=4850000,label='MR',fontface="bold",size=0.75)+
  annotate("text",x=650000,y=6055000,label='Canada',size=0.75)+
  annotate("text",x=280000,y=5150000,label='USA',size=0.75)+
  coord_sf(xlim = c(280000,2000000), ylim=c(4750000,7300000))+
  ylab("Latitude")+
  xlab("Longitude")+
  scale_fill_cmocean(TeX("Temperature ($^{\\circ} C$)"), 
                     name = "thermal",limits = c(-2.5,10))+  
  theme(axis.line = element_line(colour = "black",size=0.05),
        axis.ticks = element_line(size=0.05),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        # axis.text.x=element_blank(),
        #  axis.text.y=element_blank(),
        strip.background = element_rect(
          color="black", fill="white", size=0.05, linetype="solid"),
        legend.position="bottom",
        legend.box.spacing = unit(0.02, 'cm'),
        legend.key.width = unit(0.2, 'cm'),
        legend.key.height = unit(0.1, 'cm'),
        text = element_text(size = 3))

temperature_2075_map <- ggplot((subset(BNAM_Data,
                                       region=="Arctic"|region=="Maritimes"|region=="Gulf"|region=="Newfoundland_fall"|region=="Quebec"|region=="Newfoundland_spring")))+
  geom_raster(aes(x,y,fill=BNAM_2075_mean))+
  geom_polygon(data=Regions,aes(long,lat,group=group),fill=NA,color="black",size=0.05)+
  geom_sf(data=Canada,size=0.05)+
  geom_sf(data=US,size=0.05)+
 # labs(title = "Predicted mean fall temperature\n2075",size=2) +
  annotate("text",x=900000,y=7055000,label='AT',fontface="bold",size=0.75)+
  annotate("text",x=1700000,y=5200000,label='NLF',fontface="bold",size=0.75)+
  annotate("text",x=1250000,y=5100000,label='NLS',fontface="bold",size=0.75)+
  annotate("text",x=950000,y=5360000,label='QC',fontface="bold",size=0.75)+
  annotate("text",x=730000,y=5310000,label='SGSL',fontface="bold",size=0.75)+
  annotate("text",x=730000,y=4850000,label='MR',fontface="bold",size=0.75)+
  annotate("text",x=650000,y=6055000,label='Canada',size=0.75)+
  annotate("text",x=300000,y=5150000,label='USA',size=0.75)+
  annotate("text",x=280000,y=5150000,label='USA',size=0.75)+
  coord_sf(xlim = c(280000,2000000), ylim=c(4750000,7300000))+
  ylab("Latitude")+
  xlab("Longitude")+
  scale_fill_cmocean(TeX("Temperature ($^{\\circ} C$)"), 
                     name = "thermal",limits = c(-2.5,10))+  
  theme(axis.line = element_line(colour = "black",size=0.05),
        axis.ticks = element_line(size=0.05),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        # axis.text.x=element_blank(),
        #  axis.text.y=element_blank(),
        strip.background = element_rect(
          color="black", fill="white", size=0.05, linetype="solid"),
        legend.position="bottom",
        legend.box.spacing = unit(0.02, 'cm'),
        legend.key.width = unit(0.2, 'cm'),
        legend.key.height = unit(0.1, 'cm'),
        text = element_text(size = 3))


temperature_delta_map <- ggplot((subset(BNAM_Data,region=="Arctic"|region=="Maritimes"|region=="Gulf"|region=="Newfoundland_fall"|region=="Quebec"|region=="Newfoundland_spring")))+
  geom_raster(aes(x,y,fill=BNAM_2075_mean - BNAM_total_mean))+
  geom_polygon(data=Regions,aes(long,lat,group=group),fill=NA,color="black",size=0.05)+
  geom_sf(data=Canada,size=0.05)+
  geom_sf(data=US,size=0.05)+
  #labs(title = "Difference between 2075 and 1995-2018\nmean temperature",size=2) +
  annotate("text",x=900000,y=7055000,label='AT',fontface="bold",size=0.75)+
  annotate("text",x=1700000,y=5200000,label='NLF',fontface="bold",size=0.75)+
  annotate("text",x=1250000,y=5100000,label='NLS',fontface="bold",size=0.75)+
  annotate("text",x=950000,y=5360000,label='QC',fontface="bold",size=0.75)+
  annotate("text",x=730000,y=5310000,label='SGSL',fontface="bold",size=0.75)+
  annotate("text",x=730000,y=4850000,label='MR',fontface="bold",size=0.75)+
  annotate("text",x=650000,y=6055000,label='Canada',size=0.75)+
  annotate("text",x=280000,y=5150000,label='USA',size=0.75)+
  coord_sf(xlim = c(280000,2000000), ylim=c(4750000,7300000))+
  ylab("Latitude")+
  xlab("Longitude")+
  scale_fill_cmocean(TeX("∆ Temperature ($^{\\circ} C$)"), 
                     limits = c(-2.5,2.5),
                     name = "balance")+  
  theme(axis.line = element_line(colour = "black",size=0.05),
        axis.ticks = element_line(size=0.05),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        # axis.text.x=element_blank(),
        #  axis.text.y=element_blank(),
        strip.background = element_rect(
          color="black", fill="white", size=0.05, linetype="solid"),
        legend.position="bottom",
        legend.box.spacing = unit(0.02, 'cm'),
        legend.key.width = unit(0.2, 'cm'),
        legend.key.height = unit(0.1, 'cm'),
        text = element_text(size = 3))


#print(temperature_avg_map +temperature_2075_map +temperature_delta_map )

#temperature_map_compiled <- temperature_avg_map +temperature_2075_map +temperature_delta_map

temperature_map_compiled<-ggarrange(temperature_avg_map,temperature_2075_map,temperature_delta_map,ncol=3,nrow=1,common.legend = FALSE,legend="bottom",
                                  labels=c("(a)","(b)","(c)"),label.x=0.15,label.y=0.98,font.label = list(size = 3))

ggsave("code/outputs/figures/Figure 1_depth_map.jpeg",plot=depth_map, 
       dpi=600, width = 945, height = 1050, units = "px")

ggsave("code/outputs/figures/FigureS4_bottom_temp_map.jpg",plot=temperature_map_compiled , 
       dpi=600, width = 1961, height = 980.5, units = "px")


#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
#### Calculate survey area (km2) ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

#projections
utm <- "+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs"
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0" 

#load the trawl data and convert to an sf dataframe
rvdat <- read.csv("c:/Users/stanleyr/Downloads/Data.csv")%>%
  dplyr::select(ID,region,longitude,latitude)%>%
  st_as_sf(coords=c("longitude","latitude"),crs=utm,remove=FALSE)%>%
  rename(long_utm=longitude,lat_utm=latitude)%>%
  st_transform(latlong)%>%
  mutate(long=st_coordinates(.)[,1],
         lat=st_coordinates(.)[,2])%>%
  distinct(long,.keep_all = TRUE) #get rid of any duplicate points

#create a basemap of the land 
basemap <- rbind(ne_states(country = "Canada",returnclass = "sf")%>%
                   dplyr::select(latitude,longitude,geonunit,geometry)%>%
                   st_union()%>% #group provinces + territories
                   st_as_sf()%>%
                   st_transform(latlong),
                 ne_states(country = "United States of America",returnclass = "sf")%>%
                   dplyr::select(latitude,longitude,geonunit,geometry)%>%
                   st_union()%>% #group provinces + territories
                   st_as_sf()%>%
                   st_transform(latlong))%>%
  st_intersection(.,rvdat%>%st_bbox()%>%st_as_sfc()%>%st_as_sf())# this will trim the polygon to the extent of our focal area of interest using a bounding box

#create a bounding polygon for the area calculation
poly <- rvdat%>%
  as_Spatial()%>%
  convexHull(.,alpha = 3)%>% # this is from the spatial eco package. setting it to 3 (degrees) does the best job at covering the points without creating islands of points
  st_as_sf()%>%
  st_set_crs(latlong)%>%
  st_difference(basemap%>%st_union())

#calculate the area in km2
study_area <- as.numeric(st_area(poly)/1000/1000) # that's big

#plot it out
ggplot()+
  geom_sf(data=poly)+
  geom_sf(data=rvdat%>%sample_frac(0.4),pch=19,size=0.1)+ #thinned it out a bit
  theme_bw()+
  coord_sf(expand=0)