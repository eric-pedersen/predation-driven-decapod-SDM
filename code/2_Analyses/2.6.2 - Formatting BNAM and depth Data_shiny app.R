### Load in BNAM and depth data into one dataset----
# Purpose: Show shrimp and crab distributions based on BNAM outputs
# Author: S. Zabihi-Seissan


# Load packages
library(rmatio)
library(raster)
library(tidyverse)
library(magrittr)
library(data.table)
library(mgcv)
library(sp)
library(ggplot2)
library(sf)
library(spatial)
library(rgdal)



#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
#### Load BNAM Data ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

# Get coords
temp_coords <- read.mat("data/BNAM/latlon.mat")
lons=temp_coords$Lons
lats=temp_coords$Lats

# Loading the matlab data
BNAM_1995_1=read.mat(paste0("data/BNAM/BNAM_output/",1995,"/","BT_",1,".mat"))
BNAM_1995_1=BNAM_1995_1$BT
BNAM_1995_2=read.mat(paste0("data/BNAM/BNAM_output/",1995,"/","BT_",2,".mat"))
BNAM_1995_2=BNAM_1995_2$BT
BNAM_1995_3=read.mat(paste0("data/BNAM/BNAM_output/",1995,"/","BT_",3,".mat"))
BNAM_1995_3=BNAM_1995_3$BT
BNAM_1995_4=read.mat(paste0("data/BNAM/BNAM_output/",1995,"/","BT_",4,".mat"))
BNAM_1995_4=BNAM_1995_4$BT
BNAM_1995_5=read.mat(paste0("data/BNAM/BNAM_output/",1995,"/","BT_",5,".mat"))
BNAM_1995_5=BNAM_1995_5$BT
BNAM_1995_6=read.mat(paste0("data/BNAM/BNAM_output/",1995,"/","BT_",6,".mat"))
BNAM_1995_6=BNAM_1995_6$BT
BNAM_1995_7=read.mat(paste0("data/BNAM/BNAM_output/",1995,"/","BT_",7,".mat"))
BNAM_1995_7=BNAM_1995_7$BT
BNAM_1995_8=read.mat(paste0("data/BNAM/BNAM_output/",1995,"/","BT_",8,".mat"))
BNAM_1995_8=BNAM_1995_8$BT
BNAM_1995_9=read.mat(paste0("data/BNAM/BNAM_output/",1995,"/","BT_",9,".mat"))
BNAM_1995_9=BNAM_1995_9$BT
BNAM_1995_10=read.mat(paste0("data/BNAM/BNAM_output/",1995,"/","BT_",10,".mat"))
BNAM_1995_10=BNAM_1995_10$BT
BNAM_1995_11=read.mat(paste0("data/BNAM/BNAM_output/",1995,"/","BT_",11,".mat"))
BNAM_1995_11=BNAM_1995_11$BT
BNAM_1995_12=read.mat(paste0("data/BNAM/BNAM_output/",1995,"/","BT_",12,".mat"))
BNAM_1995_12=BNAM_1995_12$BT

BNAM_1996_1=read.mat(paste0("data/BNAM/BNAM_output/",1996,"/","BT_",1,".mat"))
BNAM_1996_1=BNAM_1996_1$BT
BNAM_1996_2=read.mat(paste0("data/BNAM/BNAM_output/",1996,"/","BT_",2,".mat"))
BNAM_1996_2=BNAM_1996_2$BT
BNAM_1996_3=read.mat(paste0("data/BNAM/BNAM_output/",1996,"/","BT_",3,".mat"))
BNAM_1996_3=BNAM_1996_3$BT
BNAM_1996_4=read.mat(paste0("data/BNAM/BNAM_output/",1996,"/","BT_",4,".mat"))
BNAM_1996_4=BNAM_1996_4$BT
BNAM_1996_5=read.mat(paste0("data/BNAM/BNAM_output/",1996,"/","BT_",5,".mat"))
BNAM_1996_5=BNAM_1996_5$BT
BNAM_1996_6=read.mat(paste0("data/BNAM/BNAM_output/",1996,"/","BT_",6,".mat"))
BNAM_1996_6=BNAM_1996_6$BT
BNAM_1996_7=read.mat(paste0("data/BNAM/BNAM_output/",1996,"/","BT_",7,".mat"))
BNAM_1996_7=BNAM_1996_7$BT
BNAM_1996_8=read.mat(paste0("data/BNAM/BNAM_output/",1996,"/","BT_",8,".mat"))
BNAM_1996_8=BNAM_1996_8$BT
BNAM_1996_9=read.mat(paste0("data/BNAM/BNAM_output/",1996,"/","BT_",9,".mat"))
BNAM_1996_9=BNAM_1996_9$BT
BNAM_1996_10=read.mat(paste0("data/BNAM/BNAM_output/",1996,"/","BT_",10,".mat"))
BNAM_1996_10=BNAM_1996_10$BT
BNAM_1996_11=read.mat(paste0("data/BNAM/BNAM_output/",1996,"/","BT_",11,".mat"))
BNAM_1996_11=BNAM_1996_11$BT
BNAM_1996_12=read.mat(paste0("data/BNAM/BNAM_output/",1996,"/","BT_",12,".mat"))
BNAM_1996_12=BNAM_1996_12$BT

BNAM_1997_1=read.mat(paste0("data/BNAM/BNAM_output/",1997,"/","BT_",1,".mat"))
BNAM_1997_1=BNAM_1997_1$BT
BNAM_1997_2=read.mat(paste0("data/BNAM/BNAM_output/",1997,"/","BT_",2,".mat"))
BNAM_1997_2=BNAM_1997_2$BT
BNAM_1997_3=read.mat(paste0("data/BNAM/BNAM_output/",1997,"/","BT_",3,".mat"))
BNAM_1997_3=BNAM_1997_3$BT
BNAM_1997_4=read.mat(paste0("data/BNAM/BNAM_output/",1997,"/","BT_",4,".mat"))
BNAM_1997_4=BNAM_1997_4$BT
BNAM_1997_5=read.mat(paste0("data/BNAM/BNAM_output/",1997,"/","BT_",5,".mat"))
BNAM_1997_5=BNAM_1997_5$BT
BNAM_1997_6=read.mat(paste0("data/BNAM/BNAM_output/",1997,"/","BT_",6,".mat"))
BNAM_1997_6=BNAM_1997_6$BT
BNAM_1997_7=read.mat(paste0("data/BNAM/BNAM_output/",1997,"/","BT_",7,".mat"))
BNAM_1997_7=BNAM_1997_7$BT
BNAM_1997_8=read.mat(paste0("data/BNAM/BNAM_output/",1997,"/","BT_",8,".mat"))
BNAM_1997_8=BNAM_1997_8$BT
BNAM_1997_9=read.mat(paste0("data/BNAM/BNAM_output/",1997,"/","BT_",9,".mat"))
BNAM_1997_9=BNAM_1997_9$BT
BNAM_1997_10=read.mat(paste0("data/BNAM/BNAM_output/",1997,"/","BT_",10,".mat"))
BNAM_1997_10=BNAM_1997_10$BT
BNAM_1997_11=read.mat(paste0("data/BNAM/BNAM_output/",1997,"/","BT_",11,".mat"))
BNAM_1997_11=BNAM_1997_11$BT
BNAM_1997_12=read.mat(paste0("data/BNAM/BNAM_output/",1997,"/","BT_",12,".mat"))
BNAM_1997_12=BNAM_1997_12$BT

BNAM_1998_1=read.mat(paste0("data/BNAM/BNAM_output/",1998,"/","BT_",1,".mat"))
BNAM_1998_1=BNAM_1998_1$BT
BNAM_1998_2=read.mat(paste0("data/BNAM/BNAM_output/",1998,"/","BT_",2,".mat"))
BNAM_1998_2=BNAM_1998_2$BT
BNAM_1998_3=read.mat(paste0("data/BNAM/BNAM_output/",1998,"/","BT_",3,".mat"))
BNAM_1998_3=BNAM_1998_3$BT
BNAM_1998_4=read.mat(paste0("data/BNAM/BNAM_output/",1998,"/","BT_",4,".mat"))
BNAM_1998_4=BNAM_1998_4$BT
BNAM_1998_5=read.mat(paste0("data/BNAM/BNAM_output/",1998,"/","BT_",5,".mat"))
BNAM_1998_5=BNAM_1998_5$BT
BNAM_1998_6=read.mat(paste0("data/BNAM/BNAM_output/",1998,"/","BT_",6,".mat"))
BNAM_1998_6=BNAM_1998_6$BT
BNAM_1998_7=read.mat(paste0("data/BNAM/BNAM_output/",1998,"/","BT_",7,".mat"))
BNAM_1998_7=BNAM_1998_7$BT
BNAM_1998_8=read.mat(paste0("data/BNAM/BNAM_output/",1998,"/","BT_",8,".mat"))
BNAM_1998_8=BNAM_1998_8$BT
BNAM_1998_9=read.mat(paste0("data/BNAM/BNAM_output/",1998,"/","BT_",9,".mat"))
BNAM_1998_9=BNAM_1998_9$BT
BNAM_1998_10=read.mat(paste0("data/BNAM/BNAM_output/",1998,"/","BT_",10,".mat"))
BNAM_1998_10=BNAM_1998_10$BT
BNAM_1998_11=read.mat(paste0("data/BNAM/BNAM_output/",1998,"/","BT_",11,".mat"))
BNAM_1998_11=BNAM_1998_11$BT
BNAM_1998_12=read.mat(paste0("data/BNAM/BNAM_output/",1998,"/","BT_",12,".mat"))
BNAM_1998_12=BNAM_1998_12$BT

BNAM_1999_1=read.mat(paste0("data/BNAM/BNAM_output/",1999,"/","BT_",1,".mat"))
BNAM_1999_1=BNAM_1999_1$BT
BNAM_1999_2=read.mat(paste0("data/BNAM/BNAM_output/",1999,"/","BT_",2,".mat"))
BNAM_1999_2=BNAM_1999_2$BT
BNAM_1999_3=read.mat(paste0("data/BNAM/BNAM_output/",1999,"/","BT_",3,".mat"))
BNAM_1999_3=BNAM_1999_3$BT
BNAM_1999_4=read.mat(paste0("data/BNAM/BNAM_output/",1999,"/","BT_",4,".mat"))
BNAM_1999_4=BNAM_1999_4$BT
BNAM_1999_5=read.mat(paste0("data/BNAM/BNAM_output/",1999,"/","BT_",5,".mat"))
BNAM_1999_5=BNAM_1999_5$BT
BNAM_1999_6=read.mat(paste0("data/BNAM/BNAM_output/",1999,"/","BT_",6,".mat"))
BNAM_1999_6=BNAM_1999_6$BT
BNAM_1999_7=read.mat(paste0("data/BNAM/BNAM_output/",1999,"/","BT_",7,".mat"))
BNAM_1999_7=BNAM_1999_7$BT
BNAM_1999_8=read.mat(paste0("data/BNAM/BNAM_output/",1999,"/","BT_",8,".mat"))
BNAM_1999_8=BNAM_1999_8$BT
BNAM_1999_9=read.mat(paste0("data/BNAM/BNAM_output/",1999,"/","BT_",9,".mat"))
BNAM_1999_9=BNAM_1999_9$BT
BNAM_1999_10=read.mat(paste0("data/BNAM/BNAM_output/",1999,"/","BT_",10,".mat"))
BNAM_1999_10=BNAM_1999_10$BT
BNAM_1999_11=read.mat(paste0("data/BNAM/BNAM_output/",1999,"/","BT_",11,".mat"))
BNAM_1999_11=BNAM_1999_11$BT
BNAM_1999_12=read.mat(paste0("data/BNAM/BNAM_output/",1999,"/","BT_",12,".mat"))
BNAM_1999_12=BNAM_1999_12$BT

BNAM_2000_1=read.mat(paste0("data/BNAM/BNAM_output/",2000,"/","BT_",1,".mat"))
BNAM_2000_1=BNAM_2000_1$BT
BNAM_2000_2=read.mat(paste0("data/BNAM/BNAM_output/",2000,"/","BT_",2,".mat"))
BNAM_2000_2=BNAM_2000_2$BT
BNAM_2000_3=read.mat(paste0("data/BNAM/BNAM_output/",2000,"/","BT_",3,".mat"))
BNAM_2000_3=BNAM_2000_3$BT
BNAM_2000_4=read.mat(paste0("data/BNAM/BNAM_output/",2000,"/","BT_",4,".mat"))
BNAM_2000_4=BNAM_2000_4$BT
BNAM_2000_5=read.mat(paste0("data/BNAM/BNAM_output/",2000,"/","BT_",5,".mat"))
BNAM_2000_5=BNAM_2000_5$BT
BNAM_2000_6=read.mat(paste0("data/BNAM/BNAM_output/",2000,"/","BT_",6,".mat"))
BNAM_2000_6=BNAM_2000_6$BT
BNAM_2000_7=read.mat(paste0("data/BNAM/BNAM_output/",2000,"/","BT_",7,".mat"))
BNAM_2000_7=BNAM_2000_7$BT
BNAM_2000_8=read.mat(paste0("data/BNAM/BNAM_output/",2000,"/","BT_",8,".mat"))
BNAM_2000_8=BNAM_2000_8$BT
BNAM_2000_9=read.mat(paste0("data/BNAM/BNAM_output/",2000,"/","BT_",9,".mat"))
BNAM_2000_9=BNAM_2000_9$BT
BNAM_2000_10=read.mat(paste0("data/BNAM/BNAM_output/",2000,"/","BT_",10,".mat"))
BNAM_2000_10=BNAM_2000_10$BT
BNAM_2000_11=read.mat(paste0("data/BNAM/BNAM_output/",2000,"/","BT_",11,".mat"))
BNAM_2000_11=BNAM_2000_11$BT
BNAM_2000_12=read.mat(paste0("data/BNAM/BNAM_output/",2000,"/","BT_",12,".mat"))
BNAM_2000_12=BNAM_2000_12$BT

BNAM_2001_1=read.mat(paste0("data/BNAM/BNAM_output/",2001,"/","BT_",1,".mat"))
BNAM_2001_1=BNAM_2001_1$BT
BNAM_2001_2=read.mat(paste0("data/BNAM/BNAM_output/",2001,"/","BT_",2,".mat"))
BNAM_2001_2=BNAM_2001_2$BT
BNAM_2001_3=read.mat(paste0("data/BNAM/BNAM_output/",2001,"/","BT_",3,".mat"))
BNAM_2001_3=BNAM_2001_3$BT
BNAM_2001_4=read.mat(paste0("data/BNAM/BNAM_output/",2001,"/","BT_",4,".mat"))
BNAM_2001_4=BNAM_2001_4$BT
BNAM_2001_5=read.mat(paste0("data/BNAM/BNAM_output/",2001,"/","BT_",5,".mat"))
BNAM_2001_5=BNAM_2001_5$BT
BNAM_2001_6=read.mat(paste0("data/BNAM/BNAM_output/",2001,"/","BT_",6,".mat"))
BNAM_2001_6=BNAM_2001_6$BT
BNAM_2001_7=read.mat(paste0("data/BNAM/BNAM_output/",2001,"/","BT_",7,".mat"))
BNAM_2001_7=BNAM_2001_7$BT
BNAM_2001_8=read.mat(paste0("data/BNAM/BNAM_output/",2001,"/","BT_",8,".mat"))
BNAM_2001_8=BNAM_2001_8$BT
BNAM_2001_9=read.mat(paste0("data/BNAM/BNAM_output/",2001,"/","BT_",9,".mat"))
BNAM_2001_9=BNAM_2001_9$BT
BNAM_2001_10=read.mat(paste0("data/BNAM/BNAM_output/",2001,"/","BT_",10,".mat"))
BNAM_2001_10=BNAM_2001_10$BT
BNAM_2001_11=read.mat(paste0("data/BNAM/BNAM_output/",2001,"/","BT_",11,".mat"))
BNAM_2001_11=BNAM_2001_11$BT
BNAM_2001_12=read.mat(paste0("data/BNAM/BNAM_output/",2001,"/","BT_",12,".mat"))
BNAM_2001_12=BNAM_2001_12$BT

BNAM_2002_1=read.mat(paste0("data/BNAM/BNAM_output/",2002,"/","BT_",1,".mat"))
BNAM_2002_1=BNAM_2002_1$BT
BNAM_2002_2=read.mat(paste0("data/BNAM/BNAM_output/",2002,"/","BT_",2,".mat"))
BNAM_2002_2=BNAM_2002_2$BT
BNAM_2002_3=read.mat(paste0("data/BNAM/BNAM_output/",2002,"/","BT_",3,".mat"))
BNAM_2002_3=BNAM_2002_3$BT
BNAM_2002_4=read.mat(paste0("data/BNAM/BNAM_output/",2002,"/","BT_",4,".mat"))
BNAM_2002_4=BNAM_2002_4$BT
BNAM_2002_5=read.mat(paste0("data/BNAM/BNAM_output/",2002,"/","BT_",5,".mat"))
BNAM_2002_5=BNAM_2002_5$BT
BNAM_2002_6=read.mat(paste0("data/BNAM/BNAM_output/",2002,"/","BT_",6,".mat"))
BNAM_2002_6=BNAM_2002_6$BT
BNAM_2002_7=read.mat(paste0("data/BNAM/BNAM_output/",2002,"/","BT_",7,".mat"))
BNAM_2002_7=BNAM_2002_7$BT
BNAM_2002_8=read.mat(paste0("data/BNAM/BNAM_output/",2002,"/","BT_",8,".mat"))
BNAM_2002_8=BNAM_2002_8$BT
BNAM_2002_9=read.mat(paste0("data/BNAM/BNAM_output/",2002,"/","BT_",9,".mat"))
BNAM_2002_9=BNAM_2002_9$BT
BNAM_2002_10=read.mat(paste0("data/BNAM/BNAM_output/",2002,"/","BT_",10,".mat"))
BNAM_2002_10=BNAM_2002_10$BT
BNAM_2002_11=read.mat(paste0("data/BNAM/BNAM_output/",2002,"/","BT_",11,".mat"))
BNAM_2002_11=BNAM_2002_11$BT
BNAM_2002_12=read.mat(paste0("data/BNAM/BNAM_output/",2002,"/","BT_",12,".mat"))
BNAM_2002_12=BNAM_2002_12$BT

BNAM_2003_1=read.mat(paste0("data/BNAM/BNAM_output/",2003,"/","BT_",1,".mat"))
BNAM_2003_1=BNAM_2003_1$BT
BNAM_2003_2=read.mat(paste0("data/BNAM/BNAM_output/",2003,"/","BT_",2,".mat"))
BNAM_2003_2=BNAM_2003_2$BT
BNAM_2003_3=read.mat(paste0("data/BNAM/BNAM_output/",2003,"/","BT_",3,".mat"))
BNAM_2003_3=BNAM_2003_3$BT
BNAM_2003_4=read.mat(paste0("data/BNAM/BNAM_output/",2003,"/","BT_",4,".mat"))
BNAM_2003_4=BNAM_2003_4$BT
BNAM_2003_5=read.mat(paste0("data/BNAM/BNAM_output/",2003,"/","BT_",5,".mat"))
BNAM_2003_5=BNAM_2003_5$BT
BNAM_2003_6=read.mat(paste0("data/BNAM/BNAM_output/",2003,"/","BT_",6,".mat"))
BNAM_2003_6=BNAM_2003_6$BT
BNAM_2003_7=read.mat(paste0("data/BNAM/BNAM_output/",2003,"/","BT_",7,".mat"))
BNAM_2003_7=BNAM_2003_7$BT
BNAM_2003_8=read.mat(paste0("data/BNAM/BNAM_output/",2003,"/","BT_",8,".mat"))
BNAM_2003_8=BNAM_2003_8$BT
BNAM_2003_9=read.mat(paste0("data/BNAM/BNAM_output/",2003,"/","BT_",9,".mat"))
BNAM_2003_9=BNAM_2003_9$BT
BNAM_2003_10=read.mat(paste0("data/BNAM/BNAM_output/",2003,"/","BT_",10,".mat"))
BNAM_2003_10=BNAM_2003_10$BT
BNAM_2003_11=read.mat(paste0("data/BNAM/BNAM_output/",2003,"/","BT_",11,".mat"))
BNAM_2003_11=BNAM_2003_11$BT
BNAM_2003_12=read.mat(paste0("data/BNAM/BNAM_output/",2003,"/","BT_",12,".mat"))
BNAM_2003_12=BNAM_2003_12$BT

BNAM_2004_1=read.mat(paste0("data/BNAM/BNAM_output/",2004,"/","BT_",1,".mat"))
BNAM_2004_1=BNAM_2004_1$BT
BNAM_2004_2=read.mat(paste0("data/BNAM/BNAM_output/",2004,"/","BT_",2,".mat"))
BNAM_2004_2=BNAM_2004_2$BT
BNAM_2004_3=read.mat(paste0("data/BNAM/BNAM_output/",2004,"/","BT_",3,".mat"))
BNAM_2004_3=BNAM_2004_3$BT
BNAM_2004_4=read.mat(paste0("data/BNAM/BNAM_output/",2004,"/","BT_",4,".mat"))
BNAM_2004_4=BNAM_2004_4$BT
BNAM_2004_5=read.mat(paste0("data/BNAM/BNAM_output/",2004,"/","BT_",5,".mat"))
BNAM_2004_5=BNAM_2004_5$BT
BNAM_2004_6=read.mat(paste0("data/BNAM/BNAM_output/",2004,"/","BT_",6,".mat"))
BNAM_2004_6=BNAM_2004_6$BT
BNAM_2004_7=read.mat(paste0("data/BNAM/BNAM_output/",2004,"/","BT_",7,".mat"))
BNAM_2004_7=BNAM_2004_7$BT
BNAM_2004_8=read.mat(paste0("data/BNAM/BNAM_output/",2004,"/","BT_",8,".mat"))
BNAM_2004_8=BNAM_2004_8$BT
BNAM_2004_9=read.mat(paste0("data/BNAM/BNAM_output/",2004,"/","BT_",9,".mat"))
BNAM_2004_9=BNAM_2004_9$BT
BNAM_2004_10=read.mat(paste0("data/BNAM/BNAM_output/",2004,"/","BT_",10,".mat"))
BNAM_2004_10=BNAM_2004_10$BT
BNAM_2004_11=read.mat(paste0("data/BNAM/BNAM_output/",2004,"/","BT_",11,".mat"))
BNAM_2004_11=BNAM_2004_11$BT
BNAM_2004_12=read.mat(paste0("data/BNAM/BNAM_output/",2004,"/","BT_",12,".mat"))
BNAM_2004_12=BNAM_2004_12$BT

BNAM_2005_1=read.mat(paste0("data/BNAM/BNAM_output/",2005,"/","BT_",1,".mat"))
BNAM_2005_1=BNAM_2005_1$BT
BNAM_2005_2=read.mat(paste0("data/BNAM/BNAM_output/",2005,"/","BT_",2,".mat"))
BNAM_2005_2=BNAM_2005_2$BT
BNAM_2005_3=read.mat(paste0("data/BNAM/BNAM_output/",2005,"/","BT_",3,".mat"))
BNAM_2005_3=BNAM_2005_3$BT
BNAM_2005_4=read.mat(paste0("data/BNAM/BNAM_output/",2005,"/","BT_",4,".mat"))
BNAM_2005_4=BNAM_2005_4$BT
BNAM_2005_5=read.mat(paste0("data/BNAM/BNAM_output/",2005,"/","BT_",5,".mat"))
BNAM_2005_5=BNAM_2005_5$BT
BNAM_2005_6=read.mat(paste0("data/BNAM/BNAM_output/",2005,"/","BT_",6,".mat"))
BNAM_2005_6=BNAM_2005_6$BT
BNAM_2005_7=read.mat(paste0("data/BNAM/BNAM_output/",2005,"/","BT_",7,".mat"))
BNAM_2005_7=BNAM_2005_7$BT
BNAM_2005_8=read.mat(paste0("data/BNAM/BNAM_output/",2005,"/","BT_",8,".mat"))
BNAM_2005_8=BNAM_2005_8$BT
BNAM_2005_9=read.mat(paste0("data/BNAM/BNAM_output/",2005,"/","BT_",9,".mat"))
BNAM_2005_9=BNAM_2005_9$BT
BNAM_2005_10=read.mat(paste0("data/BNAM/BNAM_output/",2005,"/","BT_",10,".mat"))
BNAM_2005_10=BNAM_2005_10$BT
BNAM_2005_11=read.mat(paste0("data/BNAM/BNAM_output/",2005,"/","BT_",11,".mat"))
BNAM_2005_11=BNAM_2005_11$BT
BNAM_2005_12=read.mat(paste0("data/BNAM/BNAM_output/",2005,"/","BT_",12,".mat"))
BNAM_2005_12=BNAM_2005_12$BT

BNAM_2006_1=read.mat(paste0("data/BNAM/BNAM_output/",2006,"/","BT_",1,".mat"))
BNAM_2006_1=BNAM_2006_1$BT
BNAM_2006_2=read.mat(paste0("data/BNAM/BNAM_output/",2006,"/","BT_",2,".mat"))
BNAM_2006_2=BNAM_2006_2$BT
BNAM_2006_3=read.mat(paste0("data/BNAM/BNAM_output/",2006,"/","BT_",3,".mat"))
BNAM_2006_3=BNAM_2006_3$BT
BNAM_2006_4=read.mat(paste0("data/BNAM/BNAM_output/",2006,"/","BT_",4,".mat"))
BNAM_2006_4=BNAM_2006_4$BT
BNAM_2006_5=read.mat(paste0("data/BNAM/BNAM_output/",2006,"/","BT_",5,".mat"))
BNAM_2006_5=BNAM_2006_5$BT
BNAM_2006_6=read.mat(paste0("data/BNAM/BNAM_output/",2006,"/","BT_",6,".mat"))
BNAM_2006_6=BNAM_2006_6$BT
BNAM_2006_7=read.mat(paste0("data/BNAM/BNAM_output/",2006,"/","BT_",7,".mat"))
BNAM_2006_7=BNAM_2006_7$BT
BNAM_2006_8=read.mat(paste0("data/BNAM/BNAM_output/",2006,"/","BT_",8,".mat"))
BNAM_2006_8=BNAM_2006_8$BT
BNAM_2006_9=read.mat(paste0("data/BNAM/BNAM_output/",2006,"/","BT_",9,".mat"))
BNAM_2006_9=BNAM_2006_9$BT
BNAM_2006_10=read.mat(paste0("data/BNAM/BNAM_output/",2006,"/","BT_",10,".mat"))
BNAM_2006_10=BNAM_2006_10$BT
BNAM_2006_11=read.mat(paste0("data/BNAM/BNAM_output/",2006,"/","BT_",11,".mat"))
BNAM_2006_11=BNAM_2006_11$BT
BNAM_2006_12=read.mat(paste0("data/BNAM/BNAM_output/",2006,"/","BT_",12,".mat"))
BNAM_2006_12=BNAM_2006_12$BT

BNAM_2007_1=read.mat(paste0("data/BNAM/BNAM_output/",2007,"/","BT_",1,".mat"))
BNAM_2007_1=BNAM_2007_1$BT
BNAM_2007_2=read.mat(paste0("data/BNAM/BNAM_output/",2007,"/","BT_",2,".mat"))
BNAM_2007_2=BNAM_2007_2$BT
BNAM_2007_3=read.mat(paste0("data/BNAM/BNAM_output/",2007,"/","BT_",3,".mat"))
BNAM_2007_3=BNAM_2007_3$BT
BNAM_2007_4=read.mat(paste0("data/BNAM/BNAM_output/",2007,"/","BT_",4,".mat"))
BNAM_2007_4=BNAM_2007_4$BT
BNAM_2007_5=read.mat(paste0("data/BNAM/BNAM_output/",2007,"/","BT_",5,".mat"))
BNAM_2007_5=BNAM_2007_5$BT
BNAM_2007_6=read.mat(paste0("data/BNAM/BNAM_output/",2007,"/","BT_",6,".mat"))
BNAM_2007_6=BNAM_2007_6$BT
BNAM_2007_7=read.mat(paste0("data/BNAM/BNAM_output/",2007,"/","BT_",7,".mat"))
BNAM_2007_7=BNAM_2007_7$BT
BNAM_2007_8=read.mat(paste0("data/BNAM/BNAM_output/",2007,"/","BT_",8,".mat"))
BNAM_2007_8=BNAM_2007_8$BT
BNAM_2007_9=read.mat(paste0("data/BNAM/BNAM_output/",2007,"/","BT_",9,".mat"))
BNAM_2007_9=BNAM_2007_9$BT
BNAM_2007_10=read.mat(paste0("data/BNAM/BNAM_output/",2007,"/","BT_",10,".mat"))
BNAM_2007_10=BNAM_2007_10$BT
BNAM_2007_11=read.mat(paste0("data/BNAM/BNAM_output/",2007,"/","BT_",11,".mat"))
BNAM_2007_11=BNAM_2007_11$BT
BNAM_2007_12=read.mat(paste0("data/BNAM/BNAM_output/",2007,"/","BT_",12,".mat"))
BNAM_2007_12=BNAM_2007_12$BT

BNAM_2008_1=read.mat(paste0("data/BNAM/BNAM_output/",2008,"/","BT_",1,".mat"))
BNAM_2008_1=BNAM_2008_1$BT
BNAM_2008_2=read.mat(paste0("data/BNAM/BNAM_output/",2008,"/","BT_",2,".mat"))
BNAM_2008_2=BNAM_2008_2$BT
BNAM_2008_3=read.mat(paste0("data/BNAM/BNAM_output/",2008,"/","BT_",3,".mat"))
BNAM_2008_3=BNAM_2008_3$BT
BNAM_2008_4=read.mat(paste0("data/BNAM/BNAM_output/",2008,"/","BT_",4,".mat"))
BNAM_2008_4=BNAM_2008_4$BT
BNAM_2008_5=read.mat(paste0("data/BNAM/BNAM_output/",2008,"/","BT_",5,".mat"))
BNAM_2008_5=BNAM_2008_5$BT
BNAM_2008_6=read.mat(paste0("data/BNAM/BNAM_output/",2008,"/","BT_",6,".mat"))
BNAM_2008_6=BNAM_2008_6$BT
BNAM_2008_7=read.mat(paste0("data/BNAM/BNAM_output/",2008,"/","BT_",7,".mat"))
BNAM_2008_7=BNAM_2008_7$BT
BNAM_2008_8=read.mat(paste0("data/BNAM/BNAM_output/",2008,"/","BT_",8,".mat"))
BNAM_2008_8=BNAM_2008_8$BT
BNAM_2008_9=read.mat(paste0("data/BNAM/BNAM_output/",2008,"/","BT_",9,".mat"))
BNAM_2008_9=BNAM_2008_9$BT
BNAM_2008_10=read.mat(paste0("data/BNAM/BNAM_output/",2008,"/","BT_",10,".mat"))
BNAM_2008_10=BNAM_2008_10$BT
BNAM_2008_11=read.mat(paste0("data/BNAM/BNAM_output/",2008,"/","BT_",11,".mat"))
BNAM_2008_11=BNAM_2008_11$BT
BNAM_2008_12=read.mat(paste0("data/BNAM/BNAM_output/",2008,"/","BT_",12,".mat"))
BNAM_2008_12=BNAM_2008_12$BT

BNAM_2009_1=read.mat(paste0("data/BNAM/BNAM_output/",2009,"/","BT_",1,".mat"))
BNAM_2009_1=BNAM_2009_1$BT
BNAM_2009_2=read.mat(paste0("data/BNAM/BNAM_output/",2009,"/","BT_",2,".mat"))
BNAM_2009_2=BNAM_2009_2$BT
BNAM_2009_3=read.mat(paste0("data/BNAM/BNAM_output/",2009,"/","BT_",3,".mat"))
BNAM_2009_3=BNAM_2009_3$BT
BNAM_2009_4=read.mat(paste0("data/BNAM/BNAM_output/",2009,"/","BT_",4,".mat"))
BNAM_2009_4=BNAM_2009_4$BT
BNAM_2009_5=read.mat(paste0("data/BNAM/BNAM_output/",2009,"/","BT_",5,".mat"))
BNAM_2009_5=BNAM_2009_5$BT
BNAM_2009_6=read.mat(paste0("data/BNAM/BNAM_output/",2009,"/","BT_",6,".mat"))
BNAM_2009_6=BNAM_2009_6$BT
BNAM_2009_7=read.mat(paste0("data/BNAM/BNAM_output/",2009,"/","BT_",7,".mat"))
BNAM_2009_7=BNAM_2009_7$BT
BNAM_2009_8=read.mat(paste0("data/BNAM/BNAM_output/",2009,"/","BT_",8,".mat"))
BNAM_2009_8=BNAM_2009_8$BT
BNAM_2009_9=read.mat(paste0("data/BNAM/BNAM_output/",2009,"/","BT_",9,".mat"))
BNAM_2009_9=BNAM_2009_9$BT
BNAM_2009_10=read.mat(paste0("data/BNAM/BNAM_output/",2009,"/","BT_",10,".mat"))
BNAM_2009_10=BNAM_2009_10$BT
BNAM_2009_11=read.mat(paste0("data/BNAM/BNAM_output/",2009,"/","BT_",11,".mat"))
BNAM_2009_11=BNAM_2009_11$BT
BNAM_2009_12=read.mat(paste0("data/BNAM/BNAM_output/",2009,"/","BT_",12,".mat"))
BNAM_2009_12=BNAM_2009_12$BT

BNAM_2010_1=read.mat(paste0("data/BNAM/BNAM_output/",2010,"/","BT_",1,".mat"))
BNAM_2010_1=BNAM_2010_1$BT
BNAM_2010_2=read.mat(paste0("data/BNAM/BNAM_output/",2010,"/","BT_",2,".mat"))
BNAM_2010_2=BNAM_2010_2$BT
BNAM_2010_3=read.mat(paste0("data/BNAM/BNAM_output/",2010,"/","BT_",3,".mat"))
BNAM_2010_3=BNAM_2010_3$BT
BNAM_2010_4=read.mat(paste0("data/BNAM/BNAM_output/",2010,"/","BT_",4,".mat"))
BNAM_2010_4=BNAM_2010_4$BT
BNAM_2010_5=read.mat(paste0("data/BNAM/BNAM_output/",2010,"/","BT_",5,".mat"))
BNAM_2010_5=BNAM_2010_5$BT
BNAM_2010_6=read.mat(paste0("data/BNAM/BNAM_output/",2010,"/","BT_",6,".mat"))
BNAM_2010_6=BNAM_2010_6$BT
BNAM_2010_7=read.mat(paste0("data/BNAM/BNAM_output/",2010,"/","BT_",7,".mat"))
BNAM_2010_7=BNAM_2010_7$BT
BNAM_2010_8=read.mat(paste0("data/BNAM/BNAM_output/",2010,"/","BT_",8,".mat"))
BNAM_2010_8=BNAM_2010_8$BT
BNAM_2010_9=read.mat(paste0("data/BNAM/BNAM_output/",2010,"/","BT_",9,".mat"))
BNAM_2010_9=BNAM_2010_9$BT
BNAM_2010_10=read.mat(paste0("data/BNAM/BNAM_output/",2010,"/","BT_",10,".mat"))
BNAM_2010_10=BNAM_2010_10$BT
BNAM_2010_11=read.mat(paste0("data/BNAM/BNAM_output/",2010,"/","BT_",11,".mat"))
BNAM_2010_11=BNAM_2010_11$BT
BNAM_2010_12=read.mat(paste0("data/BNAM/BNAM_output/",2010,"/","BT_",12,".mat"))
BNAM_2010_12=BNAM_2010_12$BT

BNAM_2011_1=read.mat(paste0("data/BNAM/BNAM_output/",2011,"/","BT_",1,".mat"))
BNAM_2011_1=BNAM_2011_1$BT
BNAM_2011_2=read.mat(paste0("data/BNAM/BNAM_output/",2011,"/","BT_",2,".mat"))
BNAM_2011_2=BNAM_2011_2$BT
BNAM_2011_3=read.mat(paste0("data/BNAM/BNAM_output/",2011,"/","BT_",3,".mat"))
BNAM_2011_3=BNAM_2011_3$BT
BNAM_2011_4=read.mat(paste0("data/BNAM/BNAM_output/",2011,"/","BT_",4,".mat"))
BNAM_2011_4=BNAM_2011_4$BT
BNAM_2011_5=read.mat(paste0("data/BNAM/BNAM_output/",2011,"/","BT_",5,".mat"))
BNAM_2011_5=BNAM_2011_5$BT
BNAM_2011_6=read.mat(paste0("data/BNAM/BNAM_output/",2011,"/","BT_",6,".mat"))
BNAM_2011_6=BNAM_2011_6$BT
BNAM_2011_7=read.mat(paste0("data/BNAM/BNAM_output/",2011,"/","BT_",7,".mat"))
BNAM_2011_7=BNAM_2011_7$BT
BNAM_2011_8=read.mat(paste0("data/BNAM/BNAM_output/",2011,"/","BT_",8,".mat"))
BNAM_2011_8=BNAM_2011_8$BT
BNAM_2011_9=read.mat(paste0("data/BNAM/BNAM_output/",2011,"/","BT_",9,".mat"))
BNAM_2011_9=BNAM_2011_9$BT
BNAM_2011_10=read.mat(paste0("data/BNAM/BNAM_output/",2011,"/","BT_",10,".mat"))
BNAM_2011_10=BNAM_2011_10$BT
BNAM_2011_11=read.mat(paste0("data/BNAM/BNAM_output/",2011,"/","BT_",11,".mat"))
BNAM_2011_11=BNAM_2011_11$BT
BNAM_2011_12=read.mat(paste0("data/BNAM/BNAM_output/",2011,"/","BT_",12,".mat"))
BNAM_2011_12=BNAM_2011_12$BT

BNAM_2012_1=read.mat(paste0("data/BNAM/BNAM_output/",2012,"/","BT_",1,".mat"))
BNAM_2012_1=BNAM_2012_1$BT
BNAM_2012_2=read.mat(paste0("data/BNAM/BNAM_output/",2012,"/","BT_",2,".mat"))
BNAM_2012_2=BNAM_2012_2$BT
BNAM_2012_3=read.mat(paste0("data/BNAM/BNAM_output/",2012,"/","BT_",3,".mat"))
BNAM_2012_3=BNAM_2012_3$BT
BNAM_2012_4=read.mat(paste0("data/BNAM/BNAM_output/",2012,"/","BT_",4,".mat"))
BNAM_2012_4=BNAM_2012_4$BT
BNAM_2012_5=read.mat(paste0("data/BNAM/BNAM_output/",2012,"/","BT_",5,".mat"))
BNAM_2012_5=BNAM_2012_5$BT
BNAM_2012_6=read.mat(paste0("data/BNAM/BNAM_output/",2012,"/","BT_",6,".mat"))
BNAM_2012_6=BNAM_2012_6$BT
BNAM_2012_7=read.mat(paste0("data/BNAM/BNAM_output/",2012,"/","BT_",7,".mat"))
BNAM_2012_7=BNAM_2012_7$BT
BNAM_2012_8=read.mat(paste0("data/BNAM/BNAM_output/",2012,"/","BT_",8,".mat"))
BNAM_2012_8=BNAM_2012_8$BT
BNAM_2012_9=read.mat(paste0("data/BNAM/BNAM_output/",2012,"/","BT_",9,".mat"))
BNAM_2012_9=BNAM_2012_9$BT
BNAM_2012_10=read.mat(paste0("data/BNAM/BNAM_output/",2012,"/","BT_",10,".mat"))
BNAM_2012_10=BNAM_2012_10$BT
BNAM_2012_11=read.mat(paste0("data/BNAM/BNAM_output/",2012,"/","BT_",11,".mat"))
BNAM_2012_11=BNAM_2012_11$BT
BNAM_2012_12=read.mat(paste0("data/BNAM/BNAM_output/",2012,"/","BT_",12,".mat"))
BNAM_2012_12=BNAM_2012_12$BT

BNAM_2013_1=read.mat(paste0("data/BNAM/BNAM_output/",2013,"/","BT_",1,".mat"))
BNAM_2013_1=BNAM_2013_1$BT
BNAM_2013_2=read.mat(paste0("data/BNAM/BNAM_output/",2013,"/","BT_",2,".mat"))
BNAM_2013_2=BNAM_2013_2$BT
BNAM_2013_3=read.mat(paste0("data/BNAM/BNAM_output/",2013,"/","BT_",3,".mat"))
BNAM_2013_3=BNAM_2013_3$BT
BNAM_2013_4=read.mat(paste0("data/BNAM/BNAM_output/",2013,"/","BT_",4,".mat"))
BNAM_2013_4=BNAM_2013_4$BT
BNAM_2013_5=read.mat(paste0("data/BNAM/BNAM_output/",2013,"/","BT_",5,".mat"))
BNAM_2013_5=BNAM_2013_5$BT
BNAM_2013_6=read.mat(paste0("data/BNAM/BNAM_output/",2013,"/","BT_",6,".mat"))
BNAM_2013_6=BNAM_2013_6$BT
BNAM_2013_7=read.mat(paste0("data/BNAM/BNAM_output/",2013,"/","BT_",7,".mat"))
BNAM_2013_7=BNAM_2013_7$BT
BNAM_2013_8=read.mat(paste0("data/BNAM/BNAM_output/",2013,"/","BT_",8,".mat"))
BNAM_2013_8=BNAM_2013_8$BT
BNAM_2013_9=read.mat(paste0("data/BNAM/BNAM_output/",2013,"/","BT_",9,".mat"))
BNAM_2013_9=BNAM_2013_9$BT
BNAM_2013_10=read.mat(paste0("data/BNAM/BNAM_output/",2013,"/","BT_",10,".mat"))
BNAM_2013_10=BNAM_2013_10$BT
BNAM_2013_11=read.mat(paste0("data/BNAM/BNAM_output/",2013,"/","BT_",11,".mat"))
BNAM_2013_11=BNAM_2013_11$BT
BNAM_2013_12=read.mat(paste0("data/BNAM/BNAM_output/",2013,"/","BT_",12,".mat"))
BNAM_2013_12=BNAM_2013_12$BT

BNAM_2014_1=read.mat(paste0("data/BNAM/BNAM_output/",2014,"/","BT_",1,".mat"))
BNAM_2014_1=BNAM_2014_1$BT
BNAM_2014_2=read.mat(paste0("data/BNAM/BNAM_output/",2014,"/","BT_",2,".mat"))
BNAM_2014_2=BNAM_2014_2$BT
BNAM_2014_3=read.mat(paste0("data/BNAM/BNAM_output/",2014,"/","BT_",3,".mat"))
BNAM_2014_3=BNAM_2014_3$BT
BNAM_2014_4=read.mat(paste0("data/BNAM/BNAM_output/",2014,"/","BT_",4,".mat"))
BNAM_2014_4=BNAM_2014_4$BT
BNAM_2014_5=read.mat(paste0("data/BNAM/BNAM_output/",2014,"/","BT_",5,".mat"))
BNAM_2014_5=BNAM_2014_5$BT
BNAM_2014_6=read.mat(paste0("data/BNAM/BNAM_output/",2014,"/","BT_",6,".mat"))
BNAM_2014_6=BNAM_2014_6$BT
BNAM_2014_7=read.mat(paste0("data/BNAM/BNAM_output/",2014,"/","BT_",7,".mat"))
BNAM_2014_7=BNAM_2014_7$BT
BNAM_2014_8=read.mat(paste0("data/BNAM/BNAM_output/",2014,"/","BT_",8,".mat"))
BNAM_2014_8=BNAM_2014_8$BT
BNAM_2014_9=read.mat(paste0("data/BNAM/BNAM_output/",2014,"/","BT_",9,".mat"))
BNAM_2014_9=BNAM_2014_9$BT
BNAM_2014_10=read.mat(paste0("data/BNAM/BNAM_output/",2014,"/","BT_",10,".mat"))
BNAM_2014_10=BNAM_2014_10$BT
BNAM_2014_11=read.mat(paste0("data/BNAM/BNAM_output/",2014,"/","BT_",11,".mat"))
BNAM_2014_11=BNAM_2014_11$BT
BNAM_2014_12=read.mat(paste0("data/BNAM/BNAM_output/",2014,"/","BT_",12,".mat"))
BNAM_2014_12=BNAM_2014_12$BT

BNAM_2015_1=read.mat(paste0("data/BNAM/BNAM_output/",2015,"/","BT_",1,".mat"))
BNAM_2015_1=BNAM_2015_1$BT
BNAM_2015_2=read.mat(paste0("data/BNAM/BNAM_output/",2015,"/","BT_",2,".mat"))
BNAM_2015_2=BNAM_2015_2$BT
BNAM_2015_3=read.mat(paste0("data/BNAM/BNAM_output/",2015,"/","BT_",3,".mat"))
BNAM_2015_3=BNAM_2015_3$BT
BNAM_2015_4=read.mat(paste0("data/BNAM/BNAM_output/",2015,"/","BT_",4,".mat"))
BNAM_2015_4=BNAM_2015_4$BT
BNAM_2015_5=read.mat(paste0("data/BNAM/BNAM_output/",2015,"/","BT_",5,".mat"))
BNAM_2015_5=BNAM_2015_5$BT
BNAM_2015_6=read.mat(paste0("data/BNAM/BNAM_output/",2015,"/","BT_",6,".mat"))
BNAM_2015_6=BNAM_2015_6$BT
BNAM_2015_7=read.mat(paste0("data/BNAM/BNAM_output/",2015,"/","BT_",7,".mat"))
BNAM_2015_7=BNAM_2015_7$BT
BNAM_2015_8=read.mat(paste0("data/BNAM/BNAM_output/",2015,"/","BT_",8,".mat"))
BNAM_2015_8=BNAM_2015_8$BT
BNAM_2015_9=read.mat(paste0("data/BNAM/BNAM_output/",2015,"/","BT_",9,".mat"))
BNAM_2015_9=BNAM_2015_9$BT
BNAM_2015_10=read.mat(paste0("data/BNAM/BNAM_output/",2015,"/","BT_",10,".mat"))
BNAM_2015_10=BNAM_2015_10$BT
BNAM_2015_11=read.mat(paste0("data/BNAM/BNAM_output/",2015,"/","BT_",11,".mat"))
BNAM_2015_11=BNAM_2015_11$BT
BNAM_2015_12=read.mat(paste0("data/BNAM/BNAM_output/",2015,"/","BT_",12,".mat"))
BNAM_2015_12=BNAM_2015_12$BT

BNAM_2016_1=read.mat(paste0("data/BNAM/BNAM_output/",2016,"/","BT_",1,".mat"))
BNAM_2016_1=BNAM_2016_1$BT
BNAM_2016_2=read.mat(paste0("data/BNAM/BNAM_output/",2016,"/","BT_",2,".mat"))
BNAM_2016_2=BNAM_2016_2$BT
BNAM_2016_3=read.mat(paste0("data/BNAM/BNAM_output/",2016,"/","BT_",3,".mat"))
BNAM_2016_3=BNAM_2016_3$BT
BNAM_2016_4=read.mat(paste0("data/BNAM/BNAM_output/",2016,"/","BT_",4,".mat"))
BNAM_2016_4=BNAM_2016_4$BT
BNAM_2016_5=read.mat(paste0("data/BNAM/BNAM_output/",2016,"/","BT_",5,".mat"))
BNAM_2016_5=BNAM_2016_5$BT
BNAM_2016_6=read.mat(paste0("data/BNAM/BNAM_output/",2016,"/","BT_",6,".mat"))
BNAM_2016_6=BNAM_2016_6$BT
BNAM_2016_7=read.mat(paste0("data/BNAM/BNAM_output/",2016,"/","BT_",7,".mat"))
BNAM_2016_7=BNAM_2016_7$BT
BNAM_2016_8=read.mat(paste0("data/BNAM/BNAM_output/",2016,"/","BT_",8,".mat"))
BNAM_2016_8=BNAM_2016_8$BT
BNAM_2016_9=read.mat(paste0("data/BNAM/BNAM_output/",2016,"/","BT_",9,".mat"))
BNAM_2016_9=BNAM_2016_9$BT
BNAM_2016_10=read.mat(paste0("data/BNAM/BNAM_output/",2016,"/","BT_",10,".mat"))
BNAM_2016_10=BNAM_2016_10$BT
BNAM_2016_11=read.mat(paste0("data/BNAM/BNAM_output/",2016,"/","BT_",11,".mat"))
BNAM_2016_11=BNAM_2016_11$BT
BNAM_2016_12=read.mat(paste0("data/BNAM/BNAM_output/",2016,"/","BT_",12,".mat"))
BNAM_2016_12=BNAM_2016_12$BT

BNAM_2017_1=read.mat(paste0("data/BNAM/BNAM_output/",2017,"/","BT_",1,".mat"))
BNAM_2017_1=BNAM_2017_1$BT
BNAM_2017_2=read.mat(paste0("data/BNAM/BNAM_output/",2017,"/","BT_",2,".mat"))
BNAM_2017_2=BNAM_2017_2$BT
BNAM_2017_3=read.mat(paste0("data/BNAM/BNAM_output/",2017,"/","BT_",3,".mat"))
BNAM_2017_3=BNAM_2017_3$BT
BNAM_2017_4=read.mat(paste0("data/BNAM/BNAM_output/",2017,"/","BT_",4,".mat"))
BNAM_2017_4=BNAM_2017_4$BT
BNAM_2017_5=read.mat(paste0("data/BNAM/BNAM_output/",2017,"/","BT_",5,".mat"))
BNAM_2017_5=BNAM_2017_5$BT
BNAM_2017_6=read.mat(paste0("data/BNAM/BNAM_output/",2017,"/","BT_",6,".mat"))
BNAM_2017_6=BNAM_2017_6$BT
BNAM_2017_7=read.mat(paste0("data/BNAM/BNAM_output/",2017,"/","BT_",7,".mat"))
BNAM_2017_7=BNAM_2017_7$BT
BNAM_2017_8=read.mat(paste0("data/BNAM/BNAM_output/",2017,"/","BT_",8,".mat"))
BNAM_2017_8=BNAM_2017_8$BT
BNAM_2017_9=read.mat(paste0("data/BNAM/BNAM_output/",2017,"/","BT_",9,".mat"))
BNAM_2017_9=BNAM_2017_9$BT
BNAM_2017_10=read.mat(paste0("data/BNAM/BNAM_output/",2017,"/","BT_",10,".mat"))
BNAM_2017_10=BNAM_2017_10$BT
BNAM_2017_11=read.mat(paste0("data/BNAM/BNAM_output/",2017,"/","BT_",11,".mat"))
BNAM_2017_11=BNAM_2017_11$BT
BNAM_2017_12=read.mat(paste0("data/BNAM/BNAM_output/",2017,"/","BT_",12,".mat"))
BNAM_2017_12=BNAM_2017_12$BT

BNAM_2018_1=read.mat(paste0("data/BNAM/BNAM_output/",2018,"/","BT_",1,".mat"))
BNAM_2018_1=BNAM_2018_1$BT
BNAM_2018_2=read.mat(paste0("data/BNAM/BNAM_output/",2018,"/","BT_",2,".mat"))
BNAM_2018_2=BNAM_2018_2$BT
BNAM_2018_3=read.mat(paste0("data/BNAM/BNAM_output/",2018,"/","BT_",3,".mat"))
BNAM_2018_3=BNAM_2018_3$BT
BNAM_2018_4=read.mat(paste0("data/BNAM/BNAM_output/",2018,"/","BT_",4,".mat"))
BNAM_2018_4=BNAM_2018_4$BT
BNAM_2018_5=read.mat(paste0("data/BNAM/BNAM_output/",2018,"/","BT_",5,".mat"))
BNAM_2018_5=BNAM_2018_5$BT
BNAM_2018_6=read.mat(paste0("data/BNAM/BNAM_output/",2018,"/","BT_",6,".mat"))
BNAM_2018_6=BNAM_2018_6$BT
BNAM_2018_7=read.mat(paste0("data/BNAM/BNAM_output/",2018,"/","BT_",7,".mat"))
BNAM_2018_7=BNAM_2018_7$BT
BNAM_2018_8=read.mat(paste0("data/BNAM/BNAM_output/",2018,"/","BT_",8,".mat"))
BNAM_2018_8=BNAM_2018_8$BT
BNAM_2018_9=read.mat(paste0("data/BNAM/BNAM_output/",2018,"/","BT_",9,".mat"))
BNAM_2018_9=BNAM_2018_9$BT
BNAM_2018_10=read.mat(paste0("data/BNAM/BNAM_output/",2018,"/","BT_",10,".mat"))
BNAM_2018_10=BNAM_2018_10$BT
BNAM_2018_11=read.mat(paste0("data/BNAM/BNAM_output/",2018,"/","BT_",11,".mat"))
BNAM_2018_11=BNAM_2018_11$BT
BNAM_2018_12=read.mat(paste0("data/BNAM/BNAM_output/",2018,"/","BT_",12,".mat"))
BNAM_2018_12=BNAM_2018_12$BT

# converting into dataframes
BNAM_1995_1=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1995_1))
BNAM_1995_2=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1995_2))
BNAM_1995_3=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1995_3))
BNAM_1995_4=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1995_4))
BNAM_1995_5=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1995_5))
BNAM_1995_6=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1995_6))
BNAM_1995_7=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1995_7))
BNAM_1995_8=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1995_8))
BNAM_1995_9=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1995_9))
BNAM_1995_10=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1995_10))
BNAM_1995_11=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1995_11))
BNAM_1995_12=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1995_12))

BNAM_1996_1=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1996_1))
BNAM_1996_2=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1996_2))
BNAM_1996_3=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1996_3))
BNAM_1996_4=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1996_4))
BNAM_1996_5=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1996_5))
BNAM_1996_6=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1996_6))
BNAM_1996_7=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1996_7))
BNAM_1996_8=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1996_8))
BNAM_1996_9=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1996_9))
BNAM_1996_10=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1996_10))
BNAM_1996_11=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1996_11))
BNAM_1996_12=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1996_12))

BNAM_1997_1=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1997_1))
BNAM_1997_2=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1997_2))
BNAM_1997_3=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1997_3))
BNAM_1997_4=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1997_4))
BNAM_1997_5=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1997_5))
BNAM_1997_6=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1997_6))
BNAM_1997_7=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1997_7))
BNAM_1997_8=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1997_8))
BNAM_1997_9=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1997_9))
BNAM_1997_10=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1997_10))
BNAM_1997_11=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1997_11))
BNAM_1997_12=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1997_12))

BNAM_1998_1=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1998_1))
BNAM_1998_2=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1998_2))
BNAM_1998_3=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1998_3))
BNAM_1998_4=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1998_4))
BNAM_1998_5=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1998_5))
BNAM_1998_6=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1998_6))
BNAM_1998_7=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1998_7))
BNAM_1998_8=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1998_8))
BNAM_1998_9=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1998_9))
BNAM_1998_10=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1998_10))
BNAM_1998_11=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1998_11))
BNAM_1998_12=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1998_12))

BNAM_1999_1=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1999_1))
BNAM_1999_2=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1999_2))
BNAM_1999_3=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1999_3))
BNAM_1999_4=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1999_4))
BNAM_1999_5=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1999_5))
BNAM_1999_6=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1999_6))
BNAM_1999_7=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1999_7))
BNAM_1999_8=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1999_8))
BNAM_1999_9=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1999_9))
BNAM_1999_10=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1999_10))
BNAM_1999_11=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1999_11))
BNAM_1999_12=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1999_12))

BNAM_2000_1=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2000_1))
BNAM_2000_2=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2000_2))
BNAM_2000_3=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2000_3))
BNAM_2000_4=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2000_4))
BNAM_2000_5=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2000_5))
BNAM_2000_6=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2000_6))
BNAM_2000_7=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2000_7))
BNAM_2000_8=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2000_8))
BNAM_2000_9=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2000_9))
BNAM_2000_10=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2000_10))
BNAM_2000_11=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2000_11))
BNAM_2000_12=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2000_12))

BNAM_2001_1=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2001_1))
BNAM_2001_2=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2001_2))
BNAM_2001_3=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2001_3))
BNAM_2001_4=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2001_4))
BNAM_2001_5=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2001_5))
BNAM_2001_6=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2001_6))
BNAM_2001_7=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2001_7))
BNAM_2001_8=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2001_8))
BNAM_2001_9=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2001_9))
BNAM_2001_10=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2001_10))
BNAM_2001_11=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2001_11))
BNAM_2001_12=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2001_12))

BNAM_2002_1=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2002_1))
BNAM_2002_2=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2002_2))
BNAM_2002_3=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2002_3))
BNAM_2002_4=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2002_4))
BNAM_2002_5=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2002_5))
BNAM_2002_6=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2002_6))
BNAM_2002_7=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2002_7))
BNAM_2002_8=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2002_8))
BNAM_2002_9=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2002_9))
BNAM_2002_10=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2002_10))
BNAM_2002_11=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2002_11))
BNAM_2002_12=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2002_12))

BNAM_2003_1=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2003_1))
BNAM_2003_2=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2003_2))
BNAM_2003_3=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2003_3))
BNAM_2003_4=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2003_4))
BNAM_2003_5=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2003_5))
BNAM_2003_6=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2003_6))
BNAM_2003_7=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2003_7))
BNAM_2003_8=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2003_8))
BNAM_2003_9=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2003_9))
BNAM_2003_10=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2003_10))
BNAM_2003_11=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2003_11))
BNAM_2003_12=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2003_12))

BNAM_2004_1=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2004_1))
BNAM_2004_2=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2004_2))
BNAM_2004_3=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2004_3))
BNAM_2004_4=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2004_4))
BNAM_2004_5=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2004_5))
BNAM_2004_6=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2004_6))
BNAM_2004_7=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2004_7))
BNAM_2004_8=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2004_8))
BNAM_2004_9=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2004_9))
BNAM_2004_10=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2004_10))
BNAM_2004_11=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2004_11))
BNAM_2004_12=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2004_12))

BNAM_2005_1=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2005_1))
BNAM_2005_2=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2005_2))
BNAM_2005_3=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2005_3))
BNAM_2005_4=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2005_4))
BNAM_2005_5=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2005_5))
BNAM_2005_6=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2005_6))
BNAM_2005_7=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2005_7))
BNAM_2005_8=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2005_8))
BNAM_2005_9=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2005_9))
BNAM_2005_10=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2005_10))
BNAM_2005_11=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2005_11))
BNAM_2005_12=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2005_12))

BNAM_2006_1=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2006_1))
BNAM_2006_2=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2006_2))
BNAM_2006_3=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2006_3))
BNAM_2006_4=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2006_4))
BNAM_2006_5=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2006_5))
BNAM_2006_6=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2006_6))
BNAM_2006_7=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2006_7))
BNAM_2006_8=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2006_8))
BNAM_2006_9=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2006_9))
BNAM_2006_10=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2006_10))
BNAM_2006_11=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2006_11))
BNAM_2006_12=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2006_12))

BNAM_2007_1=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2007_1))
BNAM_2007_2=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2007_2))
BNAM_2007_3=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2007_3))
BNAM_2007_4=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2007_4))
BNAM_2007_5=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2007_5))
BNAM_2007_6=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2007_6))
BNAM_2007_7=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2007_7))
BNAM_2007_8=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2007_8))
BNAM_2007_9=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2007_9))
BNAM_2007_10=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2007_10))
BNAM_2007_11=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2007_11))
BNAM_2007_12=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2007_12))

BNAM_2008_1=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2008_1))
BNAM_2008_2=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2008_2))
BNAM_2008_3=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2008_3))
BNAM_2008_4=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2008_4))
BNAM_2008_5=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2008_5))
BNAM_2008_6=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2008_6))
BNAM_2008_7=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2008_7))
BNAM_2008_8=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2008_8))
BNAM_2008_9=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2008_9))
BNAM_2008_10=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2008_10))
BNAM_2008_11=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2008_11))
BNAM_2008_12=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2008_12))

BNAM_2009_1=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2009_1))
BNAM_2009_2=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2009_2))
BNAM_2009_3=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2009_3))
BNAM_2009_4=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2009_4))
BNAM_2009_5=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2009_5))
BNAM_2009_6=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2009_6))
BNAM_2009_7=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2009_7))
BNAM_2009_8=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2009_8))
BNAM_2009_9=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2009_9))
BNAM_2009_10=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2009_10))
BNAM_2009_11=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2009_11))
BNAM_2009_12=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2009_12))

BNAM_2010_1=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2010_1))
BNAM_2010_2=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2010_2))
BNAM_2010_3=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2010_3))
BNAM_2010_4=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2010_4))
BNAM_2010_5=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2010_5))
BNAM_2010_6=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2010_6))
BNAM_2010_7=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2010_7))
BNAM_2010_8=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2010_8))
BNAM_2010_9=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2010_9))
BNAM_2010_10=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2010_10))
BNAM_2010_11=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2010_11))
BNAM_2010_12=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2010_12))

BNAM_2011_1=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2011_1))
BNAM_2011_2=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2011_2))
BNAM_2011_3=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2011_3))
BNAM_2011_4=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2011_4))
BNAM_2011_5=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2011_5))
BNAM_2011_6=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2011_6))
BNAM_2011_7=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2011_7))
BNAM_2011_8=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2011_8))
BNAM_2011_9=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2011_9))
BNAM_2011_10=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2011_10))
BNAM_2011_11=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2011_11))
BNAM_2011_12=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2011_12))

BNAM_2012_1=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2012_1))
BNAM_2012_2=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2012_2))
BNAM_2012_3=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2012_3))
BNAM_2012_4=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2012_4))
BNAM_2012_5=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2012_5))
BNAM_2012_6=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2012_6))
BNAM_2012_7=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2012_7))
BNAM_2012_8=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2012_8))
BNAM_2012_9=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2012_9))
BNAM_2012_10=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2012_10))
BNAM_2012_11=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2012_11))
BNAM_2012_12=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2012_12))

BNAM_2013_1=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2013_1))
BNAM_2013_2=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2013_2))
BNAM_2013_3=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2013_3))
BNAM_2013_4=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2013_4))
BNAM_2013_5=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2013_5))
BNAM_2013_6=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2013_6))
BNAM_2013_7=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2013_7))
BNAM_2013_8=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2013_8))
BNAM_2013_9=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2013_9))
BNAM_2013_10=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2013_10))
BNAM_2013_11=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2013_11))
BNAM_2013_12=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2013_12))

BNAM_2014_1=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2014_1))
BNAM_2014_2=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2014_2))
BNAM_2014_3=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2014_3))
BNAM_2014_4=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2014_4))
BNAM_2014_5=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2014_5))
BNAM_2014_6=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2014_6))
BNAM_2014_7=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2014_7))
BNAM_2014_8=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2014_8))
BNAM_2014_9=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2014_9))
BNAM_2014_10=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2014_10))
BNAM_2014_11=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2014_11))
BNAM_2014_12=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2014_12))

BNAM_2015_1=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2015_1))
BNAM_2015_2=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2015_2))
BNAM_2015_3=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2015_3))
BNAM_2015_4=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2015_4))
BNAM_2015_5=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2015_5))
BNAM_2015_6=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2015_6))
BNAM_2015_7=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2015_7))
BNAM_2015_8=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2015_8))
BNAM_2015_9=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2015_9))
BNAM_2015_10=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2015_10))
BNAM_2015_11=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2015_11))
BNAM_2015_12=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2015_12))

BNAM_2016_1=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2016_1))
BNAM_2016_2=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2016_2))
BNAM_2016_3=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2016_3))
BNAM_2016_4=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2016_4))
BNAM_2016_5=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2016_5))
BNAM_2016_6=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2016_6))
BNAM_2016_7=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2016_7))
BNAM_2016_8=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2016_8))
BNAM_2016_9=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2016_9))
BNAM_2016_10=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2016_10))
BNAM_2016_11=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2016_11))
BNAM_2016_12=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2016_12))

BNAM_2017_1=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2017_1))
BNAM_2017_2=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2017_2))
BNAM_2017_3=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2017_3))
BNAM_2017_4=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2017_4))
BNAM_2017_5=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2017_5))
BNAM_2017_6=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2017_6))
BNAM_2017_7=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2017_7))
BNAM_2017_8=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2017_8))
BNAM_2017_9=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2017_9))
BNAM_2017_10=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2017_10))
BNAM_2017_11=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2017_11))
BNAM_2017_12=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2017_12))

BNAM_2018_1=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2018_1))
BNAM_2018_2=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2018_2))
BNAM_2018_3=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2018_3))
BNAM_2018_4=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2018_4))
BNAM_2018_5=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2018_5))
BNAM_2018_6=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2018_6))
BNAM_2018_7=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2018_7))
BNAM_2018_8=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2018_8))
BNAM_2018_9=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2018_9))
BNAM_2018_10=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2018_10))
BNAM_2018_11=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2018_11))
BNAM_2018_12=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2018_12))

latlong <- ("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")                         

## Converting into rasters
#1995_1
grid <- BNAM_1995_1%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1995_1%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1995_1[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1995_1=rasterFromXYZ(out,crs=latlong)
#1995_2
grid <- BNAM_1995_2%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1995_2%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1995_2[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1995_2=rasterFromXYZ(out,crs=latlong)
#1995_3
grid <- BNAM_1995_3%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1995_3%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1995_3[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1995_3=rasterFromXYZ(out,crs=latlong)
#1995_4
grid <- BNAM_1995_4%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1995_4%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1995_4[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1995_4=rasterFromXYZ(out,crs=latlong)
#1995_5
grid <- BNAM_1995_5%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1995_5%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1995_5[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1995_5=rasterFromXYZ(out,crs=latlong)
#1995_6
grid <- BNAM_1995_6%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1995_6%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1995_6[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1995_6=rasterFromXYZ(out,crs=latlong)
#1995_7
grid <- BNAM_1995_7%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1995_7%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1995_7[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1995_7=rasterFromXYZ(out,crs=latlong)
#1995_8
grid <- BNAM_1995_8%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1995_8%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1995_8[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1995_8=rasterFromXYZ(out,crs=latlong)
#1995_9
grid <- BNAM_1995_9%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1995_9%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1995_9[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1995_9=rasterFromXYZ(out,crs=latlong)
#1995_10
grid <- BNAM_1995_10%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1995_10%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1995_10[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1995_10=rasterFromXYZ(out,crs=latlong)
#1995_11
grid <- BNAM_1995_11%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1995_11%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1995_11[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1995_11=rasterFromXYZ(out,crs=latlong)
#1995_12
grid <- BNAM_1995_12%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1995_12%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1995_12[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1995_12=rasterFromXYZ(out,crs=latlong)

#1996_1
grid <- BNAM_1996_1%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1996_1%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1996_1[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1996_1=rasterFromXYZ(out,crs=latlong)
#1996_2
grid <- BNAM_1996_2%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1996_2%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1996_2[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1996_2=rasterFromXYZ(out,crs=latlong)
#1996_3
grid <- BNAM_1996_3%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1996_3%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1996_3[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1996_3=rasterFromXYZ(out,crs=latlong)
#1996_4
grid <- BNAM_1996_4%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1996_4%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1996_4[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1996_4=rasterFromXYZ(out,crs=latlong)
#1996_5
grid <- BNAM_1996_5%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1996_5%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1996_5[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1996_5=rasterFromXYZ(out,crs=latlong)
#1996_6
grid <- BNAM_1996_6%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1996_6%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1996_6[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1996_6=rasterFromXYZ(out,crs=latlong)
#1996_7
grid <- BNAM_1996_7%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1996_7%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1996_7[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1996_7=rasterFromXYZ(out,crs=latlong)
#1996_8
grid <- BNAM_1996_8%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1996_8%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1996_8[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1996_8=rasterFromXYZ(out,crs=latlong)
#1996_9
grid <- BNAM_1996_9%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1996_9%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1996_9[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1996_9=rasterFromXYZ(out,crs=latlong)
#1996_10
grid <- BNAM_1996_10%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1996_10%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1996_10[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1996_10=rasterFromXYZ(out,crs=latlong)
#1996_11
grid <- BNAM_1996_11%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1996_11%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1996_11[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1996_11=rasterFromXYZ(out,crs=latlong)
#1996_12
grid <- BNAM_1996_12%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1996_12%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1996_12[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1996_12=rasterFromXYZ(out,crs=latlong)

#1997_1
grid <- BNAM_1997_1%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1997_1%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1997_1[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1997_1=rasterFromXYZ(out,crs=latlong)
#1997_2
grid <- BNAM_1997_2%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1997_2%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1997_2[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1997_2=rasterFromXYZ(out,crs=latlong)
#1997_3
grid <- BNAM_1997_3%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1997_3%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1997_3[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1997_3=rasterFromXYZ(out,crs=latlong)
#1997_4
grid <- BNAM_1997_4%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1997_4%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1997_4[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1997_4=rasterFromXYZ(out,crs=latlong)
#1997_5
grid <- BNAM_1997_5%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1997_5%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1997_5[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1997_5=rasterFromXYZ(out,crs=latlong)
#1997_6
grid <- BNAM_1997_6%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1997_6%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1997_6[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1997_6=rasterFromXYZ(out,crs=latlong)
#1997_7
grid <- BNAM_1997_7%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1997_7%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1997_7[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1997_7=rasterFromXYZ(out,crs=latlong)
#1997_8
grid <- BNAM_1997_8%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1997_8%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1997_8[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1997_8=rasterFromXYZ(out,crs=latlong)
#1997_9
grid <- BNAM_1997_9%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1997_9%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1997_9[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1997_9=rasterFromXYZ(out,crs=latlong)
#1997_10
grid <- BNAM_1997_10%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1997_10%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1997_10[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1997_10=rasterFromXYZ(out,crs=latlong)
#1997_11
grid <- BNAM_1997_11%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1997_11%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1997_11[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1997_11=rasterFromXYZ(out,crs=latlong)
#1997_12
grid <- BNAM_1997_12%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1997_12%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1997_12[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1997_12=rasterFromXYZ(out,crs=latlong)

#1998_1
grid <- BNAM_1998_1%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1998_1%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1998_1[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1998_1=rasterFromXYZ(out,crs=latlong)
#1998_2
grid <- BNAM_1998_2%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1998_2%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1998_2[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1998_2=rasterFromXYZ(out,crs=latlong)
#1998_3
grid <- BNAM_1998_3%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1998_3%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1998_3[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1998_3=rasterFromXYZ(out,crs=latlong)
#1998_4
grid <- BNAM_1998_4%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1998_4%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1998_4[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1998_4=rasterFromXYZ(out,crs=latlong)
#1998_5
grid <- BNAM_1998_5%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1998_5%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1998_5[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1998_5=rasterFromXYZ(out,crs=latlong)
#1998_6
grid <- BNAM_1998_6%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1998_6%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1998_6[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1998_6=rasterFromXYZ(out,crs=latlong)
#1998_7
grid <- BNAM_1998_7%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1998_7%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1998_7[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1998_7=rasterFromXYZ(out,crs=latlong)
#1998_8
grid <- BNAM_1998_8%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1998_8%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1998_8[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1998_8=rasterFromXYZ(out,crs=latlong)
#1998_9
grid <- BNAM_1998_9%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1998_9%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1998_9[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1998_9=rasterFromXYZ(out,crs=latlong)
#1998_10
grid <- BNAM_1998_10%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1998_10%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1998_10[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1998_10=rasterFromXYZ(out,crs=latlong)
#1998_11
grid <- BNAM_1998_11%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1998_11%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1998_11[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1998_11=rasterFromXYZ(out,crs=latlong)
#1998_12
grid <- BNAM_1998_12%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1998_12%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1998_12[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1998_12=rasterFromXYZ(out,crs=latlong)

#1999_1
grid <- BNAM_1999_1%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1999_1%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1999_1[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1999_1=rasterFromXYZ(out,crs=latlong)
#1999_2
grid <- BNAM_1999_2%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1999_2%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1999_2[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1999_2=rasterFromXYZ(out,crs=latlong)
#1999_3
grid <- BNAM_1999_3%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1999_3%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1999_3[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1999_3=rasterFromXYZ(out,crs=latlong)
#1999_4
grid <- BNAM_1999_4%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1999_4%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1999_4[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1999_4=rasterFromXYZ(out,crs=latlong)
#1999_5
grid <- BNAM_1999_5%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1999_5%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1999_5[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1999_5=rasterFromXYZ(out,crs=latlong)
#1999_6
grid <- BNAM_1999_6%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1999_6%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1999_6[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1999_6=rasterFromXYZ(out,crs=latlong)
#1999_7
grid <- BNAM_1999_7%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1999_7%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1999_7[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1999_7=rasterFromXYZ(out,crs=latlong)
#1999_8
grid <- BNAM_1999_8%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1999_8%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1999_8[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1999_8=rasterFromXYZ(out,crs=latlong)
#1999_9
grid <- BNAM_1999_9%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1999_9%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1999_9[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1999_9=rasterFromXYZ(out,crs=latlong)
#1999_10
grid <- BNAM_1999_10%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1999_10%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1999_10[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1999_10=rasterFromXYZ(out,crs=latlong)
#1999_11
grid <- BNAM_1999_11%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1999_11%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1999_11[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1999_11=rasterFromXYZ(out,crs=latlong)
#1999_12
grid <- BNAM_1999_12%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_1999_12%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_1999_12[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_1999_12=rasterFromXYZ(out,crs=latlong)

#2000_1
grid <- BNAM_2000_1%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2000_1%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2000_1[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2000_1=rasterFromXYZ(out,crs=latlong)
#2000_2
grid <- BNAM_2000_2%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2000_2%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2000_2[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2000_2=rasterFromXYZ(out,crs=latlong)
#2000_3
grid <- BNAM_2000_3%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2000_3%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2000_3[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2000_3=rasterFromXYZ(out,crs=latlong)
#2000_4
grid <- BNAM_2000_4%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2000_4%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2000_4[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2000_4=rasterFromXYZ(out,crs=latlong)
#2000_5
grid <- BNAM_2000_5%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2000_5%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2000_5[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2000_5=rasterFromXYZ(out,crs=latlong)
#2000_6
grid <- BNAM_2000_6%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2000_6%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2000_6[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2000_6=rasterFromXYZ(out,crs=latlong)
#2000_7
grid <- BNAM_2000_7%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2000_7%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2000_7[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2000_7=rasterFromXYZ(out,crs=latlong)
#2000_8
grid <- BNAM_2000_8%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2000_8%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2000_8[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2000_8=rasterFromXYZ(out,crs=latlong)
#2000_9
grid <- BNAM_2000_9%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2000_9%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2000_9[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2000_9=rasterFromXYZ(out,crs=latlong)
#2000_10
grid <- BNAM_2000_10%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2000_10%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2000_10[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2000_10=rasterFromXYZ(out,crs=latlong)
#2000_11
grid <- BNAM_2000_11%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2000_11%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2000_11[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2000_11=rasterFromXYZ(out,crs=latlong)
#2000_12
grid <- BNAM_2000_12%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2000_12%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2000_12[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2000_12=rasterFromXYZ(out,crs=latlong)

#2001_1
grid <- BNAM_2001_1%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2001_1%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2001_1[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2001_1=rasterFromXYZ(out,crs=latlong)
#2001_2
grid <- BNAM_2001_2%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2001_2%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2001_2[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2001_2=rasterFromXYZ(out,crs=latlong)
#2001_3
grid <- BNAM_2001_3%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2001_3%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2001_3[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2001_3=rasterFromXYZ(out,crs=latlong)
#2001_4
grid <- BNAM_2001_4%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2001_4%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2001_4[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2001_4=rasterFromXYZ(out,crs=latlong)
#2001_5
grid <- BNAM_2001_5%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2001_5%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2001_5[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2001_5=rasterFromXYZ(out,crs=latlong)
#2001_6
grid <- BNAM_2001_6%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2001_6%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2001_6[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2001_6=rasterFromXYZ(out,crs=latlong)
#2001_7
grid <- BNAM_2001_7%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2001_7%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2001_7[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2001_7=rasterFromXYZ(out,crs=latlong)
#2001_8
grid <- BNAM_2001_8%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2001_8%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2001_8[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2001_8=rasterFromXYZ(out,crs=latlong)
#2001_9
grid <- BNAM_2001_9%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2001_9%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2001_9[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2001_9=rasterFromXYZ(out,crs=latlong)
#2001_10
grid <- BNAM_2001_10%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2001_10%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2001_10[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2001_10=rasterFromXYZ(out,crs=latlong)
#2001_11
grid <- BNAM_2001_11%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2001_11%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2001_11[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2001_11=rasterFromXYZ(out,crs=latlong)
#2001_12
grid <- BNAM_2001_12%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2001_12%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2001_12[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2001_12=rasterFromXYZ(out,crs=latlong)

#2002_1
grid <- BNAM_2002_1%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2002_1%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2002_1[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2002_1=rasterFromXYZ(out,crs=latlong)
#2002_2
grid <- BNAM_2002_2%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2002_2%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2002_2[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2002_2=rasterFromXYZ(out,crs=latlong)
#2002_3
grid <- BNAM_2002_3%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2002_3%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2002_3[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2002_3=rasterFromXYZ(out,crs=latlong)
#2002_4
grid <- BNAM_2002_4%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2002_4%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2002_4[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2002_4=rasterFromXYZ(out,crs=latlong)
#2002_5
grid <- BNAM_2002_5%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2002_5%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2002_5[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2002_5=rasterFromXYZ(out,crs=latlong)
#2002_6
grid <- BNAM_2002_6%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2002_6%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2002_6[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2002_6=rasterFromXYZ(out,crs=latlong)
#2002_7
grid <- BNAM_2002_7%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2002_7%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2002_7[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2002_7=rasterFromXYZ(out,crs=latlong)
#2002_8
grid <- BNAM_2002_8%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2002_8%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2002_8[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2002_8=rasterFromXYZ(out,crs=latlong)
#2002_9
grid <- BNAM_2002_9%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2002_9%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2002_9[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2002_9=rasterFromXYZ(out,crs=latlong)
#2002_10
grid <- BNAM_2002_10%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2002_10%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2002_10[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2002_10=rasterFromXYZ(out,crs=latlong)
#2002_11
grid <- BNAM_2002_11%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2002_11%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2002_11[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2002_11=rasterFromXYZ(out,crs=latlong)
#2002_12
grid <- BNAM_2002_12%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2002_12%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2002_12[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2002_12=rasterFromXYZ(out,crs=latlong)

#2003_1
grid <- BNAM_2003_1%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2003_1%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2003_1[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2003_1=rasterFromXYZ(out,crs=latlong)
#2003_2
grid <- BNAM_2003_2%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2003_2%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2003_2[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2003_2=rasterFromXYZ(out,crs=latlong)
#2003_3
grid <- BNAM_2003_3%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2003_3%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2003_3[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2003_3=rasterFromXYZ(out,crs=latlong)
#2003_4
grid <- BNAM_2003_4%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2003_4%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2003_4[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2003_4=rasterFromXYZ(out,crs=latlong)
#2003_5
grid <- BNAM_2003_5%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2003_5%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2003_5[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2003_5=rasterFromXYZ(out,crs=latlong)
#2003_6
grid <- BNAM_2003_6%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2003_6%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2003_6[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2003_6=rasterFromXYZ(out,crs=latlong)
#2003_7
grid <- BNAM_2003_7%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2003_7%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2003_7[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2003_7=rasterFromXYZ(out,crs=latlong)
#2003_8
grid <- BNAM_2003_8%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2003_8%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2003_8[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2003_8=rasterFromXYZ(out,crs=latlong)
#2003_9
grid <- BNAM_2003_9%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2003_9%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2003_9[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2003_9=rasterFromXYZ(out,crs=latlong)
#2003_10
grid <- BNAM_2003_10%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2003_10%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2003_10[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2003_10=rasterFromXYZ(out,crs=latlong)
#2003_11
grid <- BNAM_2003_11%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2003_11%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2003_11[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2003_11=rasterFromXYZ(out,crs=latlong)
#2003_12
grid <- BNAM_2003_12%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2003_12%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2003_12[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2003_12=rasterFromXYZ(out,crs=latlong)

#2004_1
grid <- BNAM_2004_1%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2004_1%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2004_1[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2004_1=rasterFromXYZ(out,crs=latlong)
#2004_2
grid <- BNAM_2004_2%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2004_2%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2004_2[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2004_2=rasterFromXYZ(out,crs=latlong)
#2004_3
grid <- BNAM_2004_3%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2004_3%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2004_3[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2004_3=rasterFromXYZ(out,crs=latlong)
#2004_4
grid <- BNAM_2004_4%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2004_4%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2004_4[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2004_4=rasterFromXYZ(out,crs=latlong)
#2004_5
grid <- BNAM_2004_5%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2004_5%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2004_5[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2004_5=rasterFromXYZ(out,crs=latlong)
#2004_6
grid <- BNAM_2004_6%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2004_6%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2004_6[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2004_6=rasterFromXYZ(out,crs=latlong)
#2004_7
grid <- BNAM_2004_7%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2004_7%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2004_7[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2004_7=rasterFromXYZ(out,crs=latlong)
#2004_8
grid <- BNAM_2004_8%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2004_8%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2004_8[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2004_8=rasterFromXYZ(out,crs=latlong)
#2004_9
grid <- BNAM_2004_9%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2004_9%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2004_9[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2004_9=rasterFromXYZ(out,crs=latlong)
#2004_10
grid <- BNAM_2004_10%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2004_10%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2004_10[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2004_10=rasterFromXYZ(out,crs=latlong)
#2004_11
grid <- BNAM_2004_11%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2004_11%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2004_11[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2004_11=rasterFromXYZ(out,crs=latlong)
#2004_12
grid <- BNAM_2004_12%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2004_12%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2004_12[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2004_12=rasterFromXYZ(out,crs=latlong)

#2005_1
grid <- BNAM_2005_1%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2005_1%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2005_1[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2005_1=rasterFromXYZ(out,crs=latlong)
#2005_2
grid <- BNAM_2005_2%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2005_2%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2005_2[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2005_2=rasterFromXYZ(out,crs=latlong)
#2005_3
grid <- BNAM_2005_3%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2005_3%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2005_3[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2005_3=rasterFromXYZ(out,crs=latlong)
#2005_4
grid <- BNAM_2005_4%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2005_4%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2005_4[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2005_4=rasterFromXYZ(out,crs=latlong)
#2005_5
grid <- BNAM_2005_5%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2005_5%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2005_5[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2005_5=rasterFromXYZ(out,crs=latlong)
#2005_6
grid <- BNAM_2005_6%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2005_6%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2005_6[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2005_6=rasterFromXYZ(out,crs=latlong)
#2005_7
grid <- BNAM_2005_7%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2005_7%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2005_7[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2005_7=rasterFromXYZ(out,crs=latlong)
#2005_8
grid <- BNAM_2005_8%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2005_8%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2005_8[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2005_8=rasterFromXYZ(out,crs=latlong)
#2005_9
grid <- BNAM_2005_9%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2005_9%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2005_9[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2005_9=rasterFromXYZ(out,crs=latlong)
#2005_10
grid <- BNAM_2005_10%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2005_10%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2005_10[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2005_10=rasterFromXYZ(out,crs=latlong)
#2005_11
grid <- BNAM_2005_11%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2005_11%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2005_11[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2005_11=rasterFromXYZ(out,crs=latlong)
#2005_12
grid <- BNAM_2005_12%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2005_12%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2005_12[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2005_12=rasterFromXYZ(out,crs=latlong)

#2006_1
grid <- BNAM_2006_1%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2006_1%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2006_1[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2006_1=rasterFromXYZ(out,crs=latlong)
#2006_2
grid <- BNAM_2006_2%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2006_2%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2006_2[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2006_2=rasterFromXYZ(out,crs=latlong)
#2006_3
grid <- BNAM_2006_3%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2006_3%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2006_3[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2006_3=rasterFromXYZ(out,crs=latlong)
#2006_4
grid <- BNAM_2006_4%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2006_4%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2006_4[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2006_4=rasterFromXYZ(out,crs=latlong)
#2006_5
grid <- BNAM_2006_5%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2006_5%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2006_5[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2006_5=rasterFromXYZ(out,crs=latlong)
#2006_6
grid <- BNAM_2006_6%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2006_6%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2006_6[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2006_6=rasterFromXYZ(out,crs=latlong)
#2006_7
grid <- BNAM_2006_7%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2006_7%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2006_7[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2006_7=rasterFromXYZ(out,crs=latlong)
#2006_8
grid <- BNAM_2006_8%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2006_8%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2006_8[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2006_8=rasterFromXYZ(out,crs=latlong)
#2006_9
grid <- BNAM_2006_9%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2006_9%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2006_9[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2006_9=rasterFromXYZ(out,crs=latlong)
#2006_10
grid <- BNAM_2006_10%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2006_10%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2006_10[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2006_10=rasterFromXYZ(out,crs=latlong)
#2006_11
grid <- BNAM_2006_11%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2006_11%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2006_11[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2006_11=rasterFromXYZ(out,crs=latlong)
#2006_12
grid <- BNAM_2006_12%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2006_12%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2006_12[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2006_12=rasterFromXYZ(out,crs=latlong)

#2007_1
grid <- BNAM_2007_1%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2007_1%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2007_1[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2007_1=rasterFromXYZ(out,crs=latlong)
#2007_2
grid <- BNAM_2007_2%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2007_2%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2007_2[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2007_2=rasterFromXYZ(out,crs=latlong)
#2007_3
grid <- BNAM_2007_3%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2007_3%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2007_3[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2007_3=rasterFromXYZ(out,crs=latlong)
#2007_4
grid <- BNAM_2007_4%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2007_4%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2007_4[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2007_4=rasterFromXYZ(out,crs=latlong)
#2007_5
grid <- BNAM_2007_5%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2007_5%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2007_5[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2007_5=rasterFromXYZ(out,crs=latlong)
#2007_6
grid <- BNAM_2007_6%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2007_6%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2007_6[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2007_6=rasterFromXYZ(out,crs=latlong)
#2007_7
grid <- BNAM_2007_7%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2007_7%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2007_7[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2007_7=rasterFromXYZ(out,crs=latlong)
#2007_8
grid <- BNAM_2007_8%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2007_8%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2007_8[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2007_8=rasterFromXYZ(out,crs=latlong)
#2007_9
grid <- BNAM_2007_9%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2007_9%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2007_9[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2007_9=rasterFromXYZ(out,crs=latlong)
#2007_10
grid <- BNAM_2007_10%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2007_10%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2007_10[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2007_10=rasterFromXYZ(out,crs=latlong)
#2007_11
grid <- BNAM_2007_11%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2007_11%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2007_11[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2007_11=rasterFromXYZ(out,crs=latlong)
#2007_12
grid <- BNAM_2007_12%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2007_12%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2007_12[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2007_12=rasterFromXYZ(out,crs=latlong)

#2008_1
grid <- BNAM_2008_1%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2008_1%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2008_1[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2008_1=rasterFromXYZ(out,crs=latlong)
#2008_2
grid <- BNAM_2008_2%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2008_2%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2008_2[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2008_2=rasterFromXYZ(out,crs=latlong)
#2008_3
grid <- BNAM_2008_3%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2008_3%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2008_3[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2008_3=rasterFromXYZ(out,crs=latlong)
#2008_4
grid <- BNAM_2008_4%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2008_4%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2008_4[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2008_4=rasterFromXYZ(out,crs=latlong)
#2008_5
grid <- BNAM_2008_5%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2008_5%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2008_5[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2008_5=rasterFromXYZ(out,crs=latlong)
#2008_6
grid <- BNAM_2008_6%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2008_6%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2008_6[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2008_6=rasterFromXYZ(out,crs=latlong)
#2008_7
grid <- BNAM_2008_7%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2008_7%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2008_7[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2008_7=rasterFromXYZ(out,crs=latlong)
#2008_8
grid <- BNAM_2008_8%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2008_8%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2008_8[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2008_8=rasterFromXYZ(out,crs=latlong)
#2008_9
grid <- BNAM_2008_9%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2008_9%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2008_9[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2008_9=rasterFromXYZ(out,crs=latlong)
#2008_10
grid <- BNAM_2008_10%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2008_10%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2008_10[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2008_10=rasterFromXYZ(out,crs=latlong)
#2008_11
grid <- BNAM_2008_11%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2008_11%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2008_11[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2008_11=rasterFromXYZ(out,crs=latlong)
#2008_12
grid <- BNAM_2008_12%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2008_12%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2008_12[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2008_12=rasterFromXYZ(out,crs=latlong)

#2009_1
grid <- BNAM_2009_1%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2009_1%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2009_1[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2009_1=rasterFromXYZ(out,crs=latlong)
#2009_2
grid <- BNAM_2009_2%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2009_2%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2009_2[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2009_2=rasterFromXYZ(out,crs=latlong)
#2009_3
grid <- BNAM_2009_3%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2009_3%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2009_3[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2009_3=rasterFromXYZ(out,crs=latlong)
#2009_4
grid <- BNAM_2009_4%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2009_4%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2009_4[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2009_4=rasterFromXYZ(out,crs=latlong)
#2009_5
grid <- BNAM_2009_5%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2009_5%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2009_5[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2009_5=rasterFromXYZ(out,crs=latlong)
#2009_6
grid <- BNAM_2009_6%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2009_6%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2009_6[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2009_6=rasterFromXYZ(out,crs=latlong)
#2009_7
grid <- BNAM_2009_7%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2009_7%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2009_7[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2009_7=rasterFromXYZ(out,crs=latlong)
#2009_8
grid <- BNAM_2009_8%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2009_8%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2009_8[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2009_8=rasterFromXYZ(out,crs=latlong)
#2009_9
grid <- BNAM_2009_9%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2009_9%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2009_9[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2009_9=rasterFromXYZ(out,crs=latlong)
#2009_10
grid <- BNAM_2009_10%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2009_10%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2009_10[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2009_10=rasterFromXYZ(out,crs=latlong)
#2009_11
grid <- BNAM_2009_11%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2009_11%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2009_11[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2009_11=rasterFromXYZ(out,crs=latlong)
#2009_12
grid <- BNAM_2009_12%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2009_12%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2009_12[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2009_12=rasterFromXYZ(out,crs=latlong)

#2010_1
grid <- BNAM_2010_1%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2010_1%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2010_1[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2010_1=rasterFromXYZ(out,crs=latlong)
#2010_2
grid <- BNAM_2010_2%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2010_2%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2010_2[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2010_2=rasterFromXYZ(out,crs=latlong)
#2010_3
grid <- BNAM_2010_3%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2010_3%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2010_3[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2010_3=rasterFromXYZ(out,crs=latlong)
#2010_4
grid <- BNAM_2010_4%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2010_4%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2010_4[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2010_4=rasterFromXYZ(out,crs=latlong)
#2010_5
grid <- BNAM_2010_5%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2010_5%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2010_5[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2010_5=rasterFromXYZ(out,crs=latlong)
#2010_6
grid <- BNAM_2010_6%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2010_6%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2010_6[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2010_6=rasterFromXYZ(out,crs=latlong)
#2010_7
grid <- BNAM_2010_7%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2010_7%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2010_7[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2010_7=rasterFromXYZ(out,crs=latlong)
#2010_8
grid <- BNAM_2010_8%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2010_8%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2010_8[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2010_8=rasterFromXYZ(out,crs=latlong)
#2010_9
grid <- BNAM_2010_9%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2010_9%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2010_9[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2010_9=rasterFromXYZ(out,crs=latlong)
#2010_10
grid <- BNAM_2010_10%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2010_10%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2010_10[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2010_10=rasterFromXYZ(out,crs=latlong)
#2010_11
grid <- BNAM_2010_11%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2010_11%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2010_11[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2010_11=rasterFromXYZ(out,crs=latlong)
#2010_12
grid <- BNAM_2010_12%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2010_12%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2010_12[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2010_12=rasterFromXYZ(out,crs=latlong)

#2011_1
grid <- BNAM_2011_1%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2011_1%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2011_1[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2011_1=rasterFromXYZ(out,crs=latlong)
#2011_2
grid <- BNAM_2011_2%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2011_2%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2011_2[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2011_2=rasterFromXYZ(out,crs=latlong)
#2011_3
grid <- BNAM_2011_3%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2011_3%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2011_3[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2011_3=rasterFromXYZ(out,crs=latlong)
#2011_4
grid <- BNAM_2011_4%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2011_4%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2011_4[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2011_4=rasterFromXYZ(out,crs=latlong)
#2011_5
grid <- BNAM_2011_5%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2011_5%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2011_5[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2011_5=rasterFromXYZ(out,crs=latlong)
#2011_6
grid <- BNAM_2011_6%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2011_6%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2011_6[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2011_6=rasterFromXYZ(out,crs=latlong)
#2011_7
grid <- BNAM_2011_7%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2011_7%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2011_7[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2011_7=rasterFromXYZ(out,crs=latlong)
#2011_8
grid <- BNAM_2011_8%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2011_8%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2011_8[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2011_8=rasterFromXYZ(out,crs=latlong)
#2011_9
grid <- BNAM_2011_9%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2011_9%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2011_9[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2011_9=rasterFromXYZ(out,crs=latlong)
#2011_10
grid <- BNAM_2011_10%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2011_10%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2011_10[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2011_10=rasterFromXYZ(out,crs=latlong)
#2011_11
grid <- BNAM_2011_11%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2011_11%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2011_11[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2011_11=rasterFromXYZ(out,crs=latlong)
#2011_12
grid <- BNAM_2011_12%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2011_12%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2011_12[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2011_12=rasterFromXYZ(out,crs=latlong)

#2012_1
grid <- BNAM_2012_1%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2012_1%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2012_1[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2012_1=rasterFromXYZ(out,crs=latlong)
#2012_2
grid <- BNAM_2012_2%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2012_2%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2012_2[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2012_2=rasterFromXYZ(out,crs=latlong)
#2012_3
grid <- BNAM_2012_3%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2012_3%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2012_3[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2012_3=rasterFromXYZ(out,crs=latlong)
#2012_4
grid <- BNAM_2012_4%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2012_4%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2012_4[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2012_4=rasterFromXYZ(out,crs=latlong)
#2012_5
grid <- BNAM_2012_5%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2012_5%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2012_5[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2012_5=rasterFromXYZ(out,crs=latlong)
#2012_6
grid <- BNAM_2012_6%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2012_6%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2012_6[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2012_6=rasterFromXYZ(out,crs=latlong)
#2012_7
grid <- BNAM_2012_7%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2012_7%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2012_7[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2012_7=rasterFromXYZ(out,crs=latlong)
#2012_8
grid <- BNAM_2012_8%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2012_8%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2012_8[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2012_8=rasterFromXYZ(out,crs=latlong)
#2012_9
grid <- BNAM_2012_9%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2012_9%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2012_9[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2012_9=rasterFromXYZ(out,crs=latlong)
#2012_10
grid <- BNAM_2012_10%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2012_10%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2012_10[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2012_10=rasterFromXYZ(out,crs=latlong)
#2012_11
grid <- BNAM_2012_11%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2012_11%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2012_11[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2012_11=rasterFromXYZ(out,crs=latlong)
#2012_12
grid <- BNAM_2012_12%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2012_12%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2012_12[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2012_12=rasterFromXYZ(out,crs=latlong)

#2013_1
grid <- BNAM_2013_1%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2013_1%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2013_1[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2013_1=rasterFromXYZ(out,crs=latlong)
#2013_2
grid <- BNAM_2013_2%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2013_2%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2013_2[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2013_2=rasterFromXYZ(out,crs=latlong)
#2013_3
grid <- BNAM_2013_3%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2013_3%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2013_3[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2013_3=rasterFromXYZ(out,crs=latlong)
#2013_4
grid <- BNAM_2013_4%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2013_4%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2013_4[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2013_4=rasterFromXYZ(out,crs=latlong)
#2013_5
grid <- BNAM_2013_5%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2013_5%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2013_5[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2013_5=rasterFromXYZ(out,crs=latlong)
#2013_6
grid <- BNAM_2013_6%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2013_6%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2013_6[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2013_6=rasterFromXYZ(out,crs=latlong)
#2013_7
grid <- BNAM_2013_7%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2013_7%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2013_7[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2013_7=rasterFromXYZ(out,crs=latlong)
#2013_8
grid <- BNAM_2013_8%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2013_8%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2013_8[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2013_8=rasterFromXYZ(out,crs=latlong)
#2013_9
grid <- BNAM_2013_9%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2013_9%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2013_9[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2013_9=rasterFromXYZ(out,crs=latlong)
#2013_10
grid <- BNAM_2013_10%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2013_10%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2013_10[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2013_10=rasterFromXYZ(out,crs=latlong)
#2013_11
grid <- BNAM_2013_11%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2013_11%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2013_11[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2013_11=rasterFromXYZ(out,crs=latlong)
#2013_12
grid <- BNAM_2013_12%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2013_12%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2013_12[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2013_12=rasterFromXYZ(out,crs=latlong)

#2014_1
grid <- BNAM_2014_1%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2014_1%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2014_1[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2014_1=rasterFromXYZ(out,crs=latlong)
#2014_2
grid <- BNAM_2014_2%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2014_2%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2014_2[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2014_2=rasterFromXYZ(out,crs=latlong)
#2014_3
grid <- BNAM_2014_3%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2014_3%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2014_3[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2014_3=rasterFromXYZ(out,crs=latlong)
#2014_4
grid <- BNAM_2014_4%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2014_4%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2014_4[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2014_4=rasterFromXYZ(out,crs=latlong)
#2014_5
grid <- BNAM_2014_5%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2014_5%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2014_5[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2014_5=rasterFromXYZ(out,crs=latlong)
#2014_6
grid <- BNAM_2014_6%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2014_6%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2014_6[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2014_6=rasterFromXYZ(out,crs=latlong)
#2014_7
grid <- BNAM_2014_7%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2014_7%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2014_7[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2014_7=rasterFromXYZ(out,crs=latlong)
#2014_8
grid <- BNAM_2014_8%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2014_8%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2014_8[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2014_8=rasterFromXYZ(out,crs=latlong)
#2014_9
grid <- BNAM_2014_9%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2014_9%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2014_9[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2014_9=rasterFromXYZ(out,crs=latlong)
#2014_10
grid <- BNAM_2014_10%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2014_10%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2014_10[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2014_10=rasterFromXYZ(out,crs=latlong)
#2014_11
grid <- BNAM_2014_11%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2014_11%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2014_11[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2014_11=rasterFromXYZ(out,crs=latlong)
#2014_12
grid <- BNAM_2014_12%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2014_12%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2014_12[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2014_12=rasterFromXYZ(out,crs=latlong)

#2015_1
grid <- BNAM_2015_1%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2015_1%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2015_1[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2015_1=rasterFromXYZ(out,crs=latlong)
#2015_2
grid <- BNAM_2015_2%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2015_2%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2015_2[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2015_2=rasterFromXYZ(out,crs=latlong)
#2015_3
grid <- BNAM_2015_3%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2015_3%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2015_3[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2015_3=rasterFromXYZ(out,crs=latlong)
#2015_4
grid <- BNAM_2015_4%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2015_4%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2015_4[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2015_4=rasterFromXYZ(out,crs=latlong)
#2015_5
grid <- BNAM_2015_5%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2015_5%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2015_5[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2015_5=rasterFromXYZ(out,crs=latlong)
#2015_6
grid <- BNAM_2015_6%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2015_6%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2015_6[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2015_6=rasterFromXYZ(out,crs=latlong)
#2015_7
grid <- BNAM_2015_7%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2015_7%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2015_7[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2015_7=rasterFromXYZ(out,crs=latlong)
#2015_8
grid <- BNAM_2015_8%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2015_8%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2015_8[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2015_8=rasterFromXYZ(out,crs=latlong)
#2015_9
grid <- BNAM_2015_9%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2015_9%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2015_9[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2015_9=rasterFromXYZ(out,crs=latlong)
#2015_10
grid <- BNAM_2015_10%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2015_10%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2015_10[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2015_10=rasterFromXYZ(out,crs=latlong)
#2015_11
grid <- BNAM_2015_11%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2015_11%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2015_11[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2015_11=rasterFromXYZ(out,crs=latlong)
#2015_12
grid <- BNAM_2015_12%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2015_12%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2015_12[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2015_12=rasterFromXYZ(out,crs=latlong)

#2016_1
grid <- BNAM_2016_1%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2016_1%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2016_1[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2016_1=rasterFromXYZ(out,crs=latlong)
#2016_2
grid <- BNAM_2016_2%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2016_2%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2016_2[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2016_2=rasterFromXYZ(out,crs=latlong)
#2016_3
grid <- BNAM_2016_3%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2016_3%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2016_3[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2016_3=rasterFromXYZ(out,crs=latlong)
#2016_4
grid <- BNAM_2016_4%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2016_4%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2016_4[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2016_4=rasterFromXYZ(out,crs=latlong)
#2016_5
grid <- BNAM_2016_5%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2016_5%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2016_5[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2016_5=rasterFromXYZ(out,crs=latlong)
#2016_6
grid <- BNAM_2016_6%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2016_6%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2016_6[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2016_6=rasterFromXYZ(out,crs=latlong)
#2016_7
grid <- BNAM_2016_7%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2016_7%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2016_7[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2016_7=rasterFromXYZ(out,crs=latlong)
#2016_8
grid <- BNAM_2016_8%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2016_8%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2016_8[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2016_8=rasterFromXYZ(out,crs=latlong)
#2016_9
grid <- BNAM_2016_9%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2016_9%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2016_9[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2016_9=rasterFromXYZ(out,crs=latlong)
#2016_10
grid <- BNAM_2016_10%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2016_10%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2016_10[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2016_10=rasterFromXYZ(out,crs=latlong)
#2016_11
grid <- BNAM_2016_11%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2016_11%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2016_11[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2016_11=rasterFromXYZ(out,crs=latlong)
#2016_12
grid <- BNAM_2016_12%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2016_12%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2016_12[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2016_12=rasterFromXYZ(out,crs=latlong)

#2017_1
grid <- BNAM_2017_1%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2017_1%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2017_1[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2017_1=rasterFromXYZ(out,crs=latlong)
#2017_2
grid <- BNAM_2017_2%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2017_2%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2017_2[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2017_2=rasterFromXYZ(out,crs=latlong)
#2017_3
grid <- BNAM_2017_3%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2017_3%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2017_3[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2017_3=rasterFromXYZ(out,crs=latlong)
#2017_4
grid <- BNAM_2017_4%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2017_4%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2017_4[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2017_4=rasterFromXYZ(out,crs=latlong)
#2017_5
grid <- BNAM_2017_5%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2017_5%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2017_5[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2017_5=rasterFromXYZ(out,crs=latlong)
#2017_6
grid <- BNAM_2017_6%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2017_6%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2017_6[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2017_6=rasterFromXYZ(out,crs=latlong)
#2017_7
grid <- BNAM_2017_7%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2017_7%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2017_7[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2017_7=rasterFromXYZ(out,crs=latlong)
#2017_8
grid <- BNAM_2017_8%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2017_8%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2017_8[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2017_8=rasterFromXYZ(out,crs=latlong)
#2017_9
grid <- BNAM_2017_9%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2017_9%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2017_9[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2017_9=rasterFromXYZ(out,crs=latlong)
#2017_10
grid <- BNAM_2017_10%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2017_10%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2017_10[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2017_10=rasterFromXYZ(out,crs=latlong)
#2017_11
grid <- BNAM_2017_11%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2017_11%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2017_11[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2017_11=rasterFromXYZ(out,crs=latlong)
#2017_12
grid <- BNAM_2017_12%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2017_12%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2017_12[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2017_12=rasterFromXYZ(out,crs=latlong)

#2018_1
grid <- BNAM_2018_1%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2018_1%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2018_1[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2018_1=rasterFromXYZ(out,crs=latlong)
#2018_2
grid <- BNAM_2018_2%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2018_2%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2018_2[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2018_2=rasterFromXYZ(out,crs=latlong)
#2018_3
grid <- BNAM_2018_3%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2018_3%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2018_3[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2018_3=rasterFromXYZ(out,crs=latlong)
#2018_4
grid <- BNAM_2018_4%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2018_4%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2018_4[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2018_4=rasterFromXYZ(out,crs=latlong)
#2018_5
grid <- BNAM_2018_5%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2018_5%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2018_5[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2018_5=rasterFromXYZ(out,crs=latlong)
#2018_6
grid <- BNAM_2018_6%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2018_6%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2018_6[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2018_6=rasterFromXYZ(out,crs=latlong)
#2018_7
grid <- BNAM_2018_7%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2018_7%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2018_7[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2018_7=rasterFromXYZ(out,crs=latlong)
#2018_8
grid <- BNAM_2018_8%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2018_8%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2018_8[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2018_8=rasterFromXYZ(out,crs=latlong)
#2018_9
grid <- BNAM_2018_9%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2018_9%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2018_9[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2018_9=rasterFromXYZ(out,crs=latlong)
#2018_10
grid <- BNAM_2018_10%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2018_10%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2018_10[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2018_10=rasterFromXYZ(out,crs=latlong)
#2018_11
grid <- BNAM_2018_11%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2018_11%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2018_11[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2018_11=rasterFromXYZ(out,crs=latlong)
#2018_12
grid <- BNAM_2018_12%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- BNAM_2018_12%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=BNAM_2018_12[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
BNAM_2018_12=rasterFromXYZ(out,crs=latlong)

# Create raster stacks (Summer only)
BNAM_1995<-stack(BNAM_1995_6,BNAM_1995_7,BNAM_1995_8)
BNAM_1996<-stack(BNAM_1996_6,BNAM_1996_7,BNAM_1996_8)
BNAM_1997<-stack(BNAM_1997_6,BNAM_1997_7,BNAM_1997_8)
BNAM_1998<-stack(BNAM_1998_6,BNAM_1998_7,BNAM_1998_8)
BNAM_1999<-stack(BNAM_1999_6,BNAM_1999_7,BNAM_1999_8)
BNAM_2000<-stack(BNAM_2000_6,BNAM_2000_7,BNAM_2000_8)
BNAM_2001<-stack(BNAM_2001_6,BNAM_2001_7,BNAM_2001_8)
BNAM_2002<-stack(BNAM_2002_6,BNAM_2002_7,BNAM_2002_8)
BNAM_2003<-stack(BNAM_2003_6,BNAM_2003_7,BNAM_2003_8)
BNAM_2004<-stack(BNAM_2004_6,BNAM_2004_7,BNAM_2004_8)
BNAM_2005<-stack(BNAM_2005_6,BNAM_2005_7,BNAM_2005_8)
BNAM_2006<-stack(BNAM_2006_6,BNAM_2006_7,BNAM_2006_8)
BNAM_2007<-stack(BNAM_2007_6,BNAM_2007_7,BNAM_2007_8)
BNAM_2008<-stack(BNAM_2008_6,BNAM_2008_7,BNAM_2008_8)
BNAM_2009<-stack(BNAM_2009_6,BNAM_2009_7,BNAM_2009_8)
BNAM_2010<-stack(BNAM_2010_6,BNAM_2010_7,BNAM_2010_8)
BNAM_2011<-stack(BNAM_2011_6,BNAM_2011_7,BNAM_2011_8)
BNAM_2012<-stack(BNAM_2012_6,BNAM_2012_7,BNAM_2012_8)
BNAM_2013<-stack(BNAM_2013_6,BNAM_2013_7,BNAM_2013_8)
BNAM_2014<-stack(BNAM_2014_6,BNAM_2014_7,BNAM_2014_8)
BNAM_2015<-stack(BNAM_2015_6,BNAM_2015_7,BNAM_2015_8)
BNAM_2016<-stack(BNAM_2016_6,BNAM_2016_7,BNAM_2016_8)
BNAM_2017<-stack(BNAM_2017_6,BNAM_2017_7,BNAM_2017_8)
BNAM_2018<-stack(BNAM_2018_6,BNAM_2018_7,BNAM_2018_8)

#Calculate mean
BNAM_1995_mean<-calc(BNAM_1995, mean)
BNAM_1996_mean<-calc(BNAM_1996, mean)
BNAM_1997_mean<-calc(BNAM_1997, mean)
BNAM_1998_mean<-calc(BNAM_1998, mean)
BNAM_1999_mean<-calc(BNAM_1999, mean)
BNAM_2000_mean<-calc(BNAM_2000, mean)
BNAM_2001_mean<-calc(BNAM_2001, mean)
BNAM_2002_mean<-calc(BNAM_2002, mean)
BNAM_2003_mean<-calc(BNAM_2003, mean)
BNAM_2004_mean<-calc(BNAM_2004, mean)
BNAM_2005_mean<-calc(BNAM_2005, mean)
BNAM_2006_mean<-calc(BNAM_2006, mean)
BNAM_2007_mean<-calc(BNAM_2007, mean)
BNAM_2008_mean<-calc(BNAM_2008, mean)
BNAM_2009_mean<-calc(BNAM_2009, mean)
BNAM_2010_mean<-calc(BNAM_2010, mean)
BNAM_2011_mean<-calc(BNAM_2011, mean)
BNAM_2012_mean<-calc(BNAM_2012, mean)
BNAM_2013_mean<-calc(BNAM_2013, mean)
BNAM_2014_mean<-calc(BNAM_2014, mean)
BNAM_2015_mean<-calc(BNAM_2015, mean)
BNAM_2016_mean<-calc(BNAM_2016, mean)
BNAM_2017_mean<-calc(BNAM_2017, mean)
BNAM_2018_mean<-calc(BNAM_2018, mean)

# Remove unnecessary objects
rm(list=setdiff(ls(), c("BNAM_1995_mean","BNAM_1996_mean","BNAM_1997_mean","BNAM_1998_mean","BNAM_1999_mean",
                        "BNAM_2000_mean","BNAM_2001_mean","BNAM_2002_mean","BNAM_2003_mean","BNAM_2004_mean",
                        "BNAM_2005_mean","BNAM_2006_mean","BNAM_2007_mean","BNAM_2008_mean","BNAM_2009_mean",
                        "BNAM_2010_mean","BNAM_2011_mean","BNAM_2012_mean","BNAM_2013_mean","BNAM_2014_mean",
                        "BNAM_2015_mean","BNAM_2016_mean","BNAM_2017_mean","BNAM_2018_mean")))

# Convert coordinate system to match RV data
BNAM_1995_mean <- projectRaster(BNAM_1995_mean, crs = "+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs")
BNAM_1996_mean <- projectRaster(BNAM_1996_mean, crs = "+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs")
BNAM_1997_mean <- projectRaster(BNAM_1997_mean, crs = "+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs")
BNAM_1998_mean <- projectRaster(BNAM_1998_mean, crs = "+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs")
BNAM_1999_mean <- projectRaster(BNAM_1999_mean, crs = "+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs")
BNAM_2000_mean <- projectRaster(BNAM_2000_mean, crs = "+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs")
BNAM_2001_mean <- projectRaster(BNAM_2001_mean, crs = "+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs")
BNAM_2002_mean <- projectRaster(BNAM_2002_mean, crs = "+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs")
BNAM_2003_mean <- projectRaster(BNAM_2003_mean, crs = "+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs")
BNAM_2004_mean <- projectRaster(BNAM_2004_mean, crs = "+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs")
BNAM_2005_mean <- projectRaster(BNAM_2005_mean, crs = "+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs")
BNAM_2006_mean <- projectRaster(BNAM_2006_mean, crs = "+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs")
BNAM_2007_mean <- projectRaster(BNAM_2007_mean, crs = "+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs")
BNAM_2008_mean <- projectRaster(BNAM_2008_mean, crs = "+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs")
BNAM_2009_mean <- projectRaster(BNAM_2009_mean, crs = "+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs")
BNAM_2010_mean <- projectRaster(BNAM_2010_mean, crs = "+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs")
BNAM_2011_mean <- projectRaster(BNAM_2011_mean, crs = "+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs")
BNAM_2012_mean <- projectRaster(BNAM_2012_mean, crs = "+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs")
BNAM_2013_mean <- projectRaster(BNAM_2013_mean, crs = "+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs")
BNAM_2014_mean <- projectRaster(BNAM_2014_mean, crs = "+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs")
BNAM_2015_mean <- projectRaster(BNAM_2015_mean, crs = "+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs")
BNAM_2016_mean <- projectRaster(BNAM_2016_mean, crs = "+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs")
BNAM_2017_mean <- projectRaster(BNAM_2017_mean, crs = "+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs")
BNAM_2018_mean <- projectRaster(BNAM_2018_mean, crs = "+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs")


#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
#### Load depth data ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

depth<-"data/depth.tif" 
depth=raster(depth)

# Convert coordinate system
depth_raster <- projectRaster(depth, crs = "+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs")

# Make rasters match
depth_raster = resample(depth_raster, BNAM_1996_mean, "bilinear")

# Clip depth to BNAM data

ext<-extent(BNAM_1996_mean)

depth_crop<-crop(depth_raster, ext)
depth_clip<-mask(depth_raster,BNAM_1996_mean)


#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
#### Merge all environmental data and format ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

# Merge
env_data<-stack(BNAM_1995_mean,BNAM_1996_mean,BNAM_1997_mean,BNAM_1998_mean,BNAM_1999_mean,BNAM_2000_mean
                ,BNAM_2001_mean,BNAM_2002_mean,BNAM_2003_mean,BNAM_2004_mean,BNAM_2005_mean,BNAM_2006_mean
                ,BNAM_2007_mean,BNAM_2008_mean,BNAM_2009_mean,BNAM_2010_mean,BNAM_2011_mean,BNAM_2012_mean
                ,BNAM_2013_mean,BNAM_2014_mean,BNAM_2015_mean,BNAM_2016_mean,BNAM_2017_mean,BNAM_2018_mean,depth_clip)
env_data <- rasterToPoints(env_data)
env_data<-as.data.table(env_data)

# Format columns
colnames(env_data)[colnames(env_data)=="layer.1"] <- "BNAM_1995_mean"
colnames(env_data)[colnames(env_data)=="layer.2"] <- "BNAM_1996_mean"
colnames(env_data)[colnames(env_data)=="layer.3"] <- "BNAM_1997_mean"
colnames(env_data)[colnames(env_data)=="layer.4"] <- "BNAM_1998_mean"
colnames(env_data)[colnames(env_data)=="layer.5"] <- "BNAM_1999_mean"
colnames(env_data)[colnames(env_data)=="layer.6"] <- "BNAM_2000_mean"
colnames(env_data)[colnames(env_data)=="layer.7"] <- "BNAM_2001_mean"
colnames(env_data)[colnames(env_data)=="layer.8"] <- "BNAM_2002_mean"
colnames(env_data)[colnames(env_data)=="layer.9"] <- "BNAM_2003_mean"
colnames(env_data)[colnames(env_data)=="layer.10"] <- "BNAM_2004_mean"
colnames(env_data)[colnames(env_data)=="layer.11"] <- "BNAM_2005_mean"
colnames(env_data)[colnames(env_data)=="layer.12"] <- "BNAM_2006_mean"
colnames(env_data)[colnames(env_data)=="layer.13"] <- "BNAM_2007_mean"
colnames(env_data)[colnames(env_data)=="layer.14"] <- "BNAM_2008_mean"
colnames(env_data)[colnames(env_data)=="layer.15"] <- "BNAM_2009_mean"
colnames(env_data)[colnames(env_data)=="layer.16"] <- "BNAM_2010_mean"
colnames(env_data)[colnames(env_data)=="layer.17"] <- "BNAM_2011_mean"
colnames(env_data)[colnames(env_data)=="layer.18"] <- "BNAM_2012_mean"
colnames(env_data)[colnames(env_data)=="layer.19"] <- "BNAM_2013_mean"
colnames(env_data)[colnames(env_data)=="layer.20"] <- "BNAM_2014_mean"
colnames(env_data)[colnames(env_data)=="layer.21"] <- "BNAM_2015_mean"
colnames(env_data)[colnames(env_data)=="layer.22"] <- "BNAM_2016_mean"
colnames(env_data)[colnames(env_data)=="layer.23"] <- "BNAM_2017_mean"
colnames(env_data)[colnames(env_data)=="layer.24"] <- "BNAM_2018_mean"
colnames(env_data)[colnames(env_data)=="depth"] <- "depth"

# Remove unnecessary objects
rm(list=setdiff(ls(), c("env_data")))


#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
#### Add in region to data ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

# Load in shapefile for regions
regions <- readOGR('data/shapefiles/Survey_areas.shp')

# Modify coords to matchd data
regions<-spTransform(regions,CRS("+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs"))
region_data<-as.data.frame(regions)
# Turn data into spatial objects
env_dataspatial<-env_data
coordinates(env_dataspatial) <- ~ x + y
proj4string(env_dataspatial) <- proj4string(regions)

test <- data.frame(xx=over(env_dataspatial, regions))
combine <- cbind(test, env_data)
#combine <- na.omit(combine)

data<-combine
#Format output columns
data$xx.Shape_Area<-NULL
data$region<-as.factor(data$xx.region)
data$xx.region<-NULL


#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
#### Format data for mass data projection ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
data<-as.data.table(data)
Data_BNAM<-melt(data, id.vars = c("x","y","depth","region"), measure.vars = c("BNAM_1995_mean","BNAM_1996_mean","BNAM_1997_mean","BNAM_1998_mean","BNAM_1999_mean",
                                                                              "BNAM_2000_mean","BNAM_2001_mean","BNAM_2002_mean","BNAM_2003_mean","BNAM_2004_mean",
                                                                              "BNAM_2005_mean","BNAM_2006_mean","BNAM_2007_mean","BNAM_2008_mean","BNAM_2009_mean",
                                                                              "BNAM_2010_mean","BNAM_2011_mean","BNAM_2012_mean","BNAM_2013_mean","BNAM_2014_mean",
                                                                              "BNAM_2015_mean","BNAM_2016_mean","BNAM_2017_mean","BNAM_2018_mean"))

colnames(Data_BNAM)[colnames(Data_BNAM)=="value"] <- "temperature_at_bottom"
Data_BNAM[variable=="BNAM_1995_mean",year_surv:="1995"]
Data_BNAM[variable=="BNAM_1996_mean",year_surv:="1996"]
Data_BNAM[variable=="BNAM_1997_mean",year_surv:="1997"]
Data_BNAM[variable=="BNAM_1998_mean",year_surv:="1998"]
Data_BNAM[variable=="BNAM_1999_mean",year_surv:="1999"]
Data_BNAM[variable=="BNAM_2000_mean",year_surv:="2000"]
Data_BNAM[variable=="BNAM_2001_mean",year_surv:="2001"]
Data_BNAM[variable=="BNAM_2002_mean",year_surv:="2002"]
Data_BNAM[variable=="BNAM_2003_mean",year_surv:="2003"]
Data_BNAM[variable=="BNAM_2004_mean",year_surv:="2004"]
Data_BNAM[variable=="BNAM_2005_mean",year_surv:="2005"]
Data_BNAM[variable=="BNAM_2006_mean",year_surv:="2006"]
Data_BNAM[variable=="BNAM_2007_mean",year_surv:="2007"]
Data_BNAM[variable=="BNAM_2008_mean",year_surv:="2008"]
Data_BNAM[variable=="BNAM_2009_mean",year_surv:="2009"]
Data_BNAM[variable=="BNAM_2010_mean",year_surv:="2010"]
Data_BNAM[variable=="BNAM_2011_mean",year_surv:="2011"]
Data_BNAM[variable=="BNAM_2012_mean",year_surv:="2012"]
Data_BNAM[variable=="BNAM_2013_mean",year_surv:="2013"]
Data_BNAM[variable=="BNAM_2014_mean",year_surv:="2014"]
Data_BNAM[variable=="BNAM_2015_mean",year_surv:="2015"]
Data_BNAM[variable=="BNAM_2016_mean",year_surv:="2016"]
Data_BNAM[variable=="BNAM_2017_mean",year_surv:="2017"]
Data_BNAM[variable=="BNAM_2018_mean",year_surv:="2018"]
Data_BNAM$variable<-NULL
Data_BNAM$year_surv<-as.factor(Data_BNAM$year_surv)

#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
#### Export data ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

fwrite(Data_BNAM,file="code/outputs/data/BNAM_Data_Output_shiny_app.csv")
