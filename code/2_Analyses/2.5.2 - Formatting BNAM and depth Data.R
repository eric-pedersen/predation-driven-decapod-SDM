### Load in BNAM and depth data into one dataset----
# Purpose: Show shrimp and crab distributions based on BNAM outputs
# Author: S. Zabihi-Seissan


# Load packages
library(rmatio)
library(raster)
library(tidyverse)
library(dplyr)
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
BNAM_1995_7=read.mat(paste0("data/BNAM/BNAM_output/",1995,"/","BT_",7,".mat"))
BNAM_1995_7=BNAM_1995_7$BT
BNAM_1995_8=read.mat(paste0("data/BNAM/BNAM_output/",1995,"/","BT_",8,".mat"))
BNAM_1995_8=BNAM_1995_8$BT
BNAM_1995_9=read.mat(paste0("data/BNAM/BNAM_output/",1995,"/","BT_",9,".mat"))
BNAM_1995_9=BNAM_1995_9$BT

BNAM_1996_7=read.mat(paste0("data/BNAM/BNAM_output/",1996,"/","BT_",7,".mat"))
BNAM_1996_7=BNAM_1996_7$BT
BNAM_1996_8=read.mat(paste0("data/BNAM/BNAM_output/",1996,"/","BT_",8,".mat"))
BNAM_1996_8=BNAM_1996_8$BT
BNAM_1996_9=read.mat(paste0("data/BNAM/BNAM_output/",1996,"/","BT_",9,".mat"))
BNAM_1996_9=BNAM_1996_9$BT

BNAM_1997_7=read.mat(paste0("data/BNAM/BNAM_output/",1997,"/","BT_",7,".mat"))
BNAM_1997_7=BNAM_1997_7$BT
BNAM_1997_8=read.mat(paste0("data/BNAM/BNAM_output/",1997,"/","BT_",8,".mat"))
BNAM_1997_8=BNAM_1997_8$BT
BNAM_1997_9=read.mat(paste0("data/BNAM/BNAM_output/",1997,"/","BT_",9,".mat"))
BNAM_1997_9=BNAM_1997_9$BT

BNAM_1998_7=read.mat(paste0("data/BNAM/BNAM_output/",1998,"/","BT_",7,".mat"))
BNAM_1998_7=BNAM_1998_7$BT
BNAM_1998_8=read.mat(paste0("data/BNAM/BNAM_output/",1998,"/","BT_",8,".mat"))
BNAM_1998_8=BNAM_1998_8$BT
BNAM_1998_9=read.mat(paste0("data/BNAM/BNAM_output/",1998,"/","BT_",9,".mat"))
BNAM_1998_9=BNAM_1998_9$BT

BNAM_1999_7=read.mat(paste0("data/BNAM/BNAM_output/",1999,"/","BT_",7,".mat"))
BNAM_1999_7=BNAM_1999_7$BT
BNAM_1999_8=read.mat(paste0("data/BNAM/BNAM_output/",1999,"/","BT_",8,".mat"))
BNAM_1999_8=BNAM_1999_8$BT
BNAM_1999_9=read.mat(paste0("data/BNAM/BNAM_output/",1999,"/","BT_",9,".mat"))
BNAM_1999_9=BNAM_1999_9$BT

BNAM_2000_7=read.mat(paste0("data/BNAM/BNAM_output/",2000,"/","BT_",7,".mat"))
BNAM_2000_7=BNAM_2000_7$BT
BNAM_2000_8=read.mat(paste0("data/BNAM/BNAM_output/",2000,"/","BT_",8,".mat"))
BNAM_2000_8=BNAM_2000_8$BT
BNAM_2000_9=read.mat(paste0("data/BNAM/BNAM_output/",2000,"/","BT_",9,".mat"))
BNAM_2000_9=BNAM_2000_9$BT

BNAM_2001_7=read.mat(paste0("data/BNAM/BNAM_output/",2001,"/","BT_",7,".mat"))
BNAM_2001_7=BNAM_2001_7$BT
BNAM_2001_8=read.mat(paste0("data/BNAM/BNAM_output/",2001,"/","BT_",8,".mat"))
BNAM_2001_8=BNAM_2001_8$BT
BNAM_2001_9=read.mat(paste0("data/BNAM/BNAM_output/",2001,"/","BT_",9,".mat"))
BNAM_2001_9=BNAM_2001_9$BT

BNAM_2002_7=read.mat(paste0("data/BNAM/BNAM_output/",2002,"/","BT_",7,".mat"))
BNAM_2002_7=BNAM_2002_7$BT
BNAM_2002_8=read.mat(paste0("data/BNAM/BNAM_output/",2002,"/","BT_",8,".mat"))
BNAM_2002_8=BNAM_2002_8$BT
BNAM_2002_9=read.mat(paste0("data/BNAM/BNAM_output/",2002,"/","BT_",9,".mat"))
BNAM_2002_9=BNAM_2002_9$BT

BNAM_2003_7=read.mat(paste0("data/BNAM/BNAM_output/",2003,"/","BT_",7,".mat"))
BNAM_2003_7=BNAM_2003_7$BT
BNAM_2003_8=read.mat(paste0("data/BNAM/BNAM_output/",2003,"/","BT_",8,".mat"))
BNAM_2003_8=BNAM_2003_8$BT
BNAM_2003_9=read.mat(paste0("data/BNAM/BNAM_output/",2003,"/","BT_",9,".mat"))
BNAM_2003_9=BNAM_2003_9$BT

BNAM_2004_7=read.mat(paste0("data/BNAM/BNAM_output/",2004,"/","BT_",7,".mat"))
BNAM_2004_7=BNAM_2004_7$BT
BNAM_2004_8=read.mat(paste0("data/BNAM/BNAM_output/",2004,"/","BT_",8,".mat"))
BNAM_2004_8=BNAM_2004_8$BT
BNAM_2004_9=read.mat(paste0("data/BNAM/BNAM_output/",2004,"/","BT_",9,".mat"))
BNAM_2004_9=BNAM_2004_9$BT

BNAM_2005_7=read.mat(paste0("data/BNAM/BNAM_output/",2005,"/","BT_",7,".mat"))
BNAM_2005_7=BNAM_2005_7$BT
BNAM_2005_8=read.mat(paste0("data/BNAM/BNAM_output/",2005,"/","BT_",8,".mat"))
BNAM_2005_8=BNAM_2005_8$BT
BNAM_2005_9=read.mat(paste0("data/BNAM/BNAM_output/",2005,"/","BT_",9,".mat"))
BNAM_2005_9=BNAM_2005_9$BT

BNAM_2006_7=read.mat(paste0("data/BNAM/BNAM_output/",2006,"/","BT_",7,".mat"))
BNAM_2006_7=BNAM_2006_7$BT
BNAM_2006_8=read.mat(paste0("data/BNAM/BNAM_output/",2006,"/","BT_",8,".mat"))
BNAM_2006_8=BNAM_2006_8$BT
BNAM_2006_9=read.mat(paste0("data/BNAM/BNAM_output/",2006,"/","BT_",9,".mat"))
BNAM_2006_9=BNAM_2006_9$BT

BNAM_2007_7=read.mat(paste0("data/BNAM/BNAM_output/",2007,"/","BT_",7,".mat"))
BNAM_2007_7=BNAM_2007_7$BT
BNAM_2007_8=read.mat(paste0("data/BNAM/BNAM_output/",2007,"/","BT_",8,".mat"))
BNAM_2007_8=BNAM_2007_8$BT
BNAM_2007_9=read.mat(paste0("data/BNAM/BNAM_output/",2007,"/","BT_",9,".mat"))
BNAM_2007_9=BNAM_2007_9$BT

BNAM_2008_7=read.mat(paste0("data/BNAM/BNAM_output/",2008,"/","BT_",7,".mat"))
BNAM_2008_7=BNAM_2008_7$BT
BNAM_2008_8=read.mat(paste0("data/BNAM/BNAM_output/",2008,"/","BT_",8,".mat"))
BNAM_2008_8=BNAM_2008_8$BT
BNAM_2008_9=read.mat(paste0("data/BNAM/BNAM_output/",2008,"/","BT_",9,".mat"))
BNAM_2008_9=BNAM_2008_9$BT

BNAM_2009_7=read.mat(paste0("data/BNAM/BNAM_output/",2009,"/","BT_",7,".mat"))
BNAM_2009_7=BNAM_2009_7$BT
BNAM_2009_8=read.mat(paste0("data/BNAM/BNAM_output/",2009,"/","BT_",8,".mat"))
BNAM_2009_8=BNAM_2009_8$BT
BNAM_2009_9=read.mat(paste0("data/BNAM/BNAM_output/",2009,"/","BT_",9,".mat"))
BNAM_2009_9=BNAM_2009_9$BT

BNAM_2010_7=read.mat(paste0("data/BNAM/BNAM_output/",2010,"/","BT_",7,".mat"))
BNAM_2010_7=BNAM_2010_7$BT
BNAM_2010_8=read.mat(paste0("data/BNAM/BNAM_output/",2010,"/","BT_",8,".mat"))
BNAM_2010_8=BNAM_2010_8$BT
BNAM_2010_9=read.mat(paste0("data/BNAM/BNAM_output/",2010,"/","BT_",9,".mat"))
BNAM_2010_9=BNAM_2010_9$BT

BNAM_2011_7=read.mat(paste0("data/BNAM/BNAM_output/",2011,"/","BT_",7,".mat"))
BNAM_2011_7=BNAM_2011_7$BT
BNAM_2011_8=read.mat(paste0("data/BNAM/BNAM_output/",2011,"/","BT_",8,".mat"))
BNAM_2011_8=BNAM_2011_8$BT
BNAM_2011_9=read.mat(paste0("data/BNAM/BNAM_output/",2011,"/","BT_",9,".mat"))
BNAM_2011_9=BNAM_2011_9$BT

BNAM_2012_7=read.mat(paste0("data/BNAM/BNAM_output/",2012,"/","BT_",7,".mat"))
BNAM_2012_7=BNAM_2012_7$BT
BNAM_2012_8=read.mat(paste0("data/BNAM/BNAM_output/",2012,"/","BT_",8,".mat"))
BNAM_2012_8=BNAM_2012_8$BT
BNAM_2012_9=read.mat(paste0("data/BNAM/BNAM_output/",2012,"/","BT_",9,".mat"))
BNAM_2012_9=BNAM_2012_9$BT

BNAM_2013_7=read.mat(paste0("data/BNAM/BNAM_output/",2013,"/","BT_",7,".mat"))
BNAM_2013_7=BNAM_2013_7$BT
BNAM_2013_8=read.mat(paste0("data/BNAM/BNAM_output/",2013,"/","BT_",8,".mat"))
BNAM_2013_8=BNAM_2013_8$BT
BNAM_2013_9=read.mat(paste0("data/BNAM/BNAM_output/",2013,"/","BT_",9,".mat"))
BNAM_2013_9=BNAM_2013_9$BT

BNAM_2014_7=read.mat(paste0("data/BNAM/BNAM_output/",2014,"/","BT_",7,".mat"))
BNAM_2014_7=BNAM_2014_7$BT
BNAM_2014_8=read.mat(paste0("data/BNAM/BNAM_output/",2014,"/","BT_",8,".mat"))
BNAM_2014_8=BNAM_2014_8$BT
BNAM_2014_9=read.mat(paste0("data/BNAM/BNAM_output/",2014,"/","BT_",9,".mat"))
BNAM_2014_9=BNAM_2014_9$BT

BNAM_2015_7=read.mat(paste0("data/BNAM/BNAM_output/",2015,"/","BT_",7,".mat"))
BNAM_2015_7=BNAM_2015_7$BT
BNAM_2015_8=read.mat(paste0("data/BNAM/BNAM_output/",2015,"/","BT_",8,".mat"))
BNAM_2015_8=BNAM_2015_8$BT
BNAM_2015_9=read.mat(paste0("data/BNAM/BNAM_output/",2015,"/","BT_",9,".mat"))
BNAM_2015_9=BNAM_2015_9$BT

BNAM_2016_7=read.mat(paste0("data/BNAM/BNAM_output/",2016,"/","BT_",7,".mat"))
BNAM_2016_7=BNAM_2016_7$BT
BNAM_2016_8=read.mat(paste0("data/BNAM/BNAM_output/",2016,"/","BT_",8,".mat"))
BNAM_2016_8=BNAM_2016_8$BT
BNAM_2016_9=read.mat(paste0("data/BNAM/BNAM_output/",2016,"/","BT_",9,".mat"))
BNAM_2016_9=BNAM_2016_9$BT

BNAM_2017_7=read.mat(paste0("data/BNAM/BNAM_output/",2017,"/","BT_",7,".mat"))
BNAM_2017_7=BNAM_2017_7$BT
BNAM_2017_8=read.mat(paste0("data/BNAM/BNAM_output/",2017,"/","BT_",8,".mat"))
BNAM_2017_8=BNAM_2017_8$BT
BNAM_2017_9=read.mat(paste0("data/BNAM/BNAM_output/",2017,"/","BT_",9,".mat"))
BNAM_2017_9=BNAM_2017_9$BT

BNAM_2018_7=read.mat(paste0("data/BNAM/BNAM_output/",2018,"/","BT_",7,".mat"))
BNAM_2018_7=BNAM_2018_7$BT
BNAM_2018_8=read.mat(paste0("data/BNAM/BNAM_output/",2018,"/","BT_",8,".mat"))
BNAM_2018_8=BNAM_2018_8$BT
BNAM_2018_9=read.mat(paste0("data/BNAM/BNAM_output/",2018,"/","BT_",9,".mat"))
BNAM_2018_9=BNAM_2018_9$BT

# converting into dataframes
BNAM_1995_7=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1995_7))
BNAM_1995_8=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1995_8))
BNAM_1995_9=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1995_9))

BNAM_1996_7=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1996_7))
BNAM_1996_8=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1996_8))
BNAM_1996_9=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1996_9))

BNAM_1997_7=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1997_7))
BNAM_1997_8=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1997_8))
BNAM_1997_9=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1997_9))

BNAM_1998_7=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1998_7))
BNAM_1998_8=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1998_8))
BNAM_1998_9=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1998_9))

BNAM_1999_7=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1999_7))
BNAM_1999_8=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1999_8))
BNAM_1999_9=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_1999_9))

BNAM_2000_7=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2000_7))
BNAM_2000_8=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2000_8))
BNAM_2000_9=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2000_9))

BNAM_2001_7=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2001_7))
BNAM_2001_8=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2001_8))
BNAM_2001_9=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2001_9))

BNAM_2002_7=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2002_7))
BNAM_2002_8=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2002_8))
BNAM_2002_9=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2002_9))

BNAM_2003_7=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2003_7))
BNAM_2003_8=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2003_8))
BNAM_2003_9=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2003_9))

BNAM_2004_7=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2004_7))
BNAM_2004_8=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2004_8))
BNAM_2004_9=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2004_9))

BNAM_2005_7=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2005_7))
BNAM_2005_8=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2005_8))
BNAM_2005_9=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2005_9))

BNAM_2006_7=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2006_7))
BNAM_2006_8=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2006_8))
BNAM_2006_9=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2006_9))

BNAM_2007_7=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2007_7))
BNAM_2007_8=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2007_8))
BNAM_2007_9=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2007_9))

BNAM_2008_7=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2008_7))
BNAM_2008_8=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2008_8))
BNAM_2008_9=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2008_9))

BNAM_2009_7=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2009_7))
BNAM_2009_8=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2009_8))
BNAM_2009_9=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2009_9))

BNAM_2010_7=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2010_7))
BNAM_2010_8=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2010_8))
BNAM_2010_9=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2010_9))

BNAM_2011_7=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2011_7))
BNAM_2011_8=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2011_8))
BNAM_2011_9=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2011_9))

BNAM_2012_7=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2012_7))
BNAM_2012_8=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2012_8))
BNAM_2012_9=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2012_9))

BNAM_2013_7=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2013_7))
BNAM_2013_8=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2013_8))
BNAM_2013_9=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2013_9))

BNAM_2014_7=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2014_7))
BNAM_2014_8=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2014_8))
BNAM_2014_9=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2014_9))

BNAM_2015_7=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2015_7))
BNAM_2015_8=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2015_8))
BNAM_2015_9=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2015_9))

BNAM_2016_7=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2016_7))
BNAM_2016_8=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2016_8))
BNAM_2016_9=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2016_9))

BNAM_2017_7=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2017_7))
BNAM_2017_8=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2017_8))
BNAM_2017_9=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2017_9))

BNAM_2018_7=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2018_7))
BNAM_2018_8=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2018_8))
BNAM_2018_9=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(BNAM_2018_9))

# Get coords
temp_coords <- read.mat("data/BNAM/anomalies/dTbtm_dSbtm_for_RS.mat")
lons=temp_coords$nav_lon
lats=temp_coords$nav_lat

# Loading the matlab data
Data_Anomalies=temp_coords$RCP85_2075_dTbtm
Anomalies_7<-Data_Anomalies[7,,]
Anomalies_8<-Data_Anomalies[8,,]
Anomalies_9<-Data_Anomalies[9,,]

# converting into dataframes
Anomalies_7=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(Anomalies_7))
Anomalies_8=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(Anomalies_8))
Anomalies_9=data.frame(Longitude=as.vector(lons),Latitude=as.vector(lats),z=as.vector(Anomalies_9))

latlong <- ("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")                         

## Converting into rasters
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

#Month 7
grid <- Anomalies_7%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- Anomalies_7%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=Anomalies_7[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
Anomalies_7=rasterFromXYZ(out,crs=latlong)
#Month 8
grid <- Anomalies_8%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- Anomalies_8%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=Anomalies_8[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
Anomalies_8=rasterFromXYZ(out,crs=latlong)
#Month 9
grid <- Anomalies_9%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%raster::extent(.)%>%raster::raster(.)
raster::res(grid) <- 0.09
out <- Anomalies_9%>%st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%st_coordinates()%>%
  raster::rasterize(.,grid,field=Anomalies_9[,3],fun=mean)%>%raster::rasterToPoints(.)%>%data.frame()%>%
  rename(Longitude = x, Latitude = y, MAP = layer)
Anomalies_9=rasterFromXYZ(out,crs=latlong)

# Create raster stacks
BNAM_1995<-stack(BNAM_1995_7,BNAM_1995_8,BNAM_1995_9)
BNAM_1996<-stack(BNAM_1996_7,BNAM_1996_8,BNAM_1996_9)
BNAM_1997<-stack(BNAM_1997_7,BNAM_1997_8,BNAM_1997_9)
BNAM_1998<-stack(BNAM_1998_7,BNAM_1998_8,BNAM_1998_9)
BNAM_1999<-stack(BNAM_1999_7,BNAM_1999_8,BNAM_1999_9)
BNAM_2000<-stack(BNAM_2000_7,BNAM_2000_8,BNAM_2000_9)
BNAM_2001<-stack(BNAM_2001_7,BNAM_2001_8,BNAM_2001_9)
BNAM_2002<-stack(BNAM_2002_7,BNAM_2002_8,BNAM_2002_9)
BNAM_2003<-stack(BNAM_2003_7,BNAM_2003_8,BNAM_2003_9)
BNAM_2004<-stack(BNAM_2004_7,BNAM_2004_8,BNAM_2004_9)
BNAM_2005<-stack(BNAM_2005_7,BNAM_2005_8,BNAM_2005_9)
BNAM_2006<-stack(BNAM_2006_7,BNAM_2006_8,BNAM_2006_9)
BNAM_2007<-stack(BNAM_2007_7,BNAM_2007_8,BNAM_2007_9)
BNAM_2008<-stack(BNAM_2008_7,BNAM_2008_8,BNAM_2008_9)
BNAM_2009<-stack(BNAM_2009_7,BNAM_2009_8,BNAM_2009_9)
BNAM_2010<-stack(BNAM_2010_7,BNAM_2010_8,BNAM_2010_9)
BNAM_2011<-stack(BNAM_2011_7,BNAM_2011_8,BNAM_2011_9)
BNAM_2012<-stack(BNAM_2012_7,BNAM_2012_8,BNAM_2012_9)
BNAM_2013<-stack(BNAM_2013_7,BNAM_2013_8,BNAM_2013_9)
BNAM_2014<-stack(BNAM_2014_7,BNAM_2014_8,BNAM_2014_9)
BNAM_2015<-stack(BNAM_2015_7,BNAM_2015_8,BNAM_2015_9)
BNAM_2016<-stack(BNAM_2016_7,BNAM_2016_8,BNAM_2016_9)
BNAM_2017<-stack(BNAM_2017_7,BNAM_2017_8,BNAM_2017_9)
BNAM_2018<-stack(BNAM_2018_7,BNAM_2018_8,BNAM_2018_9)
BNAM_Anomalies<-stack(Anomalies_7,Anomalies_8,Anomalies_9)

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
BNAM_total<-stack(BNAM_1995_mean,BNAM_1996_mean,BNAM_1997_mean,BNAM_1998_mean,BNAM_1999_mean,BNAM_2000_mean,
                   BNAM_2001_mean,BNAM_2002_mean,BNAM_2003_mean,BNAM_2004_mean,BNAM_2005_mean,BNAM_2006_mean,
                   BNAM_2007_mean,BNAM_2008_mean,BNAM_2009_mean,BNAM_2010_mean,BNAM_2011_mean,BNAM_2012_mean,
                   BNAM_2013_mean,BNAM_2014_mean,BNAM_2015_mean)
BNAM_total_mean<-calc(BNAM_total, mean)
BNAM_Anomalies_mean<-calc(BNAM_Anomalies, mean)

# Remove unnecessary objects
rm(list=setdiff(ls(), c("BNAM_1995_mean","BNAM_1996_mean","BNAM_2005_mean","BNAM_2006_mean","BNAM_2015_mean","BNAM_2016_mean","BNAM_total_mean","BNAM_Anomalies_mean")))

# Convert coordinate system to match RV data
BNAM_1995_mean <- projectRaster(BNAM_1995_mean, crs = "+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs")
BNAM_1996_mean <- projectRaster(BNAM_1996_mean, crs = "+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs")
BNAM_2005_mean <- projectRaster(BNAM_2005_mean, crs = "+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs")
BNAM_2006_mean <- projectRaster(BNAM_2006_mean, crs = "+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs")
BNAM_2015_mean <- projectRaster(BNAM_2015_mean, crs = "+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs")
BNAM_2016_mean <- projectRaster(BNAM_2016_mean, crs = "+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs")
BNAM_Anomalies_mean <- projectRaster(BNAM_Anomalies_mean, crs = "+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs")
BNAM_total_mean <- projectRaster(BNAM_total_mean, crs = "+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs")


#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
#### Load depth data ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

depth<-"data/shapefiles/depth.tif" 
depth=raster(depth)

# Convert coordinate system
depth_raster <- projectRaster(depth, crs = "+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs")

# Make rasters match
depth_raster = resample(depth_raster, BNAM_1996_mean, "bilinear")
BNAM_Anomalies_mean=resample(BNAM_Anomalies_mean,BNAM_1996_mean, "bilinear")

# Clip depth and anomalies to BNAM data

ext<-extent(BNAM_1996_mean)

depth_crop<-crop(depth_raster, ext)
depth_clip<-mask(depth_raster,BNAM_1996_mean)
BNAM_Anomalies_crop<-crop(BNAM_Anomalies_mean,ext)
BNAM_Anomalies_clip<-mask(BNAM_Anomalies_mean,BNAM_1996_mean)

# Create 2075 temperature predictions
BNAM_2075_mean<-(BNAM_total_mean+BNAM_Anomalies_mean)

#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
#### Merge all environmental data and format ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

# Merge,
env_data<-stack(BNAM_1995_mean,BNAM_1996_mean,BNAM_2005_mean,BNAM_2006_mean,BNAM_2015_mean,BNAM_2016_mean,depth_clip,BNAM_2075_mean,
                BNAM_total_mean,BNAM_Anomalies_mean)
env_data <- rasterToPoints(env_data)
env_data<-as.data.table(env_data)

# Format columns
colnames(env_data)[colnames(env_data)=="layer.1"] <- "BNAM_1995_mean"
colnames(env_data)[colnames(env_data)=="layer.2"] <- "BNAM_1996_mean"
colnames(env_data)[colnames(env_data)=="layer.3"] <- "BNAM_2005_mean"
colnames(env_data)[colnames(env_data)=="layer.4"] <- "BNAM_2006_mean"
colnames(env_data)[colnames(env_data)=="layer.5"] <- "BNAM_2015_mean"
colnames(env_data)[colnames(env_data)=="layer.6"] <- "BNAM_2016_mean"
colnames(env_data)[colnames(env_data)=="layer.7"] <- "BNAM_2075_mean"
colnames(env_data)[colnames(env_data)=="layer.8"] <- "BNAM_total_mean"
colnames(env_data)[colnames(env_data)=="layer.9"] <- "BNAM_anomalies"

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
#data$xx.Shape_Area<-NULL
#data$region<-as.factor(data$xx.region)
#data$xx.region<-NULL

#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
#### Export data ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>

fwrite(data,file="code/outputs/data/BNAM_Data_Output.csv")
