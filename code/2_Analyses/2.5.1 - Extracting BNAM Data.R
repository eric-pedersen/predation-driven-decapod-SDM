### Projecting shrimp and crab models using BNAM ----
# Purpose: Show shrimp and crab distributions based on BNAM outputs
# Author: S. Zabihi-Seissan

#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
### Load packages ----
#<>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>>
library(dplyr)
library(sf)
library(rmatio)
library(raster)
library(tidyverse)
library(magrittr)
library(data.table)

#extract coordinates
temp_coords <- read.mat("data/BNAM/latlon.mat") 
lons <- temp_coords$Lons
lats <- temp_coords$Lats

#data extract. I find the unzip function buggy. Thankfully you can use 7zip from the commandline
z7path = shQuote('C:\\Program Files\\7-Zip\\7z') # need to have 7zip

#create folder to hold the unzipped contents
dir.create("data/BNAM/BNAM_output\\")

#Unzip first list
file <- paste0("data/BNAM/bnam_list.tar")
cmd <- paste0(z7path, ' e ', file, ' -y -o', "data/BNAM/BNAM_output/",'/') #command line 7zip
shell(cmd)

rm(cmd,file)

#Now extract the individual files
years <- 1995:2018
months <- 1:12

BNAM_output <- list()
for(i in years){
  
  if(!dir.exists(paste0("data/BNAM/BNAM_output/",i,"/"))){dir.create(paste0("data/BNAM/BNAM_output/",i,"//"))}
  
  file <- paste0("data/BNAM/BNAM_output/",i,".tar")
  
  cmd <- paste0(z7path, ' e ', file, ' -y -o', paste0("data/BNAM/BNAM_output/",i,"/"),'/') #command line 7zip
  
  shell(cmd)
  
  for (m in months){
    
    BNAM_output[paste(i,"BT",m,sep="_")] <- read.mat(paste0("data/BNAM/BNAM_output/",i,"/","BT_",m,".mat"))
    BNAM_output[paste(i,"BS",m,sep="_")] <- read.mat(paste0("data/BNAM/BNAM_output/",i,"/","BS_",m,".mat"))
    
  }
  
}


