######################################################################################################
## Generalized Dissimilarity modelling - Append Environment Columns to Site Pairs in GDM Format     ##

## 3 January 2014 - Dan Rosauer, Australian National University (dan.rosauer@anu.edu.au)            ##

## This script takes a table with compositional dssimilarity between pairs of sites, and adds       ##
##    columns for environment at each of the sites, in the format required by GDM for Tables in R
##    and by GDM Modeller.                                                                          ##

## NOTE: when generating a model in GDM Modeller, this format works for model fitting, but to       ##
##    generate transformed grids apply the model beyond the training sites in GDM Modeller, it is   ##
##    best to attach the environment data using GDM Modeller.                                       ##

## It requires:                                                                                     ##
## (1) a site-pair dissimilarity table in the 6 column GDM input format                             ##
## (2) the environment grids, either listed, or using all the grids in the specified directory      ##

######################################################################################################

library(raster)

# define the file locations
work.dir <- "//wallace.uds.anu.edu.au/EEG/Moritz Lab/Christiana"   # EDIT THIS TO YOUR LOCATION
input.filename  <- "dist_reptile_AMT_test.csv"
output.filename <- "GDM_input.csv"
env.dir         <- '//wallace.uds.anu.edu.au/EEG/Moritz Lab/Christiana/PaleoClimate/OZ.climates/bioclim/000/asc_only'
setwd(work.dir)

# comment the following line and get all grids in the directory if grids_to_use doesn't exist
#grids_to_use    <- c("bioclim_01.asc","isothermality_2010_1km.asc","jan_rad_2010_1km.asc","mintempjuly_2010_1km.asc","precip_pet_ratio_2010_1km.asc")

# this gets all grids in the folder, if none specified
#if (!exists("grids_to_use")) {
 # grids_to_use <- list.files(env.dir,pattern='*.asc',full.name=TRUE)
#}


#read in raster files and create stack
##this was not in the original script
r01 <- raster("./PaleoClimate/OZ.climates/bioclim/000/bioclim_01.asc")
r04 <- raster("./PaleoClimate/OZ.climates/bioclim/000/bioclim_04.asc")
r10 <- raster("./PaleoClimate/OZ.climates/bioclim/000/bioclim_10.asc")
r11 <- raster("./PaleoClimate/OZ.climates/bioclim/000/bioclim_11.asc")
r12 <- raster("./PaleoClimate/OZ.climates/bioclim/000/bioclim_12.asc")
r15 <- raster("./PaleoClimate/OZ.climates/bioclim/000/bioclim_15.asc")
r16 <- raster("./PaleoClimate/OZ.climates/bioclim/000/bioclim_16.asc")
r17 <- raster("./PaleoClimate/OZ.climates/bioclim/000/bioclim_17.asc")
env.stack <- stack(r01, r04, r10, r11, r12, r15, r16, r17)

# load the file with the site pairs and biological response
sitepairs   <- read.csv(input.filename)

# isolate the site coordinates
site0_xy <- sitepairs[,3:4]
site1_xy <- sitepairs[,5:6]

#
setwd(env.dir)
#env.stack <- stack(grids_to_use)
env.site0 <- data.frame(extract(env.stack,site0_xy))
env.site1 <- data.frame(extract(env.stack,site1_xy))

for (i in 1:ncol(env.site0)) {
  names(env.site0)[i] <- paste("site0.",names(env.site0)[i],sep="")
  names(env.site1)[i] <- paste("site1.",names(env.site1)[i],sep="")
}

sitepairs <- cbind(sitepairs,env.site0,env.site1)

setwd(work.dir)
write.csv(sitepairs,output.filename,row.names=FALSE)
cat("\nGDM composite input data created as",output.filename)

rm(env.site0,env.site1,site0_xy,site1_xy,i)
