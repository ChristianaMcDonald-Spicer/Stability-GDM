##Run and Project GDM to other times
#Christiana McDonald-Spicer
#August 2016

############################## Description and Instructions ##############################

##Description of the script:
  #This script creates a Generalised Dissimilarity Model, project the model to different time periods, and visualises the results in six steps.
  #It requires species-site data in csv format and environmental raster data for multiple time periods in asc format
  #The steps are as follows:
      #Step 1 - sets up the variables. User input required.
      #Step 2 - reads in and prepares the data for the model selection.
      #Step 3 - runs a model selection to assist with choosing environmental variables. Outputs a summary and plot to the console.
      #Step 4 - runs a GDM. This requires the user to input selected variables. Outputs a summary and plot to the console.
      #Step 5 - transform the rasters for the current period and then project the model to different time periods.
      #Step 6 - visualise the patterns in the current time period. Outputs a plot to the console

##Instructions:
  #You will need to edit variables and file paths in Step 1. 
  #After model selection in Step 3, examine the outputs and select the variables you wish to include. 
  #Edit the first two lines of Step 4 for reflect the selected variables.


############################## Step 1 - Set up the variables ##############################

##load the packages
library(gdm)
library(raster)

##set working directory
#opens window to choose working directory
#replace with file path if you wish
setwd(choose.dir())
dir.create(file.path(".", "Output"))

##set paths for files and folders
species_data <- "./Input Data/KW_data_rounded_to_0.041.csv"
present_enviro_folder <- "./Input Data/present_layers/"

##set file path for other time periods
#this should contain subdirectories for each time period
other_times_folder <- "./Input Data/past layers/"

##set the extent of your study area
#this may be the same extent as your rasters or smaller if you need to crop them
#order c(xmin, xmax, ymin, ymax)
study_extent <- c(111, 156, -25.5, -9.008331)

##set the sppFilter number to account for limited sampling
#this excludes sites with less than the specified number of species
#set to 0 for default/no filter
spprich <- 6

##set the number of splines for the model
spl <- 4

############################## Step 2 - Read in and prepare data ##############################

##read in species occurrence data in x,y,species format
sppTab <- read.csv(species_data, header=TRUE, sep=",")
names(sppTab) <- c("Lon", "Lat","Species")
head(sppTab)

##read in raster data from a folder
#change folder to appropriate folder path
rasters <- list.files(path = present_enviro_folder, pattern = "*.asc$")
rasters <- sort(rasters)
rasters <- paste(present_enviro_folder, rasters, sep="")
envRast_full <- stack(rasters)
envRast <- crop(envRast_full, study_extent)

##create site pair table 
gdmTab_rast <- formatsitepair(sppTab, bioFormat=2, XColumn="Lon", YColumn="Lat", sppColumn="Species", sppFilter = spprich, predData=envRast)

##remove NAs
gdmTab_rast <- na.omit(gdmTab_rast)

############################## Step 3 - Run the Model Selection ##############################

##set the number of splines
stack_length_gdmtest <- envRast@data@nlayers + 1
splines_gdmtest <- rep_len(spl, stack_length_gdmtest)

##run the model selection
gdm_test <- gdm.varImp(gdmTab_rast, geo=TRUE)
gdm_test
barplot(gdm_test[[2]][,1])

############################## Step 4 - Run the Chosen Model ##############################

##set raster layers to include in the final model
#enter the environmental variables chosen based on the model test
rasters_gdmfinal <- c("bioclim_04", "bioclim_10", "bioclim_12", "elevation_resampled", "GEOLMNAGE_resampled", "LITHFERT_resampled", "longitude")
#set the layout of the final plot to reflect the number of variables chosen
plot_layout <- c(4, 4)

##create and prepare the new site pair table 
envRast_final <- subset(envRast, rasters_gdmfinal)
gdmTab_final <- formatsitepair(sppTab, bioFormat=2, XColumn="Lon", YColumn="Lat", sppColumn="Species", sppFilter = spprich, predData=envRast_final)
gdmTab_final <- na.omit(gdmTab_final)

##set the number of splines
stack_length <- envRast_final@data@nlayers + 1
splines_gdmfinal <- rep_len(spl, stack_length)

##run 
gdm_final <- gdm(gdmTab_final, geo=TRUE, splines = splines_gdmfinal)
summary.gdm(gdm_final)

##plot the splines of the variables
plot(gdm_final, plot.layout=plot_layout)

############################## Step 5 - Transform the rasters and project to different time periods ##############################

##transform the rasters for the current time period
rastTrans <- gdm.transform(gdm_final, envRast_final)
dir.create(file.path("Output", "trans_present"))
writeRaster(rastTrans, "./Output/trans_present/trans_rasters_present.asc", bylayer=TRUE, suffix = names(rastTrans))

##transform rasters for other time periods
time_periods <- list.dirs(path = other_times_folder, full.names=FALSE, recursive=FALSE)

for (time in time_periods){
  #choose the folder
  time_folder <- paste(other_times_folder, time, sep="/")
  
  #create a raster stack of the layers
  rasters_time <- sort(list.files(path = time_folder, pattern = "*.asc$", full.names=TRUE))
  envRast_time <- stack(rasters_time)
  envRast_time <- subset(envRast_time, rasters_gdmfinal)
  envRast_time <- crop(envRast_time, study_extent)
  
  #transform the rasters
  rastTrans_time <- gdm.transform(gdm_final, envRast_time)
  
  #write
  dir.create(file.path("Output1", paste("trans", time, sep="_")))
  writeRaster(rastTrans_time, paste("Output1", paste("trans", time, sep="_"), paste("trans_rasters_", time, ".asc", sep=""), sep="/"), bylayer=TRUE, suffix = names(rastTrans))
}

############################## Step 6 - Visualise the patterns in the current time ##############################

rastDat <- na.omit(getValues(rastTrans))
pcaSamp <- prcomp(rastDat)
pcaRast <- predict(rastTrans, pcaSamp, index=1:3)
pcaRast[[1]] <- (pcaRast[[1]]-pcaRast[[1]]@data@min) /
  (pcaRast[[1]]@data@max-pcaRast[[1]]@data@min)*255
pcaRast[[2]] <- (pcaRast[[2]]-pcaRast[[2]]@data@min) /
  (pcaRast[[2]]@data@max-pcaRast[[2]]@data@min)*255
pcaRast[[3]] <- (pcaRast[[3]]-pcaRast[[3]]@data@min) /
  (pcaRast[[3]]@data@max-pcaRast[[3]]@data@min)*255

plotRGB(pcaRast, r=1, g=2, b=3)
