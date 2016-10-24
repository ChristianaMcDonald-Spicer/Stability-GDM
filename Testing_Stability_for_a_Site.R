##Calculate stability for a single cell through time
#Christiana McDonald-Spicer
#October 2016

############################## Description and Instructions ##############################

##Description of the script:
#This script calculates the stability of each cell compared to the present time.
#It then extracts this information for a given set of coordinates and plots it.
#Stability plotted is both static stability (a cell compared to itself through time) and buffer stability (looking at a radius around a single cell through time).
#The steps are as follows:
#Step 1 - sets up the variables. User input required.
#Step 2 - reads in data for the present day.
#Step 3 - sets up the functions for use in the script.
#Step 4 - runs the functions to calculate stability. Outputs a raster file for each time period
#Step 5 - extracts stability data for a given set of coordinates.
#Step 6 - visualise the pattern of stability at that point. Outputs a plot to the console

##Instructions:
#You will need to edit variables and file paths in Step 1. 
#This uses raster files transformed using GDM

############################## Step 1 - Set up the variables ##############################

##load the packages
library(raster)

##set path for folder containing transformed rasters
#each raster must contain the name of the time period, as given in "time_periods" below
#must contain data from the present day, with "present" in the name
data_folder <- "//franklin.uds.anu.edu.au/home/u5596907/My Documents/Identifying stability/Data/transformed rasters/"

##list the time periods in the analysis
#these must match the names of the raster files
time_periods <- c("002", "004", "006", "008", "010", "012", "014", "016", "018", "020", "022", "024", "026", "028", "030", "032", 
                  "034", "036", "038", "040", "042", "044", "046", "048", "050", "052", "054", "056", "058", "060", "062", "064", 
                  "066", "068", "070", "072", "074", "076", "078", "080", "084", "088", "092", "096", "100", "108", "112", "116", "120")

##set the coordinates to plot
#give longitude and then latitude
coords <- data.frame(125, -17.03)

##set the buffer for the stability analysis
#measured in the units of your raster files, usually metres
buffer <- 50000

############################## Step 2 - Read in data for the present day ##############################

##find the files for the present day
rasters <- list.files(path = data_folder, pattern = "present")

##sort alphabetically so the layers are in a consistent order throughout the analysis 
rasters <- sort(rasters)

##create a raster stack
rasters <- paste(data_folder, rasters, sep="")
rasters.present <- stack(rasters)

############################## Step 3 - Set up the functions ##############################

##stability function
stab.func <- function(stack1, stack2) {
  #create new raster stack
  new.stack <- stack1
  new.stack[] <- NA
  
  #perform calculation for each stack
  new.stack <- abs(stack1-stack2) 
  
  #add all layers together
  added_raster <- sum(new.stack)
}

##function to read past time periods into stability function
run.stab.func <- function(present, time, folder){
  #read in and stack the files for the relevant time period
  rasters <- list.files(path = folder, pattern = time)
  rasters <- sort(rasters)
  rasters <- paste(data_folder, rasters, sep="")
  stack_rasters <- stack(rasters)
  
  #apply stability fuction
  result <- stab.func(present, stack_rasters)
  
  #apply GDM link function
  result <- 1 - exp(result)
}


############################## Step 4 - Apply the functions ##############################

for (i in time_periods) {
  #run the analysis
  result <- run.stab.func(rasters.present, i, data_folder)
  
  #give the output a relevant name
  assign(paste0("stability.", i), result)
  
  #print to the console
  cat(paste("Completed time period ", i), sep="\n")
  }

############################## Step 5 - Extract data for coordinates ##############################

##Set up the coordinates as SpatialPoints
points <- coords
colnames(points) <- c("lon", "lat")
coordinates(points) <- c("lon", "lat")

##Stack the output rasters
stab.rasters <- stack(mget(ls(pattern = "stability")))

##Make a dataframe for static stability
#extract the data
stability.df <- data.frame(extract(stab.rasters, points))
colnames(stability.df) <- time_periods
#add in present day (change relevant to itself will be 0)
stability.df <- cbind("000" = 0, stability.df)
View(stability.df)

##Make a dataframe for stability using buffer
buffer.df <- extract(stab.rasters, points, buffer = buffer, fun=max)
buffer.df <- cbind("000" = 0, buffer.df)
buffer.df <- t(buffer.df)
View(buffer.df)

##Add the present to the list of time periods
n <- c(0)
time_periods_0 <- c(n, time_periods)

##Prepare the dataframes
#Combine the two types of stability with the time periods 
stability.df2 <- rbind(stability.df, time_periods_0)
stability.df2 <- t(stability.df2)
stability.df2 <- cbind(stability.df2, buffer.df)
#give useful column names and reorder columns
colnames(stability.df2) <- c("static", "time", "buffer")
stability.df2 <- stability.df2[,c("time", "static", "buffer")]
#ensure the dataframe is in correct format
stability.df2 <- as.data.frame(stability.df2)
stability.df2$static <- as.numeric(as.character(stability.df2$static))
stability.df2$time <- as.numeric(as.character(stability.df2$time))
stability.df2$buffer <- as.numeric(as.character(stability.df2$buffer))
View(stability.df2)

############################## Step 6 - Plot the data for coordinates ##############################

##Set up the plot using the static suitability measure
plot(stability.df2$time, stability.df2$static, type = "l", 
     xlab="Time (kya)", ylab="Dissimilarity to present", 
     #xlim=(c(min(stability.df2$time), max(stability.df2$time))),
     #xlim=(c(min(stability.df2$time), 80)),
     xlim=(c(5, 40)),
     #ylim=c(-0.3, 0), 
     col="blue") 

##Plot the buffer suitability on the same plot
lines(stability.df2$time, stability.df2$buffer, type="l", 
      #axes=FALSE, 
      #xlim=(c(min(stability.df2$time), max(stability.df2$time))),
      #xlim=(c(min(stability.df2$time), 80)),
      xlim=(c(5, 40)),
      #ylim=c(-0.3, 0), 
      col="darkmagenta")

##Include a legend
legend("bottomleft",
       c("Static", paste("Buffer of", buffer, "m")), # puts text in the legend
       lty=c(1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5),col=c("blue", "darkmagenta")) # gives the legend lines the correct color and width

##Create a title
title(paste("Stability over time for", paste0(as.character(coords[1]), ","), as.character(coords[2])))
