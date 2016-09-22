library(sp)
library(maptools)
library(rgeos)

##read in original data
species_table <- read.csv("./Data/unique.locations.species_clean_KW.csv", header=TRUE, sep=",")

##plot to look at the data
#coordinates(species_table) <- c("Lon_round", "Lat_round")
#plot(sppTab)
##read back in original data


##round to fit raster data resolution (so 0.05?)
#create function to round
mround <- function(x,base){ 
  base*round(x/base) 
} 
#create new columns with rounded values
species_table$Lat_round <- mround(species_table$Latitude.processed, 0.041666668839753)
species_table$Lon_round <- mround(species_table$Longitude.processed, 0.041666668839753)

#get subsection of table and remove duplicate records
species_table <- unique(species_table[, c(1, 4, 5)])

head(species_table)

##write to file
write.csv(species_table, file="./Data/processed/KW_data_rounded_to_0.041.csv")


