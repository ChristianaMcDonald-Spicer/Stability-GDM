require(plyr)

#load in data
sppTab <- read.csv("./Data/processed/KW_data_rounded_to_0.041.csv", header=TRUE, sep=",")
head(sppTab)

#add column of number of species per site
sppTab.count <- ddply(sppTab, (c("x", "y")), mutate, count = length(unique(species)))

#remove duplicate sites
sppTab.unique <- unique(sppTab.count)

#plot
hist(sppTab.unique$count, xlab = "Species per site", main="Species richness per site")

histinfo <- hist(sppTab.unique$count)
histinfo

head(sppTab.unique)
write.csv(sppTab.unique, "./Output/species_rich.csv")






sppTab2 <- read.csv("./Output/species_rich_6above.csv", header=TRUE, sep=",")
length(unique(c(sppTab2$x, sppTab2$y)))
coordinates(sppTab2) <- c("x", "y")
plot(sppTab2)


length(unique(c(sppTab$Lon, sppTab$Lat)))
