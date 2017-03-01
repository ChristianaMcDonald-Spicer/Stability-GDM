library(raster)
library(ggplot2)
library(rasterVis)

folder <- "//franklin.uds.anu.edu.au/home/u5596907/My Documents/GDM fitting/Input Data/past layers/"
bioclim.layer <- "bioclim_10"
study_extent <- c(111, 156, -25.5, -9.008331)
time_periods <- c("004", "008", "012", "016", "020", "024","028", "032", 
                  "036", "040", "044", "048", "052", "056", "060", "064", 
                  "068", "072", "076", "080", "084", "088", "092", "096", "100", "108", "112", "116", "120")


bioclim <- list()
for (i in time_periods){
  bioclim[i] <- list.files(path = paste0(folder, i, "/"), pattern = bioclim.layer, 
                           all.files=TRUE, full.names=TRUE)
}
bioclim <- unlist(bioclim)
bioclim <- sort(bioclim)
stack_bioclim <- stack(bioclim)
stack_bioclim <- crop(stack_bioclim, study_extent)


present_bioclim <- raster("//franklin.uds.anu.edu.au/home/u5596907/My Documents/GDM fitting/Input Data/present_layers/bioclim_10.asc")
present_bioclim <- crop(present_bioclim, study_extent)



maximum <- maxValue(stack_bioclim)
maximum_present <- maxValue(present_bioclim)
max_limit <- max(c(maximum, maximum_present))


minimum <- minValue(stack_bioclim)
minimum_present <- minValue(present_bioclim)
min_limit <- min(c(minimum, minimum_present))

# pdf("bioclim12.pdf", width = 10, height = 10)
# #Also uses inches.
# 
# gplot(present_bioclim)+ geom_tile(aes(fill = value)) +
#   scale_colour_gradient(limits=c(min_limit, max_limit))+
#   coord_equal()
# 
# gplot(stack_bioclim)+ geom_tile(aes(fill = value)) +
#   scale_colour_gradient(limits=c(min_limit, max_limit))
# 
# dev.off()



pdf("bioclim10.pdf", width = 10, height = 10)
#Also uses inches.

plot(present_bioclim, col=rev( rainbow( 99, start=0,end=1 ) ), 
     breaks=seq(min_limit,max_limit,length.out=100))
for (i in 1:29){
plot(stack_bioclim[[i]], col=rev( rainbow( 99, start=0,end=1 ) ), 
     breaks=seq(min_limit,max_limit,length.out=100))
}

dev.off()
