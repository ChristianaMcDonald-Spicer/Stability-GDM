##crop to extent
study_extent <- c(111, 156, -25.5, -9.008331)
stab.rasters.crop <- crop(stab.rasters, study_extent)


##calculate range
stab_range <- calc(stab.rasters.crop, fun=function(x){max(x)-min(x)})
plot(stab_range, 
     col=rainbow(255))
title("Range of Dissimilarity")


##calculate arithmetic mean
stab_arithmean <- calc(stab.rasters.crop, fun=mean)
plot(stab_arithmean, 
     col=rainbow(255))
title("Arithmetic Mean of Dissimilarity")


##calculate standard deviation
stab_sdev <- calc(stab.rasters.crop, fun=sd)
plot(stab_sdev, 
     col=rainbow(255))
title("Standard Deviation of Dissimilarity")


##calculate geometric mean
##########DOES NOT WORK WITH NEGATIVE NUMBERS##
stab_geomean <- calc(stab.rasters.crop, fun=function(x){exp(mean(log(abs(x))))})
plot(stab_geomean, 
     col=rainbow(255))
title("Geometric Mean of Dissimilarity")


