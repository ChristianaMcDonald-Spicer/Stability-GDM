library(raster)

template <- raster("./Data/PaleoClimate/OZ.climates/bioclim/000/bioclim_01.asc")

x <- "SLPRELIEF"
orig <- raster(paste0("./Data/SOILS-2016-06-09/SOILS/", x, ".flt"))
plot(orig)
new <- resample(orig, template, method="bilinear")
plot(new)
new
writeRaster(new, paste0("./Data/processed/enviro_layers/", x, "_resampled.asc"), bylayer=TRUE, format="ascii")







elev <- raster("//franklin.uds.anu.edu.au/home/u5596907/My Documents/Shape Files/DEM_elev_bathym_1minute/etopo1_aust_and_seas.asc")

elev.max <- aggregate(elev, fact=sqrt(2.5), fun=(max))
elev.min <- aggregate(elev, fact=sqrt(2.5), fun=(min))
elev.diff <- overlay(elev.max, elev.min, fun=(diff))
elev.new <- resample(elev.diff, template, method="bilinear")
writeRaster(elev.new, paste0("./Data/processed/enviro_layers/", "elevation", "_resampled.asc"), bylayer=TRUE, format="ascii")
