library(raster)

AnnTemp <- raster("C:/Users/Anwyn/Documents/Christiana/bioclim_01.asc")

plot(AnnTemp)
lat <- lon <- AnnTemp
xy <- coordinates(AnnTemp)
lon[] <- xy[, 1]
lat[] <- xy[, 2]

plot(lat)
plot(lon)

writeRaster(lat, filename="latitude.asc", format="ascii")
writeRaster(lon, filename="longitude.asc", format="ascii")
