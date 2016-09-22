library(raster)

##read in species occurrence data
sppTab <- read.csv("./Data/processed/reptile_AMT_rounded_to_0.041.csv", header=TRUE, sep=",")
head(sppTab)

GeoAge <- raster("./Data/processed/enviro_layers/GEOLMNAGE_resampled.asc")

sppTab_cropped <- sppTab[GeoAge]


library(raster)
library(rgeos)


gClip <- function(shp, bb){
  if(class(bb) == "matrix") b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
  else b_poly <- as(extent(bb), "SpatialPolygons")
  gIntersection(shp, b_poly, byid = T)
}

sppTab_cropped <- gClip(sppTab, GeoAge)


## Warning: spgeom1 and spgeom2 have different proj4 strings


plot(sppTab_cropped)
