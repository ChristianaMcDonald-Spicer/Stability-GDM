#create a function
stability <- function(stack1, stack2) {
  #create new raster stack
  
  
  #loop through cells
  
  
  #perform calculation for each stack
  new.stack[[i]] <- abs(stack1[[i]]-stack2[[i]])
  
  
  #add all layers together
  added_raster <- ...
  
  #give the raster a name based on input
  assign(paste0("stabilityraster", stack2), added_raster)
  
}



#test by making fake rasters
df1 <- data.frame( x = rep( 0:1, each=2 ),
                  y = rep( 0:1,  2),
                  l = rep( 0:3,  1))

df2 <- data.frame( x = rep( 1:2, each=2 ),
                  y = rep( 0:1,  2),
                  l = rep( 1:4,  1))

df3 <- data.frame( x = rep( 5:6, each=2 ),
                  y = rep( 4:5,  2),
                  l = rep( 0:3,  1))

df4 <- data.frame( x = rep( 6:7, each=2 ),
                  y = rep( 0:1,  2),
                  l = rep( 4:7,  1))

rast1 <- as.raster(df1)
