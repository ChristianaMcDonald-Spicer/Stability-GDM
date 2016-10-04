library(raster)

##create a function
stability.func <- function(stack1, stack2) {
  #create new raster stack
  new.stack <- stack1
  new.stack[] <- NA
  
  #loop through cells
    for (i in 1:ncell(new.stack)){
    #perform calculation for each stack
    new.stack[i] <- abs(stack1[i]-stack2[i]) 
  }
  
    #add all layers together
  added_raster <- sum(new.stack)

  
}


##test the function with fake rasters
df1 <- matrix(c(2, 5, 6, 8, 9, 4), nrow = 3, ncol=3)
df2 <- matrix(c(1, 3, 5, 1, 6, 4), nrow = 3, ncol=3)
df3 <- matrix(c(7, 8, 7, 2, 1, 3), nrow = 3, ncol=3)
df4 <- matrix(c(0, 1, 2, 4, 5, 8), nrow = 3, ncol=3)

rast1 <- raster(df1)
rast2 <- raster(df2)
rast3 <- raster(df3)
rast4 <- raster(df4)

stack.a <- stack(rast1, rast2)
stack.b <- stack(rast3, rast4)

test_raster <- stability.func(stack.a, stack.b)
View(test_raster)

##read in data
#folder name
data_folder <- "//franklin.uds.anu.edu.au/home/u5596907/My Documents/Identifying stability/Data/transformed rasters/"

#read in current time data
rasters <- list.files(path = data_folder, pattern = "present")
rasters <- sort(rasters)
rasters <- paste(data_folder, rasters, sep="")
rasters.present <- stack(rasters)

#read in past time data
rasters <- list.files(path = data_folder, pattern = "002kya")
rasters <- sort(rasters)
rasters <- paste(data_folder, rasters, sep="")
rasters.002 <- stack(rasters)

stability.002 <- stability.func1(rasters.present, rasters.002)



##create a function
stability.func1 <- function(stack1, stack2) {
  #create new raster stack
  new.stack <- stack1
  new.stack[] <- NA
  
  #perform calculation for each stack
    new.stack <- abs(stack1-stack2) 
  
  #add all layers together
  added_raster <- sum(new.stack)
  
  
}



test_raster <- stability.func1(stack.a, stack.b)
View(test_raster)




##extra bits of code
#give the raster a name based on input
assign(paste0("stabilityraster", deparse(substitute(stack.b))), added_raster)



