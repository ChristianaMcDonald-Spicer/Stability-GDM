##Create paleoSDMs and stability models using maxent
#Christiana McDonald-Spicer
#Code originally written by Jeremy VanDerWal and Dan Rosauer
#February 2017

############################## Step 1 - Read in the libraries ##############################

rm(list=ls())
library(SDMTools)
library(raster)
library(readxl)
library(maptools)

work.dir  <-      'C:/SDM_Stability_models/'; setwd(work.dir)

############################## Step 2 - Set up the functions ##############################
###need to change file paths in first part of each function

####Function 1 - take csv occurrence data and turn into rasters for maxent
occ_grid <- function(species){
  
  ###set up values
  work.dir                  <- paste(getwd(), "/", sep="")
  template                  <-  'C:/PaleoClimate/OZ.climates/bioclim/000/landmask.asc'
  occ_sites_file            <- 'C:/SDM_Stability_models/Data/KW_data_rounded_to_0.041_for_maxent.csv'
  
  radius_m                  <-  400000  # metres
  buffer                    <-  0.5     # degrees
  output_mask_grid          <-  paste(work.dir, species, '_', radius_m, '_buf.asc', sep="")
  output_occ_grid           <-  paste(work.dir, species, '_', radius_m, '_occ.asc', sep="")
  
  # a buffer grid not clipped to the coast - to allow predictions off the current coastline
  output_mask_grid_nocoast  <-  paste(work.dir, species, '_', radius_m, '_buf_nocoast.asc', sep="")
  do_nocoast                <-  T
  
  #define some basic data
  template.ras              <-  raster(template)
  projection(template.ras)  <-  CRS("+proj=longlat +datum=WGS84")
  all_occ                   <-  read.csv(occ_sites_file)
  occ_sites_df              <-  all_occ[all_occ$species == species,] #where species=species
  
  #create a spatial points data frame
  occ_sites                 <-  SpatialPointsDataFrame(occ_sites_df[, c("x", "y") ], data=occ_sites_df, proj4string = CRS("+proj=longlat +datum=WGS84"))
  rm(occ_sites_df)
  
  #write occurrence data to shape file
  writeSpatialShape(occ_sites, species)
  proj.file <- file(paste(species,".prj", sep=""))
  writeLines(c(
    "Projection GCS_WGS_1984",
    "Datum D_WGS_1984",
    "Spheroid WGS_1984,6378137.0,298.257223563",
    "PrimeM Greenwich",
    "Units Degree,0.0174532925199433"
  ), proj.file)
  close(proj.file)
  
  
  # calculate euclidian distance from the sites to make a mask grid
  template.ras              <-  raster(template)
  distlayer.ras             <-  distanceFromPoints(template.ras, occ_sites)
  distlayer.ras[which(distlayer.ras[] > radius_m)]   <-  NA
  distlayer.ras[which(! is.na(distlayer.ras[]))]     <-  1
  
  if(do_nocoast) {
    distlayer_nocoast.ras   <-  distlayer.ras
    # further handling for no_coast below
  }
  
  distlayer.ras             <-  mask(distlayer.ras, template.ras)
  
  # now find the limits of non NA data
  colNotNA                  <-  which(colSums(distlayer.ras, na.rm = T) != 0)
  rowNotNA                  <-  which(rowSums(distlayer.ras, na.rm = T) != 0)
  minX                      <-  xFromCol(distlayer.ras, min(colNotNA)) - buffer
  maxX                      <-  xFromCol(distlayer.ras, max(colNotNA)) + buffer
  maxY                      <-  yFromRow(distlayer.ras, min(rowNotNA)) + buffer
  minY                      <-  yFromRow(distlayer.ras, max(rowNotNA)) - buffer
  newExtent                 <-  extent(minX, maxX, minY, maxY)
  distlayer.ras             <-  crop(distlayer.ras, newExtent)
  plot(distlayer.ras)
  
  # write the mask raster
  writeRaster(distlayer.ras, output_mask_grid, overwrite=T)
  
  # create the occurrence raster
  distlayer.ras[! is.na(distlayer.ras[])]         <-  0
  occlayer.ras              <-  rasterize(occ_sites, distlayer.ras, field=1, update=T)
  
  # write the occ raster
  writeRaster(occlayer.ras, output_occ_grid, overwrite=T)
  
  if(do_nocoast) {
    # now find the limits of non NA data
    colNotNA                <- which(colSums(distlayer_nocoast.ras, na.rm = T) != 0)
    rowNotNA                <- which(rowSums(distlayer_nocoast.ras, na.rm = T) != 0)
    minX                    <- xFromCol(distlayer_nocoast.ras, min(colNotNA)) - buffer
    maxX                    <- xFromCol(distlayer_nocoast.ras, max(colNotNA)) + buffer
    maxY                    <- yFromRow(distlayer_nocoast.ras, min(rowNotNA)) + buffer
    minY                    <- yFromRow(distlayer_nocoast.ras, max(rowNotNA)) - buffer
    newExtent               <- extent(minX, maxX, minY, maxY)
    distlayer_nocoast.ras   <- crop(distlayer_nocoast.ras, newExtent)
    # write the mask raster with no coast limit
    writeRaster(distlayer_nocoast.ras, output_mask_grid_nocoast, overwrite=T)
    plot(distlayer_nocoast.ras)
  }
}

####Function 2 - prepare and run maxent inputs
run_maxent <- function(species){
  
  #define directories
  work.dir            <- paste(getwd(), "/", sep="")
  current.bioclim     <- 'C:/PaleoClimate/OZ.climates/bioclim/000/'
  maxent.jar          <- 'C:/maxent/maxent.jar'
  presence_grid       <- paste(work.dir, species, "_4e+05_occ.asc", sep="")
  mask_layer_name     <- "landmask"
  species_name        <- species
  output_folder_name  <- paste('maxent.output_', species_name, sep="")
  
  #define some basic data
  pres.asc            <- read.asc(presence_grid)
  pres.asc[which(is.finite(pres.asc) & pres.asc!=1)] = 0  #set all other areas != 1 to 0
  maxent_threads      <- 9
  ymin                <- -25
  background_points   <- 2000
  
  #get a subset of the data for occur & background
  pos                 <- as.data.frame(which(is.finite(pres.asc),arr.ind=TRUE)) #get all points that have data
  xy                  <- getXYcoords(pres.asc)
  pos$long            <- xy$x[pos$row]
  pos$lat             <- xy$y[pos$col]
  pos$pres            <- pres.asc[cbind(pos$row,pos$col)] #append the area data
  
  # apply a southern limit
  pos                 <- pos[pos$lat >= ymin,]
  
  #pos.subset = pos[which(pos$row %% 2 == 0 & pos$col %% 2 == 0),] #get a subset of the data as a regular grid of every second cell (even row / col numbers)
  #pos.subset= pos  #this line is an alternative to the previous, which selects every 2nd cell
  
  all_background      <- which(pos$pres==0)
  bkgd_sample         <- sample(all_background, size = background_points)
  pos.subset          <- pos[bkgd_sample,]
  pos.subset          <- rbind(pos.subset,pos[which(pos$pres==1),])
  pos.subset          <- unique(pos.subset) #ensure all cells in the target area are included in dataset
  
  #append the current environmental data
  for (tfile in list.files(current.bioclim,pattern='\\.asc.gz',full.name=TRUE)) {
    tasc              <- read.asc.gz(tfile) #read in the data
    dataname          <- gsub(current.bioclim,'',tfile);
    dataname          <- gsub('\\.asc.gz','',dataname);
    dataname          <- gsub('/','',dataname)
    pos.subset[dataname] <- extract.data(data.frame(pos.subset$long,pos.subset$lat), tasc) #append the data
    cat ("\nLoaded:", dataname)
  }
  pos.subset          <- na.omit(pos.subset) #ensure there is no missing data
  
  #now remove the mask column (if any)
  if (exists("mask_layer_name")) {
    pos.subset[mask_layer_name] <- NULL
  }
  
  #define the occurrences & background ... then write out the data
  occur               <- data.frame(species=species_name, pos.subset[which(pos.subset$pres==1),])
  occur$pres          <- NULL; occur$row = NULL; occur$col = NULL
  bkgd                <- data.frame(species='bkgd',pos.subset)
  bkgd$pres           <- NULL; bkgd$row = NULL; bkgd$col = NULL
  
  write.csv(occur,'occur.csv',row.names=FALSE) #write out the occurrences
  write.csv(bkgd,'bkgd.csv',row.names=FALSE) #write out the background
  
  #run maxent
  dir.create(output_folder_name)
  sys_command         <- paste('java -mx2048m -jar ',maxent.jar,' -e bkgd.csv -s occur.csv -o', output_folder_name,'nothreshold nowarnings novisible -P jackknife -r -a') # variant with jacknifing for better info on variable importance
  
  # to run in parallel
  if (maxent_threads > 1) {
    sys_command       <- paste(sys_command, " threads=",maxent_threads,sep="")
  }
  
  system(sys_command)
  
}

####Function 3 - project using maxent
project_maxent <- function(species){
  
  #define directories
  work.dir            <- paste(getwd(), '/maxent.output_', species, "/", sep="")
  mxe.dir             <- 'C:/PaleoClimate/OZ.climates/mxe/'
  bioclim.dir         <- 'C:/PaleoClimate/OZ.climates/bioclim/'
  maxent.jar          <- 'C:/maxent/maxent.jar'
  mask_layer_name     <- paste(getwd(), "/", species, "_4e+05_buf_nocoast.asc", sep="")
  time_mask           <- 'landmask.asc.gz'
  species_name        <- species
  coast.shp           <- shapefile("C:/PaleoClimate/aus_1m.shp")
  
  #shape file
  coast.shp           <- coast.shp[coast.shp$NAM != "PAPUA NEW GUINEA" & coast.shp$NAM != "INDONESIA" & coast.shp$NAM !="CHRISTMAS ISLAND" & coast.shp$NAM !="COCOS (KEELING) ISLANDS" & coast.shp$NAM !="HEARD AND MCDONALD ISLANDS" & coast.shp$NAM !="NORFOLK ISLAND" & coast.shp$NAM !="LORD HOWE ISLAND" & coast.shp$NAM !="TASMANIA",]
  coast.shp           <- crop(coast.shp, raster(mask_layer_name))
  
  #clipping models to mask
  clip_to_mask = T
  clip_to_time_mask = T
  
  #list the projections, cycle thorugh them and project the models onto them
  proj.list           <- list.files(mxe.dir) #list the projections
  model_count         <- 0
  
  if (clip_to_mask) {mask.ras <- raster(mask_layer_name)}
  
  #cycle through the projections
  for (tproj in proj.list) {
    
    model_count       <- model_count+1
    cat("\nAbout to project model for year", tproj,"\n")
    
    #Original model projection
    maxent_call       <- paste('java -mx1024m -cp ',maxent.jar,' density.Project ', work.dir, species_name, '.lambdas ',mxe.dir,tproj,' ',work.dir,tproj,'.asc fadebyclamping nowriteclampgrid',sep="")
    
    #Modified model projection to test a model fitted with a restricted set of predictors
    #maxent_call = paste('java -mx1024m -cp ',maxent.jar,' density.Project ',work.dir,'maxent.output1/C.johnstonei.lambdas ',mxe.dir,tproj,' ',work.dir,"/maxent.output1/",tproj,'.asc fadebyclamping nowriteclampgrid',sep="")
    
    cat(maxent_call,"\n")
    system(maxent_call) #run a model
    model.name        <- paste(work.dir,tproj,".asc",sep="")
    mod.ras           <- raster(model.name)
    
    if (clip_to_mask) {
      mod.ras         <- crop(mod.ras, mask.ras)
      mod.ras         <- mask(mod.ras, mask.ras)
      writeRaster(mod.ras,model.name,overwrite=TRUE)
    }
    
    if (clip_to_time_mask) {
      #mod.ras <- crop(mod.ras, time_mask.ras)
      time_mask_file  <- paste(bioclim.dir, tproj, "/", time_mask, sep="")
      #if(!file.exists(time_mask_file)) {next}
      time_mask.asc   <- read.asc.gz(time_mask_file)
      time_mask.ras   <- raster.from.asc(time_mask.asc)
      time_mask.ras   <- crop(time_mask.ras, mod.ras)
      mod.ras         <- mask(mod.ras, time_mask.ras)
      writeRaster(mod.ras,model.name,overwrite=TRUE)
    }
    
    #plot(mod.ras,main=tproj)
    #plot(coast.shp,lwd=0.6, border = "blue", pbg="transparent", add=T)
  }
  
}

####Function 4 - calculate stability from sdms
sdm_stability <- function(species){
  
  #Define some directories
  work.dir          <- paste(getwd(), '/maxent.output_', species, "/", sep="")
  out.dir           <- paste(getwd(), '/stability_80ky_', species, "/", sep="")
  dir.create(out.dir)
  
  Oz.shape_path     <-"C:/PaleoClimate/aus_1m.shp"
  
  # species sites, for illustration
  points.shapefile  <- paste(getwd(), "/", species, ".shp", sep="")
  plot_points       <- T
  
  # plot the present day model, on which the paleo models are based
  plot_present_SDM  <- T
  plot_region_boundary <- F
  
  # restrict the date range for the models
  sim_count_max     <- 52
  if (plot_points) {
    points.shp      <- shapefile((points.shapefile))
  } else {
    points.shp      <- ""
  }
  
  #list the projections
  sims              <- list.files(path = work.dir, pattern = "*.asc$")
  sims              <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(sims))
  
  if (exists("sim_count_max")) {
    sims            <- sims[1:sim_count_max]
  }
  
  #define the cell size
  cell.size         <- 0.04166667
  
  #zero offset as cost of 1/value can produce Inf
  zero.offset       <- 1e-7
  
  #define the dispersal cost type "no.cost", "linear", "quadratic"
  disp.type         <- "linear"
  
  #define some plot info
  tcols             <- c(colorRampPalette(c('green','brown'))(14),colorRampPalette(c('brown','yellow'))(70),colorRampPalette(c('yellow','orange'))(119),colorRampPalette(c('orange','red'))(197))
  legend.pnts       <- cbind(c(113,114.5,114.5,113),c(-44,-44,-38,-38))
  Oz.shape          <- readShapePoly(Oz.shape_path)
  
  # display a progressive screen plot as it runs?
  plot.live = FALSE  # this is informative, but SLOWER
  
  #define function to create images
  image.grid        <- function(tasc, tfile, zlim=NULL, cols=NULL, plot_points=F, points=NULL) {
    if (is.null(cols)) { cols = colorRampPalette(c('green','brown','yellow','orange','red'))(101) }
    
    #plot the image
    png(tfile,height=847/100,width=1080/100,units='cm',res=300,pointsize=5)
    layout(matrix(c(rep(rep(1,8),6),1,2,2,2,2,1,1,1),nr=7,nc=8,byrow=TRUE))
    par(mar=c(0,0,0,0),cex=1)
    if (is.null(zlim)) { zlim = range(as.vector(tasc),na.rm=T) }
    image(tasc,ann=FALSE,axes=FALSE,zlim=zlim,col=cols)
    
    #legend location is hard coded here
    legend.gradient(cbind(c(123, 123.7, 123.7, 123),c(-11.5,-11.5,-10,-10)),limits=round(zlim,2),title='stability',cols=cols,cex=1.2)
    plot(Oz.shape,add=T,border="black",pbg="transparent",lwd=0.6) #add the subregions
    
    if (plot_region_boundary) {
      plot(IBRA.shape,add=T,border="blue",pbg="transparent",lwd=0.6) #add the subregions
    }
    
    if(plot_points) {
      plot(points.shp, add=T, col="black", pch=20) #plot the species points
    }
    dev.off()
  }
  
  #define function to calculate the moving window cost
  mw.cost           <- function(n) {
    #define the size of the moving window
    max.dist.moved  <- mdd * n
    max.num.cells   <- round(max.dist.moved / cell.size); cat('max radius in cells is',max.num.cells,'\n')
    #create the moving window
    tt              <- matrix(NA,nrow=max.num.cells*2+1,ncol=max.num.cells*2+1)
    #populate the distances
    for (y in 1:(max.num.cells*2+1)){
      for (x in 1:(max.num.cells*2+1)){
        tt[y,x]     <- sqrt((max.num.cells+1-y)^2 + (max.num.cells+1-x)^2)
      }
    }
    #remove distance values > max.num.cells
    tt[which(tt>max.num.cells)]=NA
    #define the dispersal costs
    if (disp.type=="no.cost") {
      tt[which(is.finite(tt))]=1; tt = -log(tt)
    } else if (disp.type=="linear") {
      tt = 1-tt/max.num.cells; tt = -log(tt); tt[which(is.infinite(tt))] = -log(zero.offset)
    } else if (disp.type=="quadratic") {
      tt = (1-tt/max.num.cells)^2; tt = -log(tt); tt[which(is.infinite(tt))] = -log(zero.offset)
    }	else {exit} #Error... need to define dispersal type
    return(tt)
  }
  
  #read in and store all the data
  indata            <- NULL
  for (ii in 1:length(sims)) {
    sim             <- sims[ii]
    cat(sim,'\n')
    tasc            <- read.asc(paste(work.dir, sim, ".asc", sep='')) #read in the data
    #create an array to store the data and setup the base.asc object
    if (is.null(indata)) { indata = array(data=NA,dim=c(dim(tasc),length(sims))); base.asc = tasc; base.asc = base.asc*0 } #base.asc is for current
    indata[,,ii]    <- tasc #store the data
  }
  
  ###create the data associated with the cost of the predicted environmental suitability
  cost.suitability  <- -log(indata)
  #set any Infinite values to our maximim y
  cost.suitability[which(is.infinite(cost.suitability))] <- -log(zero.offset)
  ###for calculating suitability / cost... we need to account for 'offshore' information
  #to deal with this, we need to grab the maximum extent of the data and set any NA within that to 0 suitability
  #that way, it will have HUGE cost and thus
  pos = NULL #setup object to track largest set of values
  for (ii in 1:length(sims)) { cat(sims[ii],'\n'); # extract the maximum set of points
    if (is.null(pos)) {
      pos           <- which(is.finite(cost.suitability[,,ii]),arr.ind=TRUE)
    } else {
      if (nrow(pos) < length(which(is.finite(cost.suitability[,,ii])))) {
        pos = which(is.finite(cost.suitability[,,ii]),arr.ind=TRUE)
      }
    }
  }
  pos               <- as.data.frame(pos) #convert to a dataframe
  for (ii in 1:length(sims)) { cat(sims[ii],'\n') #cycle through, change NA's associated with pos to value of -log(zero.offset) cost
    tt              <- cost.suitability[,,ii][cbind(pos$row,pos$col)]
    tt              <- which(is.na(tt)) #define the NA positions
    cost.suitability[,,ii][cbind(pos$row[tt],pos$col[tt])] = -log(zero.offset) #set those NA's to -log(zero.offset)
  }
  #define the output data to store outputs
  outdata           <- cost.suitability
  
  #set up to do a live plot
  if (plot.live) {windows(10,10)}

  ### calculate the OLD static predictions
  min.asc = mean.asc = base.asc
  
  #sum the predictions to get the average
  for (ii in 1:length(sims)){
    cat(sims[ii],'\n')
    min.asc         <- pmax(min.asc,cost.suitability[,,ii],na.rm=T)
    mean.asc        <- mean.asc + cost.suitability[,,ii]
    
    # a progressive plot of the mean suitability so far, if requested
    if(plot.live) {
      plot(raster.from.asc(exp(-(mean.asc/ii))),main=paste("Static stability calculated so far, from years 000 to",sims[ii]))
    }
  }
  
  mean.asc          <- mean.asc / length(sims) #calculate the mean value
  
  #convert back to maxent values of 0,1
  mean.asc          <- exp(-mean.asc)
  min.asc           <- exp(-min.asc)
  write.asc(min.asc,paste(out.dir,'static.min.asc',sep=''))#write out the data
  write.asc(mean.asc,paste(out.dir,'static.mean.asc',sep=''))#write out the data
  bins              <- seq(0,1,length=101); bins = cut(0.0242,bins,labels=FALSE) # get the threshold bin for cols
  cols              <- c(rep('gray',bins),colorRampPalette(c('brown','yellow','forestgreen'))(100)[bins:100])
  image.grid(min.asc,paste(out.dir,'static.min.png',sep=''), zlim=c(0,1), cols=cols, plot_points=plot_points, points=points.shp) #plot the image
  image.grid(mean.asc,paste(out.dir,'static.mean.png',sep=''), zlim=c(0,1), cols=cols, plot_points=plot_points, points=points.shp) #plot the image
  
  ## calculate the shifting refugia
  
  #define cols
  tcol              <- c(colorRampPalette(c('gray30','grey80'))(51),colorRampPalette(c('grey80','yellow','red','green','blue','saddlebrown'))(50))
  
  # calculate the static prediction
  static.asc        <- base.asc; static.asc[,] = cost.suitability[,,1] #set the static.asc to the first cost surface
  #sum the predictions to get the average
  for (ii in 2:length(sims)){
    cat(sims[ii],'\n')
    static.asc      <- static.asc + cost.suitability[,,ii]
  }
  static.asc        <- static.asc / (length(sims)*2-1); static.asc = exp(-static.asc)
  write.asc(static.asc,paste(out.dir,'static.sum.cost.asc',sep=''))#write out the data
  image.grid(static.asc,paste(out.dir,'static.sum.cost.png',sep=''), zlim=c(0,1), cols=tcol, plot_points=plot_points, points=points.shp) #plot the image after logging the actual data
  
    ### 10 m/ year
  #define the max dispersal distance in units per year (defined by resolution of your inputs)
  #e.g., if cell size is 0.002998 decimal degrees (~250m resolution) and you want 10 m dispersal distance per year
  #set mdd = 10 * 0.002998 / 250
  mdd.id            <- 10 #m/year
  mdd               <- mdd.id * cell.size / 4000
  outdata           <- cost.suitability #reset the output data
  
  #set up to do a live plot
  if (plot.live) {windows(10,10)}
  
  #calculate the stability surfaces
  for (ii in (length(sims)-1):1){  #cycle through each of the layers starting with the last
    cat(sims[ii],'...')
    #define the size of the moving window
    num.years       <- (as.numeric(sims[ii+1]) - as.numeric(sims[ii])) * 1000
    mw              <- mw.cost(num.years)
    #workout and store in outdata
    outdata[,,ii]   <- cost.suitability[,,ii] + lcmw(outdata[,,ii+1],mw,round((mdd * num.years) / cell.size))
    
    # a progressive plot of the mean suitability so far, if requested
    if(plot.live) {
      plot(raster(exp(-(outdata[,,ii]/(ii*2-1)))),main=paste("Dymanic stability (",mdd.id,"/yr calculated so far, from years 000 to",sims[ii]))
    }
  }
  
  #create the plot & ascii output...
  tasc              <- base.asc; tasc[,] = outdata[,,1] #get the data
  tasc[which(tasc+1>1e20)] = NA #remove the Bullshite data
  tasc              <- tasc / (length(sims)*2-1); tasc = exp(-tasc)
  write.asc(tasc,paste(out.dir,'shift.',mdd.id,'.asc',sep='')) #write out the ascii grid file
  image.grid(tasc,paste(out.dir,'shift.',mdd.id,'.png',sep=''), zlim=c(0,1), cols=tcol, plot_points=plot_points, points=points.shp) #plot the image after logging the actual data
  
  ### 20 m/ year
  #define the max dispersal distance in units per year (defined by resolution of your inputs)
  #e.g., if cell size is 0.002998 decimal degrees (~250m resolution) and you want 10 m dispersal distance per year
  #set mdd = 10 * 0.002998 / 250
  mdd.id            <- 20 #m/year
  mdd               <- mdd.id * cell.size / 4000
  outdata           <- cost.suitability #reset the output data
  
  #set up to do a live plot
  if (plot.live) {windows(10,10)}
  
  #calculate the stability surfaces
  for (ii in (length(sims)-1):1){ cat(sims[ii],'...') #cycle through each of the layers starting with the last
    #define the size of the moving window
    num.years         <- (as.numeric(sims[ii+1]) - as.numeric(sims[ii])) *1000
    mw                <- mw.cost(num.years)
    #workout and store in outdata
    outdata[,,ii]     <- cost.suitability[,,ii] + lcmw(outdata[,,ii+1],mw,round((mdd * num.years) / cell.size))
    # a progressive plot of the mean suitability so far, if requested
    if(plot.live) {
      plot(raster(exp(-(outdata[,,ii]/(ii*2-1)))),main=paste("Dynamic stability (",mdd.id,"m/yr calculated so far, from years 000 to",sims[ii]))
    }
  }
  #create the plot & ascii output...
  tasc                <- base.asc
  tasc[,]             <- outdata[,,1] #get the data
  tasc[which(tasc+1>1e20)] = NA #remove the Bullshite data
  tasc                <- tasc / (length(sims)*2-1)
  tasc                <- exp(-tasc)
  write.asc(tasc,paste(out.dir,'shift.',mdd.id,'.asc',sep='')) #write out the ascii grid file
  image.grid(tasc, paste(out.dir,'shift.',mdd.id,'.png',sep=''), zlim=c(0,1), cols=tcol, plot_points=plot_points, points=points.shp) #plot the image after logging the actual data
  
  if (plot_present_SDM) {
    sim               <- sims[1]
    tasc              <- read.asc(paste(work.dir, sim, ".asc", sep=''))
    image.grid(tasc, paste(out.dir, 'now_SDM', '.png', sep=''), zlim=c(0,1), cols=cols, plot_points=plot_points, points=points.shp)
    
  }
}

############################## Step 3a - Run the function for one species ##############################

test_species <- "Amalosia_obscura"
occ_grid(test_species)
run_maxent(test_species)
project_maxent(test_species)
sdm_stability(test_species)

############################## Step 3b - Loop through and run for all species ##############################

data_file           <- read.csv('C:/SDM_Stability_models/Data/KW_data_rounded_to_0.041_for_maxent.csv')
list_species <- unique(data_file$species)
list_species
list_species_subset <- list_species[287:335]
list_species_subset

for (i in list_species_subset) {
  occ_grid(i)
  run_maxent(i)
  project_maxent(i)
  sdm_stability(i)
  
  #print to the console
  cat(paste("Completed species ", i), sep="\n")
}