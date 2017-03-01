current.bioclim     <- 'F:/3sClimate/'
for (tfile in list.files(current.bioclim,pattern='\\.flt',full.name=TRUE)) {
  tasc              <- raster(tfile) #read in the data
  dataname          <- gsub(current.bioclim,'',tfile);
  dataname          <- gsub('\\.flt','',dataname);
  dataname          <- gsub('/','',dataname)
  print(dataname)
  writeRaster(tasc, paste(tfile, ".asc", sep=""), format="ascii")
}