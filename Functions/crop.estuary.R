#function to load in raster files and crop them to a single estuary shapefile

crop.estuary <- function(files = pr.2002, shape = fre) {
  
  SUB <- list()
  
  for(i in 1:length(files)) {
    
    possibleError <- tryCatch( #some files missing data
      r <- rast(files[i]),
      error=function(e) e)
    
    if(inherits(possibleError, "error")) next
      
    c <- crop(r, shape)
    c <- mask(c, shape)
    
    SUB[[i]] <- c
    
  }
  
  #remove files with no data - thsi was removing all files, something was weird
  #SUB <- SUB[format(SUB) == "<S4 class ‘SpatRaster’ [package “terra”] with 1 slot>"]
  
  rast(SUB)
  
  
}