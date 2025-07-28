#function to load in raster files and crop them to a single estuary shapefile

crop.estuary <- function(files = pr.2002, shape = fre) {
  
  SUB <- list()
  
  for(i in 1:length(files)) {
    
    r <- rast(files[i])
    c <- crop(r, shape)
    c <- mask(c, shape)
    
    SUB[[i]] <- c
    
  }
  
  rast(SUB)
  
}