##'this script includes all code for downloading and cropping data from CHELSA
##'(https://chelsa-climate.org/downloads/). this script downloaded monthly precipitation
##'and average temperature data for the world, then cropped it to the study area. due to
##'memory constraints, these were downloaded and cropped one by one. 

#load in packages
library('terra') #for spatial data
library('dplyr')
library('sf') #for shapefiles

setwd("C:/Users/rekhamarcus/OneDrive - University of Victoria/Wetlands")

pnw <- readRDS('Data/PNW_shapefile.rds') #study area shapefile
pnw <- st_transform(pnw, crs = st_crs("EPSG:4326")) #reproject to be able to crop data

options(timeout = max(1000, getOption("timeout"))) #set higher timeout to allow time for file to download

#precipitation data download ---------------------------------------------------

#read in list of files
precip.files <- read.delim('Downloads/envidatS3paths_pr.txt', header = FALSE) #file containing links to all data files from chelsa

precip.files.list <- split(precip.files, seq(nrow(precip.files))) #convert to list of file urls

precip.files.urls <- list()
 for(i in 1:length(precip.files.list)){ #clean up list elements
   url <- gsub('i ', '', precip.files.list[[i]])
   url <- gsub(' ','', url)
   
   precip.files.urls[[i]] <- url
}

#for loop to download, crop, save each file

for(i in 1:length(precip.files.urls)){
  
  #find specific filename
  filename <- substr(precip.files.urls[[i]],
                     start = nchar(precip.files.urls[[i]]) - nchar('CHELSA_pr_mm_yyyy_V.2.1.tif') + 1,
                     stop = nchar(precip.files.urls[[i]]) - nchar('f') + 1)
  
  #download file
  download.file(precip.files.urls[[i]], destfile = paste0("Data/Precipitation/CHELSA/",paste(filename), sep = ''), mode = "wb")
  
  #import as raster
  r <- rast(paste0("Data/Precipitation/CHELSA/",paste(filename), sep = ''))
  
  #crop to pnw
  c <- crop(r, pnw)
  c <- mask(c, pnw)
  
  #save to overwrite original file
  writeRaster(c, paste0("Data/Precipitation/CHELSA/",paste(filename), sep = ''), overwrite = TRUE)
  
  #clean up environment
  rm(r)
  rm(c)
  
  Sys.sleep(10)
  
  print(i)
}

#temperature data download -------------------------------------------------

#read in list of files
temp.files <- read.delim('Downloads/envidatS3paths_tas.txt', header = FALSE)

temp.files.list <- split(temp.files, seq(nrow(temp.files))) #convert to list of file urls

temp.files.urls <- list()
for(i in 1:length(temp.files.list)){ #clean up list elements
  url <- gsub('i ', '', temp.files.list[[i]])
  url <- gsub(' ','', url)
  
  temp.files.urls[[i]] <- url
}

#for loop to download, crop, save each file

for(i in 1:length(temp.files.urls)){
  
  filename <- substr(temp.files.urls[[i]],
                     start = nchar(temp.files.urls[[i]]) - nchar('CHELSA_tas_mm_yyyy_V.2.1.tif') + 1,
                     stop = nchar(temp.files.urls[[i]]) - nchar('f') + 1)
  
  #download file
  download.file(temp.files.urls[[i]], destfile = paste0("Data/Temperature/CHELSA/",paste(filename), sep = ''), mode = "wb")
  
  #import as raster
  r <- rast(paste0("Data/Temperature/CHELSA/",paste(filename), sep = ''))
  
  #crop to pnw
  c <- crop(r, pnw)
  c <- mask(c, pnw)
  
  #save to overwrite original file
  writeRaster(c, paste0("Data/Temperature/CHELSA/",paste(filename), sep = ''), overwrite = TRUE)
  
  #clean up environment
  rm(r)
  rm(c)
  
  Sys.sleep(10)
  
  print(i)
}
