##'this script includes all code for downloading and cropping data from CHELSA
##'(https://chelsa-climate.org/downloads/). this script downloaded historical world 
##'monthly precipitation and temperature and projected world changes in temperature,
##'precipitation, and seasonality in these variables, then cropped it to the study 
##'area. due to memory constraints, these were downloaded and cropped one by one. 

#load in packages
library('terra') #for spatial data
library('dplyr')
library('sf') #for shapefiles

setwd("C:/Users/rekhamarcus/OneDrive - University of Victoria/Wetlands")

pnw <- readRDS('Data/PNW_shapefile.rds') #study area shapefile
pnw <- st_transform(pnw, crs = st_crs("EPSG:4326")) #reproject to be able to crop data

estuaries <- readRDS('Data/Shapefiles/estuaries.rds')
estuaries <- st_transform(estuaries, crs = st_crs("EPSG:4326"))

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

#temp/precip seasonality projection download---------------------------------------

#temperature seasonality projection data (bio4)

#read in files
bio4.files <- read.delim('Downloads/envidatS3paths_bio4.txt', header = FALSE)

bio4.files.list <- split(bio4.files, seq(nrow(bio4.files))) #convert to list of file urls

bio4.files.urls <- list()
for(i in 1:length(bio4.files.list)){ #clean up list elements
  url <- gsub('i ', '', bio4.files.list[[i]])
  url <- gsub(' ','', url)
  
  bio4.files.urls[[i]] <- url
}

#for loop to download, crop, save each file

for(i in 2:length(bio4.files.urls)){
  
  filename <- substr(bio4.files.urls[[i]],
                     start = nchar(bio4.files.urls[[i]]) - nchar('CHELSA_bio4_yyyy-yyyy_gfdl-esm4_sspxxx_V.2.1.tif') + 1,
                     stop = nchar(bio4.files.urls[[i]]) - nchar('f') + 1)
  
  #download file
  download.file(bio4.files.urls[[i]], destfile = paste0("Data/Temperature/CHELSA_seasonality_projections/",paste(filename), sep = ''), mode = "wb")
  
  #import as raster
  r <- rast(paste0("Data/Temperature/CHELSA_seasonality_projections/",paste(filename), sep = ''))
  
  #crop to pnw
  c <- crop(r, estuaries)
  c <- mask(c, estuaries)
  
  #save to overwrite original file
  writeRaster(c, paste0("Data/Temperature/CHELSA_seasonality_projections/",paste(filename), sep = ''), overwrite = TRUE)
  
  #clean up environment
  rm(r)
  rm(c)
  
  Sys.sleep(10)
  
  print(i)
}

#precipitation seasonality projection data (bio15)

#read in files
bio15.files <- read.delim('Downloads/envidatS3paths_bio15.txt', header = FALSE)

bio15.files.list <- split(bio15.files, seq(nrow(bio15.files))) #convert to list of file urls

bio15.files.urls <- list()
for(i in 1:length(bio15.files.list)){ #clean up list elements
  url <- gsub('i ', '', bio15.files.list[[i]])
  url <- gsub(' ','', url)
  
  bio15.files.urls[[i]] <- url
}

#for loop to download, crop, save each file

for(i in 1:length(bio15.files.urls)){
  
  filename <- substr(bio15.files.urls[[i]],
                     start = nchar(bio15.files.urls[[i]]) - nchar('CHELSA_bio15_yyyy-yyyy_gfdl-esm4_sspxxx_V.2.1.tif') + 1,
                     stop = nchar(bio15.files.urls[[i]]) - nchar('f') + 1)
  
  #download file
  download.file(bio15.files.urls[[i]], destfile = paste0("Data/Precipitation/CHELSA_seasonality_projections/",paste(filename), sep = ''), mode = "wb")
  
  #import as raster
  r <- rast(paste0("Data/Precipitation/CHELSA_seasonality_projections/",paste(filename), sep = ''))
  
  #crop to pnw
  c <- crop(r, estuaries)
  c <- mask(c, estuaries)
  
  #save to overwrite original file
  writeRaster(c, paste0("Data/Precipitation/CHELSA_seasonality_projections/",paste(filename), sep = ''), overwrite = TRUE)
  
  #clean up environment
  rm(r)
  rm(c)
  
  Sys.sleep(10)
  
  print(i)
}

#temperature projection data download -----------------------------------------

#read in files
tas.proj.files <- read.delim('Downloads/envidatS3paths_tas_proj.txt', header = FALSE)

tas.proj.files.list <- split(tas.proj.files, seq(nrow(tas.proj.files))) #convert to list of file urls

tas.proj.files.urls <- list()
for(i in 1:length(tas.proj.files.list)){ #clean up list elements
  url <- gsub('i ', '', tas.proj.files.list[[i]])
  url <- gsub(' ','', url)
  
  tas.proj.files.urls[[i]] <- url
}

#for loop to download, crop, save each file

for(i in 1:length(tas.proj.files.urls)){
  
  filename <- substr(tas.proj.files.urls[[i]],
                     start = nchar(tas.proj.files.urls[[i]]) - nchar('CHELSA_gfdl-esm4_r1i1p1f1_w5e5_sspxxx_tas_mm_yyyy_yyyy_norm.tif') + 1,
                     stop = nchar(tas.proj.files.urls[[i]]) - nchar('f') + 1)
  
  #download file
  download.file(tas.proj.files.urls[[i]], destfile = paste0("Data/Temperature/CHELSA_monthly_projections/",paste(filename), sep = ''), mode = "wb")
  
  #import as raster
  r <- rast(paste0("Data/Temperature/CHELSA_monthly_projections/",paste(filename), sep = ''))
  
  #crop to pnw
  c <- crop(r, estuaries)
  c <- mask(c, estuaries)
  
  #save to overwrite original file
  writeRaster(c, paste0("Data/Temperature/CHELSA_monthly_projections/",paste(filename), sep = ''), overwrite = TRUE)
  
  #clean up environment
  rm(r)
  rm(c)
  
  Sys.sleep(10)
  
  print(i)
}

#precipitation projection data download ---------------------------------------

#read in files
pr.proj.files <- read.delim('Downloads/envidatS3paths_pr_proj.txt', header = FALSE)

pr.proj.files.list <- split(pr.proj.files, seq(nrow(pr.proj.files))) #convert to list of file urls

pr.proj.files.urls <- list()
for(i in 1:length(pr.proj.files.list)){ #clean up list elements
  url <- gsub('i ', '', pr.proj.files.list[[i]])
  url <- gsub(' ','', url)
  
  pr.proj.files.urls[[i]] <- url
}

#for loop to download, crop, save each file

for(i in 1:length(pr.proj.files.urls)){
  
  filename <- substr(pr.proj.files.urls[[i]],
                     start = nchar(pr.proj.files.urls[[i]]) - nchar('CHELSA_gfdl-esm4_r1i1p1f1_w5e5_sspxxx_pr_mm_yyyy_yyyy_norm.tif') + 1,
                     stop = nchar(pr.proj.files.urls[[i]]) - nchar('f') + 1)
  
  #download file
  download.file(pr.proj.files.urls[[i]], destfile = paste0("Data/Precipitation/CHELSA_monthly_projections/",paste(filename), sep = ''), mode = "wb")
  
  #import as raster
  r <- rast(paste0("Data/Precipitation/CHELSA_monthly_projections/",paste(filename), sep = ''))
  
  #crop to pnw
  c <- crop(r, estuaries)
  c <- mask(c, estuaries)
  
  #save to overwrite original file
  writeRaster(c, paste0("Data/Precipitation/CHELSA_monthly_projections/",paste(filename), sep = ''), overwrite = TRUE)
  
  #clean up environment
  rm(r)
  rm(c)
  
  Sys.sleep(10)
  
  print(i)
}
