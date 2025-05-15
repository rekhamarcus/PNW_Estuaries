#subsetting/wrangling data

library('dplyr')
library('terra')
library('sf')

setwd("C:/Users/rekhamarcus/OneDrive - University of Victoria/Wetlands")

estuaries <- readRDS('Data/Shapefiles/estuaries.rds') #estuaries shapefile
estuaries <- st_transform(estuaries, crs = st_crs("EPSG:4326")) #reproject to be able to crop data

pr.files <- list.files(path = 'Data/Precipitation/CHELSA_monthly_timeseries_historical/',
                                        pattern = '.tif', full.names = TRUE)
  
tas.files <- list.files(path = 'Data/Temperature/CHELSA_monthly_timeseries_historical/',
                                         pattern = '.tif', full.names = TRUE)

#crop historical data to estuaries only

#precipitation
for(i in 2:length(pr.files)){
  
  r <- rast(pr.files[i])
  
  c <- crop(r, estuaries)
  
  writeRaster(c, pr.files[i], overwrite = TRUE)
  
  print(i)
  
}

#temperature
for(i in 1:length(tas.files)){
  
  r <- rast(tas.files[i])
  
  c <- crop(r, estuaries)
  
  writeRaster(c, tas.files[i], overwrite = TRUE)
  
  print(i)
  
}

#subset data to test modelling

# 1 year of historic data, 1 estuary

# 1 month of historic data for all years, 1 estuary

# projection data for 1 estuary
  
  
  
  