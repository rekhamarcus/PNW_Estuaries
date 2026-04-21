#bird data wrangling

library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(ggspatial)

setwd("C:/Users/rekhamarcus/OneDrive - University of Victoria/Wetlands")

#load in bird data

percentpop <- rast('Data/Bird_data/PacificBirds_all-bird-groups_all-species-priority_annual_percentage_population_annual.tif')
percentpop <- terra::project(percentpop, "epsg:4269") #may not need to be projected - this is messing with the data
relativeabun <- rast('Data/Bird_data/PacificBirds_all-bird-groups_all-species-priority_annual_relative_abundance_annual.tif')
relativeabun <- terra::project(relativeabun, "epsg:4269")
richness <- rast('Data/Bird_data/PacificBirds_all-bird-groups_all-species-priority_annual_richness_annual.tif')
richness <- terra::project(richness, "epsg:4269")

percentpop.hp <- rast('Data/Bird_data/PacificBirds_all-bird-groups_high-priority_annual_percentage_population_annual.tif')
percentpop.hp <- terra::project(percentpop.hp, "epsg:4269")
relativeabun.hp <- rast('Data/Bird_data/PacificBirds_all-bird-groups_high-priority_annual_relative_abundance_annual.tif')
relativeabun.hp <- terra::project(relativeabun.hp, "epsg:4269")
richness.hp <- rast('Data/Bird_data/PacificBirds_all-bird-groups_high-priority_annual_richness_annual.tif')
richness.hp <- terra::project(richness.hp, "epsg:4269")

#load estuary data

estuaries <- readRDS('Data/Shapefiles/estuaries.rds')
estuaries <- st_transform(estuaries, crs = st_crs("EPSG:4269"))

#extract bird data for each estuary ------------------------------------------

#set up dataframe

bird.df <- data.frame(EST_ID = NA,
                      percent.population = NA,
                      relative.abundance = NA,
                      richness = NA,
                      percent.population.hp = NA,
                      relative.abundance.hp = NA,
                      richness.hp = NA)
  
for(i in 1:nrow(estuaries)){
    
    #crop bird data to estuary
    c <- crop(percentpop, estuaries[i,])
    df <- as.data.frame(c, xy = TRUE)
    
    #extract data for estuary
    bird.df[i,1] <- estuaries$EST_ID[i]
    bird.df[i,2] <- mean(df[,3])
    
  }
  
for(i in 1:nrow(estuaries)){
  
  #crop bird data to estuary
  c <- crop(relativeabun, estuaries[i,])
  df <- as.data.frame(c, xy = TRUE)
  
  #extract data for estuary
  bird.df[i,3] <- mean(df[,3])
  
}

for(i in 1:nrow(estuaries)){
  
  #crop bird data to estuary
  c <- crop(richness, estuaries[i,])
  df <- as.data.frame(c, xy = TRUE)
  
  #extract data for estuary
  bird.df[i,4] <- max(df[,3])
  
}

for(i in 1:nrow(estuaries)){
  
  #crop bird data to estuary
  c <- crop(percentpop.hp, estuaries[i,])
  df <- as.data.frame(c, xy = TRUE)
  
  #extract data for estuary
  bird.df[i,5] <- mean(df[,3])
  
}

for(i in 1:nrow(estuaries)){
  
  #crop bird data to estuary
  c <- crop(relativeabun.hp, estuaries[i,])
  df <- as.data.frame(c, xy = TRUE)
  
  #extract data for estuary
  bird.df[i,6] <- mean(df[,3])
  
}

for(i in 1:nrow(estuaries)){
  
  #crop bird data to estuary
  c <- crop(richness.hp, estuaries[i,])
  df <- as.data.frame(c, xy = TRUE)
  
  #extract data for estuary
  bird.df[i,7] <- max(df[,3])
  
}

write.csv(bird.df, "Ranking/Data/PBHJV_bird_estuary_data.csv")

#find missing estuaries data ----------------------------------------------

pp.crop <- crop(percentpop, estuaries)
pp.df <- as.data.frame(pp.crop, xy = TRUE)

missing <- c(2023, 2022, 2106, 3019, 2025, 2026, 1017, 35, 69, 133, 151, 218, 236, 378, 391)
missing.estuaries <- estuaries[estuaries$EST_ID %in% missing,]

pp.2023 <- crop(pp.crop, missing.estuaries[1,])  
pp.2023.df <- as.data.frame(pp.2023, xy = TRUE)

ggplot() +
  geom_tile(data = pp.2023.df, aes(x = x, y = y, fill = sum)) + 
  geom_sf(data = missing.estuaries[1,], color = "red", fill = NA)

#some rasters had NAs within some of the estuaries, and the previous code did not address this,
#resulting in some entire estuaries with missing data. this code identified this issue - however,
#4 estuaries still had no data, and the code below identifies what the problem is with these estuaries

missing <- c(3019, 69, 133, 151)
missing.estuaries <- estuaries[estuaries$EST_ID %in% missing,]

pp.3019 <- crop(pp.crop, missing.estuaries[1,]) #all NaN values
pp.3019.df <- as.data.frame(pp.3019, xy = TRUE)

ggplot() +
  geom_tile(data = pp.df, aes(x = x, y = y, fill = sum)) + 
  geom_sf(data = missing.estuaries[1,], color = "red", fill = NA) +
  xlim(c(-123.9, -123.6)) +
  ylim(c(39.2, 39.3))

#issue is the estuary only partially overlaps with a single raster cell - need to find that raster cell value 
#to use for the entire estuary
bbox <- c(
ext(missing.estuaries[1,])[1] - 0.01,
ext(missing.estuaries[1,])[2] + 0.01,
ext(missing.estuaries[1,])[3] - 0.01,
ext(missing.estuaries[1,])[4] + 0.01)

bbox.crop <- crop(pp.crop, bbox)
bbox.df <- as.data.frame(bbox.crop, xy = TRUE)

ggplot() +
  geom_tile(data = bbox.df, aes(x = x, y = y, fill = sum)) + 
  geom_sf(data = missing.estuaries[1,], color = "red", fill = NA)

#this works for data for the missing estuary - check to see if it works for the rest

bbox <- c(
  ext(missing.estuaries[2,])[1] - 0.01,
  ext(missing.estuaries[2,])[2] + 0.01,
  ext(missing.estuaries[2,])[3] - 0.01,
  ext(missing.estuaries[2,])[4] + 0.01)

bbox.crop <- crop(pp.crop, bbox)
bbox.df <- as.data.frame(bbox.crop, xy = TRUE)

ggplot() +
  geom_tile(data = bbox.df, aes(x = x, y = y, fill = sum)) + 
  geom_sf(data = missing.estuaries[2,], color = "red", fill = NA)

#works

bbox <- c(
  ext(missing.estuaries[3,])[1] - 0.01,
  ext(missing.estuaries[3,])[2] + 0.01,
  ext(missing.estuaries[3,])[3] - 0.01,
  ext(missing.estuaries[3,])[4] + 0.01)

bbox.crop <- crop(pp.crop, bbox)
bbox.df <- as.data.frame(bbox.crop, xy = TRUE)

ggplot() +
  geom_tile(data = bbox.df, aes(x = x, y = y, fill = sum)) + 
  geom_sf(data = missing.estuaries[3,], color = "red", fill = NA)

#works

bbox <- c(
  ext(missing.estuaries[4,])[1] - 0.01,
  ext(missing.estuaries[4,])[2] + 0.01,
  ext(missing.estuaries[4,])[3] - 0.01,
  ext(missing.estuaries[4,])[4] + 0.01)

bbox.crop <- crop(pp.crop, bbox)
bbox.df <- as.data.frame(bbox.crop, xy = TRUE)

ggplot() +
  #annotation_map_tile(type = "hotstyle", zoomin = 1) +
  geom_tile(data = pp.df, aes(x = x, y = y, fill = sum)) + 
  geom_sf(data = missing.estuaries[4,], color = "red", fill = NA) +
  xlim(c(-127.83, -127.78)) +
  ylim(c(50.065, 50.10)) + 
  theme_bw() +
  theme(plot.background = element_rect(fill = NULL, color = NULL))

ggsave('amos.creek.missing.png',
       width = 8.7,
       height = 7,
       dpi = 600,
       bg = "transparent")

#doesn't work - no cell overlaps

bbox <- st_bbox(estuaries[1,])
r <- rast(xmin = bbox[1], xmax = bbox[3], ymin = bbox[2], ymax = bbox[4], crs = "EPSG:4326", res = 0.01, vals = 1)
crop(r, estuaries[1,])

amos <- missing.estuaries[4,]
saveRDS(amos, "Amos.Creek.Shapefile.rds")
st_write(amos, "Amos.Creek.Shapefile.shp")
