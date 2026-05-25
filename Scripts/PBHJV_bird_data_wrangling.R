#bird data wrangling

library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(ggspatial)

setwd("C:/Users/rekha/OneDrive - University of Victoria/Wetlands")

#load in bird data

relativeabun <- rast('Data/Bird_data/PacificBirds_all-bird-groups_all-species-priority_annual_relative_abundance_annual.tif')
relativeabun <- terra::project(relativeabun, "epsg:4269")
richness <- rast('Data/Bird_data/PacificBirds_all-bird-groups_all-species-priority_annual_richness_annual.tif')
richness <- terra::project(richness, "epsg:4269")

shorebirds.relativeabun <- rast('Data/Bird_data/PacificBirds_shorebirds_all-species-priority_annual_relative_abundance_annual.tif')
shorebirds.relativeabun <- terra::project(shorebirds.relativeabun, "epsg:4269")
shorebirds.richness <- rast('Data/Bird_data/PacificBirds_shorebirds_all-species-priority_annual_richness_annual.tif')
shorebirds.richness <- terra::project(shorebirds.richness, "epsg:4269")

waterfowl.relativeabun <- rast('Data/Bird_data/PacificBirds_waterfowl_all-species-priority_annual_relative_abundance_annual.tif')
waterfowl.relativeabun <- terra::project(waterfowl.relativeabun, "epsg:4269")
waterfowl.richness <- rast('Data/Bird_data/PacificBirds_waterfowl_all-species-priority_annual_richness_annual.tif')
waterfowl.richness <- terra::project(waterfowl.richness, "epsg:4269")

#load estuary data

estuaries <- readRDS('Data/Shapefiles/estuaries.rds')
estuaries <- st_transform(estuaries, crs = st_crs("EPSG:4269"))

#extract bird data for each estuary ------------------------------------------

#set up dataframe

bird.df <- data.frame(EST_ID = NA,
                      relative.abundance = NA,
                      richness = NA,
                      shorebird.relative.abundance = NA,
                      shorebird.richness = NA,
                      waterfowl.relative.abundance = NA,
                      waterfowl.richness = NA)

for(i in 1:nrow(estuaries)){
  
  #crop bird data to estuary
  
  c1 <- crop(relativeabun, estuaries[i,])
  df1 <- as.data.frame(c1, xy = TRUE)
  c2 <- crop(richness, estuaries[i,])
  df2 <- as.data.frame(c2, xy = TRUE)
  c3 <- crop(shorebirds.relativeabun, estuaries[i,])
  df3 <- as.data.frame(c3, xy = TRUE)
  c4 <- crop(shorebirds.richness, estuaries[i,])
  df4 <- as.data.frame(c4, xy = TRUE)
  c5 <- crop(waterfowl.relativeabun, estuaries[i,])
  df5 <- as.data.frame(c5, xy = TRUE)
  c6 <- crop(waterfowl.richness, estuaries[i,])
  df6 <- as.data.frame(c6, xy = TRUE)
  
  #extract data for estuary
  bird.df[i,1] <- estuaries$EST_ID[i]
  bird.df[i,2] <- max(df1[,3])
  bird.df[i,3] <- max(df2[,3])
  bird.df[i,4] <- max(df3[,3])
  bird.df[i,5] <- max(df4[,3])
  bird.df[i,6] <- max(df5[,3])
  bird.df[i,7] <- max(df6[,3])
  
}

write.csv(bird.df, "Ranking/Data/PBHJV_bird_estuary_data.csv")

#find missing estuaries data ----------------------------------------------

missing <- c(3019, 69, 133, 151) #subset estuaries missing data
missing.estuaries <- estuaries[estuaries$EST_ID %in% missing,]

#issue is the estuary only partially overlaps with a single raster cell - 
#need to find that raster cell value to use for the entire estuary by expanding bbox of estuary

for(i in 1:3){
  
  bbox <- c(
    ext(missing.estuaries[i,])[1] - 0.01,
    ext(missing.estuaries[i,])[2] + 0.01,
    ext(missing.estuaries[i,])[3] - 0.01,
    ext(missing.estuaries[i,])[4] + 0.01)
  
  #crop bird data to estuary
  
  c1 <- crop(relativeabun, bbox)
  df1 <- as.data.frame(c1, xy = TRUE)
  c2 <- crop(richness, bbox)
  df2 <- as.data.frame(c2, xy = TRUE)
  c3 <- crop(shorebirds.relativeabun, bbox)
  df3 <- as.data.frame(c3, xy = TRUE)
  c4 <- crop(shorebirds.richness, bbox)
  df4 <- as.data.frame(c4, xy = TRUE)
  c5 <- crop(waterfowl.relativeabun, bbox)
  df5 <- as.data.frame(c5, xy = TRUE)
  c6 <- crop(waterfowl.richness, bbox)
  df6 <- as.data.frame(c6, xy = TRUE)
  
  #extract data for estuary
  bird.df[which(bird.df$EST_ID == missing.estuaries$EST_ID[i]),2] <- max(df1[,3])
  bird.df[which(bird.df$EST_ID == missing.estuaries$EST_ID[i]),3] <- max(df2[,3])
  bird.df[which(bird.df$EST_ID == missing.estuaries$EST_ID[i]),4] <- max(df3[,3])
  bird.df[which(bird.df$EST_ID == missing.estuaries$EST_ID[i]),5] <- max(df4[,3])
  bird.df[which(bird.df$EST_ID == missing.estuaries$EST_ID[i]),6] <- max(df5[,3])
  bird.df[which(bird.df$EST_ID == missing.estuaries$EST_ID[i]),7] <- max(df6[,3])
  
}

#save data, overwriting previous data with missing data
write.csv(bird.df, "Ranking/Data/PBHJV_bird_estuary_data.csv")

#amos creek estuary (151) has no data - reach out to jenny munoz for raw bird presence data
#from this data we can only gain richness, not relative abundance values
