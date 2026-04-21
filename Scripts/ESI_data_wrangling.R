#Script to attach estuaries to their bottom types to get risk of sea level rise 
#code written by chloe, compiled by rekha

library(dplyr)
library(sf)
library(terra)
library(tidyr)
library(stringr)
library(ggplot2)

setwd("C:/Users/rekha/OneDrive - University of Victoria/Wetlands")

#join shorezone data from all sub-regions --------------------------------------

#Bring in Northern California gdb file 
#Bring up shore types in ESI layer
Cali_ESI <- read_sf("SLR/Shapefiles/NorthernCal_2008_GDB/NorthernCaliforniaESI.gdb", layer = "esi_polygon")

#Rename some column names in Cali_ESI set, so variable names match with WA_OR_ESI set
Cali_ESI_Adjusted <- Cali_ESI %>% 
  rename(
    ESI_DESCRIPTION = ESI_Descriptor,
    SHAPE_Length = Shape_Length,
    SHAPE_Area = Shape_Area,
    SHAPE = Shape
  )
#From here forward use "Cali_ESI_adjusted", name columns match up with "WA_OR_ESI" set

WA_OR_ESI <- read_sf("SLR/Shapefiles/WA_OR_2014_GDB/WA_OR_ESI_2014.gdb", layer = "ESIP")

#Remove SOURCE_ID and ESI_SOURCE columns from WA_OR set so data frame perfectly lines up with Cali set
WA_OR_Adjusted <- WA_OR_ESI[,c(1:4, 7:9)]

#Use Rbind() to combine WA_OR and Cali ESI sets into one. You now have Shoreline type data for entire US study area except Puget Sound
US_NoPuget_ESI <- rbind(WA_OR_Adjusted, Cali_ESI_Adjusted)

#Time for Puget Sound Wrangling
PugetSnd_ESI <- read_sf("SLR/Shapefiles/PugetSnd_2006_GDB/PugetSoundESI.gdb", layer = "esip_polygon")

#Change up variable names in Puget Sound data so it matches with rest of US data
Puget_ESI_Adjusted <- PugetSnd_ESI %>% 
  rename(
    ESI_DESCRIPTION = ESI_Descriptor,
    SHAPE_Length = Shape_Length,
    SHAPE_Area = Shape_Area,
    SHAPE = Shape
  )

#Rbind adjusted Puget Sound data to the rest of the US data
US_ESI_Data <- rbind(Puget_ESI_Adjusted, US_NoPuget_ESI)

#Reproject US data from NAD83 to WGS84
US_ESI_Data_reproj <- st_transform(US_ESI_Data, "epsg:4326")

#BC unitlines layer
BC_13oct <- read_sf("SLR/Shapefiles/BC_ShoreZone_Mapping_13oct23.gdb", layer = "Unit_lines")
esi <- read_sf("SLR/Shapefiles/BC_ShoreZone_Mapping_13oct23.gdb", layer = "ESI")
BC_ESI <- full_join(BC_13oct, esi)

#subset only data layers needed
BC_ESI <- BC_ESI[,c(10, 8, 9)]

BC_ESI_UnitLines_merged <- st_transform(BC_ESI, "EPSG:4326")

#add in BC historical data
st_layers("SLR/Shapefiles/BC_Historical_ShoreZone_Mapping.gdb")
BC_Historical <- read_sf("SLR/Shapefiles/BC_Historical_ShoreZone_Mapping.gdb", layer = "Unit")
a <- f[,2] #get rid of all variables except SHAPE_Length 
a <- st_transform(a, "EPSG:4326")
names(a) <- c("Shape_Length", "Shape")

#Subtract variables from US dataset that are not common with BC data
US_ESI_data_adjusted <- US_ESI_Data_reproj[,c(1, 3, 5, 7)]

#Rename some variables so US and BC datasets match perfectly
US_ESI_data_adjusted2 <- US_ESI_data_adjusted %>% 
  rename(
    Shape_Length = SHAPE_Length,
    Shape = SHAPE
  )

#Use rbind() function to bring together BC and US data
PNW_ESI <- rbind(BC_ESI_UnitLines_merged, US_ESI_data_adjusted2)
#You now have a dataset that spans from North coast of BC to Northern California, that contains shore type codes and geometry

#Right now this is a dataframe object, which is not supported by st_rasterize() function. Convert to simple feature object with st_as_sf() function
PNW_ESI_st <- st_as_sf(PNW_ESI, sf_column_name = "Shape")
PNW_ESI_rast <- rast(PNW_ESI_st)

#Save dataset to an rds file (both df and raster versions)
saveRDS(PNW_ESI_st, "PNW_ESI.rds")
PNW_ESI_raster.rds <- saveRDS(PNW_ESI_rast, file = "PNW_ESI_rast.rds")



#wrangle ESI types into individual shapefiles -----------------------------------

#load in PNW_ESI file - compiled ESI types for each region of the study area
PNW_ESI <- readRDS("SLR/PNW_ESI.rds")
#PNW_ESI_cast <- st_cast(PNW_ESI, "POLYGON")

#Separate duplicate categories into separate categories and separate lines

PNW_separated <- list()

for(i in 1:nrow(PNW_ESI)){

  row <- PNW_ESI[i,]
  cell <- as.data.frame(PNW_ESI[i, 1])
  
  cells <- separate_longer_delim(cell, cols = 1, delim = "/")
  
  pnw_sep <- cbind(cells, row)
  pnw_sep <- pnw_sep[,c(1, 3:5)]
  
  PNW_separated[[i]] <- pnw_sep
  
}

PNW_SEP <- do.call(rbind, PNW_separated)
names(PNW_SEP)[1] <- "ESI"

PNW_SEP <- st_as_sf(PNW_SEP, sf_column_name = "Shape")

saveRDS(PNW_SEP, file = "SLR/PNW__ESItypes_separated.rds")

#PNW_SEP <- readRDS("SLR/PNW__ESItypes_separated.rds")

#separate ESI values

esi_types <- unique(PNW_SEP$ESI)

list_ESI <- list()
for(i in 1:length(esi_types)) {
  
  esi <- PNW_SEP[PNW_SEP$ESI == esi_types[i],]
  
  list_ESI[[i]] <-  esi
}

#remove empty list item

list_ESI <- list_ESI[-19]

saveRDS(list_ESI, file = "SLR/list_ESI.rds")

#calculate the area of each bottom type in each estuary ------------------------

#Bring in estuaries file
estuaries <- readRDS("Data/Shapefiles/estuaries.rds")

PNW_SEP <- readRDS("SLR/PNW__ESItypes_separated.rds")
list_ESI <- readRDS("SLR/list_ESI.rds")

#For loop
ESI_estuaries <- list()

for (i in 1:nrow(estuaries)) {
  
  ESI.df <- data.frame(EST_ID = estuaries$EST_ID[i],
                                      Area_HA = estuaries$Area_Ha[i],
                                      "8A" = NA,
                                      "8D" = NA,
                                      "6B" = NA, 
                                      "5" = NA,
                                      "9A" = NA,
                                      "3A" = NA,
                                      "10A" = NA,
                                      "2A" = NA,
                                      "8B" = NA,
                                      "8C" = NA,
                                      "1A" = NA,
                                      "1C" = NA,
                                      "4" = NA,
                                      "7" = NA,
                                      "6A" = NA,
                                      "10B" = NA,
                                      "6C" = NA,
                                      "9B" = NA, 
                                      "1B" = NA,
                                      "U" = NA,
                                      "10C" = NA,
                                      "10D" = NA)
  
  bbox <- st_bbox(estuaries[i,])
  r <- rast(xmin = bbox[1], xmax = bbox[3], ymin = bbox[2], ymax = bbox[4], crs = "EPSG:4326", res = 0.01, vals = 1)
  r <- crop(r, estuaries[i,])
  r <- mask(r, estuaries[i,])
  
  #ESI.df$total.cells[i] <- 1 #ncell(r)
  
  #extract number of raster cells that intersect with the estuary shapefile 
  for(j in 1:19){ #first 19 ESI geometries are linestrings which use a different process than polygons (see below for polygons)
    
    linestring <- st_cast(list_ESI[[j]], "LINESTRING") #fix estuary geometry
    st_crs(linestring) <- "EPSG:4326"
    
    #if there is no data, skip to next esi type
    possibleError <- tryCatch(
      c <- crop(r, linestring),
      error = function(e) e)
    
    if(inherits(possibleError, "error")) next
    
    #mask to only include cells touched by shapefile 
    m <- terra::mask(c, linestring)
    
    ESI.df[1, j+2] <- ncell(m)
    
  }
  
  for(j in 20:length(list_ESI)){
    
    polygon <- st_cast(list_ESI[[j]], "POLYGON")
    st_crs(polygon) <- "EPSG:4326"
    
    #if there is no data, skip to next esi type
    possibleError <- tryCatch(
      c <- crop(r, polygon),
      error = function(e) e)
    
    if(inherits(possibleError, "error")) next
    
    #mask to only include cells touched by shapefile 
    m <- terra::mask(c, polygon)
    
    ESI.df[1, j+2] <- ncell(m)
    
  }
  
  ESI_estuaries[[i]] <- ESI.df
  
  print(i)
  
}

estuaries.esi <- do.call(rbind, ESI_estuaries)

write.csv(estuaries.esi, "Data/Results/estuaries_esi_results.csv")

#plots ---------------------------------------------------------------------------

ggplot() +
  geom_sf(data = PNW_SEP)

#OUTDATED----------------------------------------------------------------------

#import all BC data which for some reason is in a bajillion different individual files
#this is not needed because it contains all the same data as the october 13 bc data, plus some
#data in the maritime provinces and alaska. whoops lol

BC.shz <- list.files(path = "SLR/Shapefiles/BC/", pattern = '.gdb')

#st_layers(paste("SLR/Shapefiles/BC/", BC.shz[2], sep = ""))

BC.shorezone <- list()

for(i in 1:length(BC.shz)){
  
  u <- read_sf(paste("SLR/Shapefiles/BC/", BC.shz[i], sep = ""), layer = "Unit_lines")
  esi <- read_sf(paste("SLR/Shapefiles/BC/", BC.shz[i], sep = ""), layer = "ESI")
  names(esi)[1] <- "PHY_IDENT"
  
  #add esi data to shapefile
  d <- full_join(u, esi) %>% st_as_sf
  
  #subset to only the columns needed
  d <- d[,c(10, 8, 9)]
  
  #reproject to WGS84 so they all work together nicely
  d <- st_transform(d, "EPSG:4326")
  
  #rename column so they for real all work together nicely
  names(d)[2] <- "Shape_Length"
  st_geometry(d) <- "Shape"
  
  BC.shorezone[[i]] <- d
  
}

#bc historical data is different than the rest of the files - add in separately
f <- read_sf(paste("SLR/Shapefiles/BC/", BC.shz[1], sep = ""), layer = "Unit")
a <- f[,2] #get rid of all variables except SHAPE_Length 
a <- st_transform(a, "EPSG:4326")
names(a) <- c("Shape_Length", "Shape")
BC.shorezone[[1]] <- a

BC.SHOREZONE <- do.call(rbind, BC.shorezone)