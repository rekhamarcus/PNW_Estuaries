#this script contains all the code used to generate the map of the study area, as
#used in the Masters project proposal

library(dplyr)
library(ggplot2)
library(tidyterra)
library(terra) #for working with spatial data
library(sf) #for working with spatial data
library(basemaps)#for basemaps 
library(ggspatial)
library(basemapR) #for basemaps ???

setwd("C:/Users/rekha/OneDrive - University of Victoria/Wetlands")

#import study area shapefile
PBHJV <- read_sf("Data/Shapefiles/PBHJV_shapefiles/Continental_PBHJV.shp")

#reproject to North America Albers
PBHJV <- st_transform(PBHJV, crs = st_crs("ESRI:102008"))

#wrangle estuaries into single shapefile----
#import BC estuary data
BC.estuaries <- read_sf("Data/Shapefiles/BC_Estuary_shp/PECP_EstuaryPolygons_2018Update_16Jan2020.shp")

#plot(BC.estuaries$geometry)

#import WA,OR,CA estuary data - obtained at https://www.pacificfishhabitat.org/data/
USA.estuaries <- read_sf("Data/Shapefiles/PMEP_West_Coast_Estuary_Extent/PMEP_West_Coast_USA_Estuary_Extent_V1.gdb")
  
#merge all into single dataset

#limit datasets to only ID, size, and shape
BC.estuaries <- BC.estuaries[,c(1,2,4)]
USA.estuaries <- USA.estuaries[,c(3,9,12)]

#change projections to NAD 1983 Albers North America

USA.estuaries <- st_transform(USA.estuaries, crs = st_crs("ESRI:102008"))
BC.estuaries <- st_transform(BC.estuaries, crs = st_crs("ESRI:102008"))


#change names to match
names(USA.estuaries)[c(1,2)] <- c("EST_ID", "Area_Ha")
names(BC.estuaries)[c(1,2)] <- c("EST_ID", "Area_Ha")
st_geometry(USA.estuaries) <- "geometry"

#merge
estuaries <- rbind(USA.estuaries, BC.estuaries)

#crop estuaries to study area
estuaries.pnw <- st_crop(estuaries, st_bbox(PBHJV))
estuaries.pnw <- st_shift_longitude(estuaries)

#crop study area to estuary area
pnw <- st_crop(PBHJV, st_bbox(estuaries.pnw))

#save product
#saveRDS(estuaries.pnw, "Data/estuaries.rds")
#saveRDS(pnw, "Data/PNW_shapefile.rds")

#plot single estuaries from each region------

BC.est1 <- BC.estuaries[3,]
US.est1 <- USA.estuaries.PNW[3,]

ggplot() +
  coord_sf(datum = "ESRI:102008") +
  annotation_map_tile(type = "osm", zoomin = 0) +
  geom_sf(data = BC.est1, fill = NA, color = "black") + 
  #basemap_gglayer(st_bbox(estuaries), map_service = "esri", map_type = "world_shaded_relief") +
  theme_void() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

ggsave("Figures/BC_estuary3.png", 
       width = 6,
       height = 6,
       dpi = 600)

ggplot() +
  coord_sf(datum = "ESRI:102008") +
  annotation_map_tile(type = "osm", zoomin = 0) +
  geom_sf(data = US.est1, fill = NA, color = "black") + 
  #basemap_gglayer(st_bbox(estuaries), map_service = "esri", map_type = "world_shaded_relief") +
  theme_void() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

ggsave("Figures/USA_estuary3.png", 
       width = 6,
       height = 6,
       dpi = 600)

#plot estuaries shapefile-----

estuaries <- readRDS("Data/estuaries.rds")

ggplot() +
  coord_sf(datum = "ESRI:102008") +
  annotation_map_tile(type = "osm", zoomin = 0) +
  geom_sf(data = estuaries, fill = NA, color = "black") + 
  theme_void() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

ggsave("estuaries.png", 
       width = 6,
       height = 12,
       dpi = 600)

#plot estuaries as points

estuary.centers <- st_centroid(estuaries)

ggplot() +
  coord_sf(datum = "ESRI:102008") +
  annotation_map_tile(type = "osm", zoomin = 0) +
  geom_sf(data = estuary.centers, color = "black", size = 0.1) + 
  #basemap_gglayer(st_bbox(estuaries), map_service = "esri", map_type = "world_shaded_relief") +
  theme_void() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

ggsave("estuaries_points.png", 
       width = 6,
       height = 12,
       dpi = 600)


#plot study area------

#crop PBHJV area to study area 
study.area <- st_crop(PBHJV, st_bbox(estuaries))

ggplot() +
  coord_sf(datum = "ESRI:102008") +
  annotation_map_tile(type = "osm", zoomin = 3) +
  geom_sf(data = study.area, fill = NA, color = "black") + 
  #basemap_gglayer(st_bbox(estuaries), map_service = "esri", map_type = "world_shaded_relief") +
  theme_void() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

ggsave("study_area.png", 
       width = 6,
       height = 12,
       dpi = 600)

#plot study area with estuaries as points 

ggplot() +
  coord_sf(datum = "ESRI:102008") +
  annotation_map_tile(type = "osm", zoomin = 3) +
  geom_sf(data = study.area, fill = NA, color = "black") + 
  geom_sf(data = estuary.centers, color = "black", size = 0.1) + 
  theme_void() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

ggsave("study_area_estuary_points.png", 
       width = 6,
       height = 12,
       dpi = 600)

#plot study area with estuaries as polygons

ggplot() +
  coord_sf(datum = "ESRI:102008") +
  annotation_map_tile(type = "osm", zoomin = 3) +
  geom_sf(data = study.area, fill = NA, color = "black") + 
  geom_sf(data = estuaries, fill = NA, color = "black") +
  theme_void() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

ggsave("study_area_estuary_polygons.png", 
       width = 6,
       height = 12,
       dpi = 600)
