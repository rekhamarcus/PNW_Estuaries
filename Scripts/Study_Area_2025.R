##'File name: Study_Are_2025
##'Author: Rekha Marcus
##'Thesis: Chapter 1 & Chapter 2
##'
##'This script contains the code used to create a single shapefile of all the 
##'estuaries in the study area (BC, WA, OR, and northern CA) that are within the
##'Pacific Birds Habitat Joint Venture's administrative area. This contains the
##'code used to generate Figure 1 for both Chapter 1 and Chapter 2

#load packages ----------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(tidyterra)
library(terra) #for working with spatial data
library(sf) #for working with spatial data
library(basemaps)#for basemaps 
library(ggspatial)
library(gdalUtilities)

source("PNW_Estuaries/Functions/colors.R") #color palettes

#load data --------------------------------------------------------------------
#import study area shapefile
PBHJV <- read_sf("Data/Shapefiles/PBHJV_shapefiles/Continental_PBHJV.shp")
PBHJV <- st_transform(PBHJV, crs = st_crs("ESRI:102008"))

#import WA,OR,CA estuary data - obtained at https://www.pacificfishhabitat.org/data/
USA.estuaries <- read_sf("Data/Shapefiles/PMEP_West_Coast_Estuary_Extent/PMEP_West_Coast_USA_Estuary_Extent_V1.gdb")
USA.estuaries <- st_transform(USA.estuaries, crs = st_crs("ESRI:102008"))

BC.estuaries <- read_sf("Data/Shapefiles/BC_Estuary_shp/PECP_EstuaryPolygons_2018Update_16Jan2020.shp")
BC.estuaries <- st_transform(BC.estuaries, crs = st_crs("ESRI:102008"))

#join estuaries and label with names ------------------------------------------

#USA estuaries will have EST_ID codes over 1000, and BC estuaries will have EST_ID
#codes between 1 and 425.

names(USA.estuaries)[3] <- "EST_ID"

USA.names <- USA.estuaries[,c(3,5)] %>%
  st_drop_geometry()

estuaries <- readRDS("Data/Shapefiles/estuaries.rds")

estuaries.simple <- estuaries[,1] %>%
  st_drop_geometry()

estuary.names <- left_join(estuaries.simple, USA.names, by = "EST_ID")
write.csv(estuary.names, "Data/estuary.names.csv")

#fix multisurface geometries --------------------------------------------------

#extract the ones with weird geometry
multisurface <- USA.estuaries[which(st_geometry_type(USA.estuaries) == "MULTISURFACE"),]

#subset only the ones in the study area
multisurface.pnw <- multisurface[which(multisurface$PMEP_Region == "Washington, Oregon, Northern California Coast"),]

#function to convert each to a separate shapefile
fix.multisurface <- function(shape, name) {
  
  st_write(shape, paste("Data/Shapefiles/Bad_shapefiles/",name,".shp", sep = ""))
  
  #convert to linear geometry
  multisurface <- ogr2ogr(paste("Data/Shapefiles/Bad_shapefiles/",name,".shp", sep = ""),
                          paste("Data/Shapefiles/PMEP_West_Coast_Estuary_Extent/Multisurface_",name,"_fixed.gdb",sep = ""),
                          explodecollections = T, nlt = 'CONVERT_TO_LINEAR')
  
  multisurface.fixed <- read_sf(paste("Data/Shapefiles/PMEP_West_Coast_Estuary_Extent/Multisurface_",name,"_fixed.gdb", sep = ""))
  
  #collapse all polygons back into single multipolygon row
  multisurface.fixed <- multisurface.fixed %>% group_by(PMEP_EI, Estry_H) %>% summarise()
  
  multisurface.fixed
  
}

klamath <- fix.multisurface(multisurface.pnw[1,], name = "klamath")
ossagon <- fix.multisurface(multisurface.pnw[2,], name = "ossagon")
fern <- fix.multisurface(multisurface.pnw[3,], name = "fern")
squashan <- fix.multisurface(multisurface.pnw[4,], name = "squashan")
columbiaf <- fix.multisurface(multisurface.pnw[5,], name = "columbiaf")
columbiag <- fix.multisurface(multisurface.pnw[6,], name = "columbiag")
columbiah <- fix.multisurface(multisurface.pnw[7,], name = "columbiah")

#bind all problem estuaries into a single file

problem.estuaries <- rbind(klamath, ossagon, fern, squashan, columbiaf, columbiag, columbiah)
problem.estuaries <- st_as_sf(problem.estuaries)

#wrangle estuaries into single file -------------------------------------------

#limit datasets to only ID, size, and shape
BC.estuaries <- BC.estuaries[,c(1,2,4)]
USA.estuaries <- USA.estuaries[,c(3,9,12)]

#change names to match
names(USA.estuaries)[c(1,2)] <- c("EST_ID", "Area_Ha")
names(problem.estuaries)[c(1,2)] <- c("EST_ID", "Area_Ha")
names(BC.estuaries)[c(1,2)] <- c("EST_ID", "Area_Ha")
st_geometry(USA.estuaries) <- "geometry"
st_geometry(problem.estuaries) <- "geometry"

#merge
estuaries <- rbind(USA.estuaries, BC.estuaries, problem.estuaries)

#crop estuaries to study area
estuaries.pnw <- st_crop(estuaries, st_bbox(PBHJV))

#crop study area to estuary area
pnw <- st_crop(PBHJV, st_bbox(estuaries.pnw))

#reproject both files
estuaries.pnw <- st_transform(estuaries.pnw, crs = st_crs("EPSG:4326"))
pnw <- st_transform(pnw, crs = st_crs("EPSG:4326"))

#save product
saveRDS(estuaries.pnw, "Data/Shapefiles/estuaries.rds")
saveRDS(pnw, "Data/PNW_shapefile.rds")
st_write(estuaries, "Data/estuaries.shp")

#Chapter 2 Figure 1 - estuaries map -------------------------------------------

estuaries <- readRDS("Data/Shapefiles/estuaries.rds")
pnw <- readRDS("Data/Shapefiles/PNW_shapefile.rds")

#plot estuaries as points

estuary.centers <- st_centroid(estuaries)

ggplot() +
  coord_sf(datum = "ESRI:102008") +
  annotation_map_tile(type = "osm", zoomin = 0) +
  geom_sf(data = pnw, fill = "#ffb703", alpha = 0.3, color = "black", size = 0.1) + 
  geom_sf(data = estuary.centers, color = "#e63946", fill = "#e63946", shape = 23, size = 0.5) + 
  theme_void() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

ggsave("Figures/Study_area/estuaries_points.png", 
       width = 6,
       height = 12,
       dpi = 600)

#Chapter 1 Figure 1 - estuaries with sub-regions map --------------------------

#add in sub-regions

SWBC <- readRDS("Data/Shapefiles/Regions/southwestern_BC.rds")
NWVI <- readRDS("Data/Shapefiles/Regions/northwest_VI.rds")
NCM <- readRDS("Data/Shapefiles/Regions/northcentral_BC.rds")
HG <- readRDS("Data/Shapefiles/Regions/haida_gwaii.rds")
SS <- readRDS("Data/Shapefiles/Regions/salish_sea.rds")
WONC <- readRDS("Data/Shapefiles/Regions/WA_OR_northernCA.rds")
CC <- readRDS("Data/Shapefiles/Regions/central_CA.rds")

SWBC <- estuaries[estuaries$EST_ID %in% SWBC$EST_NO,]
NWVI <- estuaries[estuaries$EST_ID %in% NWVI$EST_NO,]
NCM <- estuaries[estuaries$EST_ID %in% NCM$EST_NO,]
HG <- estuaries[estuaries$EST_ID %in% HG$EST_NO,]
SS <- estuaries[estuaries$EST_ID %in% SS$PMEP_EstuaryID,]
WONC <- estuaries[estuaries$EST_ID %in% WONC$PMEP_EstuaryID,]
CC <- estuaries[estuaries$EST_ID %in% CC$PMEP_EstuaryID,]

#add in regions to results dataset
SWBC$region <- "SWBC"
NWVI$region <- "NWVI"
NCM$region <- "NCM"
HG$region <- "HG"
SS$region <- "SS"
WONC$region <- "WONC"
CC$region <- "CC"

#save all results in single file
estuaries.region.points <- rbind(NCM, HG, NWVI, SWBC, SS, WONC, CC) %>%
  st_centroid()

#plot estuaries with different colors for sub-regions

labels <- rev(c("North Central BC", "Haida Gwaii", "Northwest Vancouver Island", "Southwest BC", "Salish Sea", "WA, OR, Northern CA", "Central California"))

ggplot() +
  coord_sf(datum = "ESRI:102008") +
  annotation_map_tile(type = "hotstyle", zoomin = 1) +
  #geom_sf(data = pnw, fill = NA, color = 'black') + 
  geom_sf(data = estuaries.region.points, aes(colour = region, fill = region), size = 0.5, shape = 23) + 
  #scale_colour_discrete(name = " Region", limits = rev(c("NCM", "HG", "NWVI", "SWBC", "SS", "WONC", "CC")), labels = labels) +
  #scale_fill_discrete(name = " Region", limits = rev(c("NCM", "HG", "NWVI", "SWBC", "SS", "WONC", "CC")), labels = labels) +
  scale_colour_manual(name = " Region", limits = rev(c("NCM", "HG", "NWVI", "SWBC", "SS", "WONC", "CC")), labels = labels, values = colors7) +
  scale_fill_manual(name = " Region", limits = rev(c("NCM", "HG", "NWVI", "SWBC", "SS", "WONC", "CC")), labels = labels, values = colors7) + 
  #guides(colour = guide_legend(nrow = 4)) + 
  theme_void() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(), 
        legend.position = "inside",
        legend.position.inside = c(0.35, 0.15),
        legend.text = element_text(size = 7, family = "sans"),
        legend.title = element_text(size = 9, family = "sans"), 
        legend.key.size = unit(0.5, "cm"),
        legend.background = element_rect(fill = "white", colour = "white"))

ggsave("Figures/estuary.region.points.png",
       width = 3.05,
       height = 7.86,
       dpi = 600)
