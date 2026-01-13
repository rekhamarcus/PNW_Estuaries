#this script contains all the code used to generate the map of the study area, as
#used in the Masters project proposal

library(dplyr)
library(ggplot2)
library(tidyterra)
library(terra) #for working with spatial data
library(sf) #for working with spatial data
library(basemaps)#for basemaps 
library(ggspatial)
library(gdalUtilities)

setwd("C:/Users/rekha/OneDrive - University of Victoria/Wetlands")

#import study area shapefile
PBHJV <- read_sf("Data/Shapefiles/PBHJV_shapefiles/Continental_PBHJV.shp")
PBHJV <- st_transform(PBHJV, crs = st_crs("ESRI:102008"))

#import WA,OR,CA estuary data - obtained at https://www.pacificfishhabitat.org/data/
USA.estuaries <- read_sf("Data/Shapefiles/PMEP_West_Coast_Estuary_Extent/PMEP_West_Coast_USA_Estuary_Extent_V1.gdb")
USA.estuaries <- st_transform(USA.estuaries, crs = st_crs("ESRI:102008"))

BC.estuaries <- read_sf("Data/Shapefiles/BC_Estuary_shp/PECP_EstuaryPolygons_2018Update_16Jan2020.shp")
BC.estuaries <- st_transform(BC.estuaries, crs = st_crs("ESRI:102008"))

#join estuaries and label with names

names(USA.estuaries)[3] <- "EST_ID"

USA.names <- USA.estuaries[,c(3,5)] %>%
  st_drop_geometry()

estuaries <- readRDS("Data/Shapefiles/estuaries.rds")

estuaries.simple <- estuaries[,1] %>%
  st_drop_geometry()

estuary.names <- left_join(estuaries.simple, USA.names, by = "EST_ID")
write.csv(estuary.names, "Data/estuary.names.csv")

#fix multisurface geometries ---------------------------

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

#wrangle estuaries into single file----

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

#subset out estuaries for data testing---------------------------------

#BC: fraser river estuary - 391, bella coola estuary - 328")
#USA: columbia river estuary - 270, garcia river estuary - 131

fre <- BC.estuaries[391,]

sub1 <- BC.estuaries[c(328, 391),]
sub2 <- USA.estuaries[c(131, 270),]

estuaries.subset <- rbind(sub1, sub2)

#save product
saveRDS(estuaries.subset, "Data/Shapefiles/estuary_subset.rds")

#save individual estuary
saveRDS(fre, "Data/Shapefiles/fraser_river_estuary.rds")

#subset all columbia river estuary into one file
cre <- estuaries.pnw[which(estuaries.pnw$EST_ID == c(2025:2032)),]
saveRDS(cre, "Data/Shapefiles/columbia_river_estuary.rds")

#plot single estuaries from each region------

BC.est1 <- estuaries[estuaries$EST_ID == 1017,]
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

ggsave("Figures/samish_estuary.png", 
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
  geom_sf(data = pnw, fill = NA, color = 'grey') + 
  geom_sf(data = estuary.centers, color = "#c9184a", size = 0.5) + 
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

#plot fraser river data -----------------------------------------

fre <- readRDS("Data/Shapefiles/fraser_river_estuary.rds")
fre <- st_transform(fre, crs = st_crs("EPSG:4326")) #reproject to be able to crop data

rain <- c("#3fc1c0", "#20bac5", "#00b2ca", "#04a6c2", "#0899ba", "#0f80aa", "#16679a", "#1a5b92", "#1c558e", "#1d4e89")
heat <- rev(c("#880d1e", "#dd2d4a", "#f26a8d","#f49cbb", "#cbeef3"))

fre.temp.hist <- rast("Data/Temperature/CHELSA_monthly_timeseries_historical/CHELSA_tas_02_2002_V.2.1.tif") %>%
  crop(fre) %>%
  mask(fre)
fre.temp.proj <- rast("Data/Temperature/CHELSA_monthly_projections/CHELSA_gfdl-esm4_r1i1p1f1_w5e5_ssp370_tas_02_2041_2070_norm.tif")%>%
  crop(fre) %>%
  mask(fre)
fre.temp.sea.proj <- rast("Data/Temperature/CHELSA_seasonality_projections/CHELSA_bio4_2041-2070_gfdl-esm4_ssp370_V.2.1.tif")%>%
  crop(fre) %>%
  mask(fre)

ssp370.2041.2070 <- readRDS("Data/Results/ssp370.2041.2070.rds")
  ssp370.2041.2070 <- ssp370.2041.2070[which(ssp370.2041.2070$EST_ID == 391),]

#plot historical mean (a)
ggplot() +
  annotation_map_tile(type = "osm", zoomin = 0) +
  labs(title = "Average Temperature in the Fraser River Estuary", subtitle = "February 2002") +
  geom_spatraster(data = (fre.temp.hist/10) - 273.15, interpolate = TRUE) + 
  scale_fill_gradientn(colors = heat, na.value = NA, limits = c(-5,25), name = "Temperature (C)") +
  geom_sf(data = fre, fill = NA, color = "black") +
  theme_void() 

ggsave("fre.temp.feb2002.png",
       width = 5,
       height = 5, 
       dpi = 600)

#plot projected mean (b)
ggplot() +
  annotation_map_tile(type = "osm", zoomin = 0) +
  labs(title = "Average Temperature in the Fraser River Estuary", subtitle = "February, SSP3 7.0, 2041-2070") +
  geom_spatraster(data = fre.temp.proj, interpolate = TRUE) + 
  scale_fill_gradientn(colors = heat, na.value = NA, limits = c(-5,25), name = "Temperature (C)") +
  geom_sf(data = fre, fill = NA, color = "black") +
  theme_void() 

ggsave("fre.temp.proj.ssp370.2041.2070.png",
       width = 5,
       height = 5, 
       dpi = 600)

#plot percent change in mean (c)
ggplot() +
  annotation_map_tile(type = "osm", zoomin = 0) +
  labs(title = "Percent Change in Average Temperature, FRE", subtitle = "February, SSP3 7.0, 2041-2070") +
  geom_sf(data = fre, fill = "#f26a8d", color = "black") +
  #scale_fill_manual(colors = heat, na.value = NA, name = "Temperature (C)") +
  theme_void() 

ggsave("fre.tempchange.ssp370.2041.2070.png",
       width = 5,
       height = 5, 
       dpi = 600)

#plot historical seasonality (d)

tas.files <- list.files(path = 'Data/Temperature/CHELSA_monthly_timeseries_historical/',
                        pattern = '.tif', full.names = TRUE)
tas <- sds(tas.files)

tas.sd <- app(tas, fun = "sd")
tas.sd <- tas.sd %>%
  crop(fre) %>%
  mask(fre)

ggplot() +
  annotation_map_tile(type = "osm", zoomin = 0) +
  labs(title = "Historical Temperature Seasonality in the Fraser River Estuary", subtitle = "February, SSP3 7.0, 2041-2070") +
  geom_spatraster(data = tas.sd/10, interpolate = TRUE) + 
  scale_fill_gradientn(colors = heat, na.value = NA, name = "Standard deviation") +
  geom_sf(data = fre, fill = NA, color = "black") +
  theme_void() 

ggsave("fre.hist.seas.png",
       width = 5,
       height = 5, 
       dpi = 600)

#plot projected seasonality (e)
ggplot() +
  annotation_map_tile(type = "osm", zoomin = 0) +
  labs(title = "Projected Temperature Seasonality in the Fraser River Estuary", subtitle = "SSP3 7.0, 2041-2070") +
  geom_spatraster(data = fre.temp.sea.proj/100, interpolate = TRUE) + 
  scale_fill_gradientn(colors = heat, na.value = NA, name = "Standard Deviation") +
  geom_sf(data = fre, fill = NA, color = "black") +
  theme_void() 

ggsave("fre.temp.sea.proj.ssp370.2041.2070.png",
       width = 5,
       height = 5, 
       dpi = 600)

#plot percent change in seasonality (f)

ggplot() +
  annotation_map_tile(type = "osm", zoomin = 0) +
  labs(title = "Percent Change in Seasonality, FRE", subtitle = "February, SSP3 7.0, 2041-2070") +
  geom_sf(data = fre, fill = "#f49cbb", color = "black") +
  theme_void() 

ggsave("fre.seaschange.ssp370.2041.2070.png",
       width = 5,
       height = 5, 
       dpi = 600)
 
#plot histograms to visualize data distribution
df.tas <- as.data.frame(tas.estuary, xy = TRUE)
df.tas <- pivot_longer(df.tas, cols = c(3:493))

#plot distribution of historical data (g)
ggplot(df.tas, aes(x = (value/10) - 273.15, fill = ..x..)) +
  geom_bar(width = 0.1) +
  scale_fill_gradientn(colours = heat) +
  ggtitle(label = "Recorded Temperatures in the Fraser River Estuary", subtitle = "1979 - 2019") +
  xlab("Temperature (C)") +
  ylab("Frequency") +
  theme_classic() + 
  theme(legend.position = "none")

ggsave("fre.temp.hist.png",
       width = 5,
       height = 5, 
       dpi = 600)

#not needed? 
ggplot(extremes, aes(x = tasmax, fill = ..x..)) +
  geom_bar(width = 0.2) +
  scale_fill_gradientn(colours = heat) +
  ggtitle(label = "Maximum Yearly Recorded Temperatures in the Fraser River Estuary", subtitle = "1979 - 2019") +
  xlab("Temperature (C)") +
  ylab("Frequency") +
  theme_classic() + 
  theme(legend.position = "none")

ggsave("fre.tasmax.hist.png",
       width = 5,
       height = 5, 
       dpi = 600)

#plot distribution of future data (h)

tas.proj.files <- list.files(path = 'Data/Temperature/CHELSA_monthly_projections/',
                        pattern = '2041_2070_norm.tif', full.names = TRUE)

tas.proj <- list()
for(i in 1:length(tas.proj.files)){
  r <- rast(tas.proj.files[[i]])
  c <- crop(r, fre)
  m <- mask(c, fre)
  
  df <- as.data.frame(m, xy = TRUE)
  names(df)[3] <- "value"
  
  tas.proj[[i]] <- df
}

tas.proj <- do.call(rbind, tas.proj)

ggplot(tas.proj, aes(x = value, fill = ..x..)) +
  geom_bar(width = 0.1) +
  scale_fill_gradientn(colours = heat) +
  ggtitle(label = "Projected Temperatures in the Fraser River Estuary", subtitle = "2041 - 2070") +
  xlab("Temperature (C)") +
  ylab("Frequency") +
  theme_classic() + 
  theme(legend.position = "none")

ggsave("fre.temp.proj.png",
       width = 5,
       height = 5, 
       dpi = 600)

#plot percent change in extremes (g)

ggplot() +
  annotation_map_tile(type = "osm", zoomin = 0) +
  labs(title = "Percent Change in 1-in-100 Year Temperature, FRE", subtitle = "February, SSP3 7.0, 2041-2070") +
  geom_sf(data = fre, fill = "#880d1e", color = "black") +
  theme_void() 

ggsave("fre.rp100.change.ssp370.2041.2070.png",
       width = 5,
       height = 5, 
       dpi = 600)


#plot results from columbia river estuary------------------------------------

temp.hist <- rast("Data/Temperature/CHELSA_monthly_timeseries_historical/CHELSA_tas_02_2002_V.2.1.tif")
temp.proj <- rast("Data/Temperature/CHELSA_monthly_projections/CHELSA_gfdl-esm4_r1i1p1f1_w5e5_ssp370_tas_02_2041_2070_norm.tif")
temp.sea.proj <- rast("Data/Temperature/CHELSA_seasonality_projections/CHELSA_bio4_2041-2070_gfdl-esm4_ssp370_V.2.1.tif")

ssp370.2041.2070 <- readRDS("Data/Results/ssp370.2041.2070.rds")

h <- crop(temp.hist, cre) %>%
  mask(cre)

p <- crop(temp.proj, cre) %>%
    mask(cre)
  
s <- crop(temp.sea.proj, cre) %>%
    mask(cre)



  

  

