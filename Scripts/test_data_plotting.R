#test data

library(dplyr)
library(terra)
library(sf)
library(ggplot2)
library(raster)
library(tidyterra)
library(ggspatial)
library(prettymapr)

setwd('C:/Users/rekha/OneDrive - University of Victoria/Wetlands/')

#sort estuary data by region --------------------------------------------------

#import estuary data

usa.estuaries <- read_sf('Data/Shapefiles/PMEP_West_Coast_Estuary_Extent/PMEP_West_Coast_USA_Estuary_Extent_V1.gdb')
BC.estuaries <- read_sf("Data/Shapefiles/BC_Estuary_shp/PECP_EstuaryPolygons_2018Update_16Jan2020.shp")
salishsea <- readRDS("Data/Shapefiles/Regions/salish_sea.rds")

estuaries <- readRDS("Data/Shapefiles/estuaries.rds")

#sort estuary data by region

BC.estuary.regions <- read.csv('Ranking/PECPestuary_regions.csv')
names(BC.estuary.regions)[1] <- "EST_NO"
BC.estuaries <- left_join(BC.estuaries, BC.estuary.regions, by = 'EST_NO')

SWBC <- filter(BC.estuaries, Planning.Area == "Southwestern BC")
saveRDS(SWBC, "Data/Shapefiles/Regions/southwestern_BC.rds")
NWVI <- filter(BC.estuaries, Planning.Area == "Northern & Western Vancouver Island")
saveRDS(NWVI, "Data/Shapefiles/Regions/northwest_VI.rds")
NCM <- filter(BC.estuaries, Planning.Area == "Northern & Central Mainland")
saveRDS(NCM, "Data/Shapefiles/Regions/northcentral_BC.rds")
HG <- filter(BC.estuaries, Planning.Area == "Haida Gwaii")
saveRDS(HG, "Data/Shapefiles/Regions/haida_gwaii.rds")

SS <- filter(usa.estuaries, PMEP_Region == "Salish Sea")
saveRDS(SS, "Data/Shapefiles/Regions/salish_sea.rds")
WONC <- filter(usa.estuaries, PMEP_Region == "Washington, Oregon, Northern California Coast")
saveRDS(WONC, "Data/Shapefiles/Regions/WA_OR_northernCA.rds")
CC <- filter(usa.estuaries, PMEP_Region == "Central California")
saveRDS(CC, "Data/Shapefiles/Regions/central_CA.rds")

#import data 

CC <- readRDS("Data/Shapefiles/Regions/central_CA.rds")
HG <- readRDS("Data/Shapefiles/Regions/haida_gwaii.rds")
NCBC <- readRDS("Data/Shapefiles/Regions/northcentral_BC.rds")
NWVI <- readRDS("Data/Shapefiles/Regions/northwest_VI.rds")
SS <- readRDS("Data/Shapefiles/Regions/salish_sea.rds")
SWBC <- readRDS("Data/Shapefiles/Regions/southwestern_BC.rds")
WONC <- readRDS("Data/Shapefiles/Regions/WA_OR_northernCA.rds")

#plot results by region -------------------------------------------------------

#import data

ssp370.2041.2070 <- readRDS('Data/Results/ssp370.2041.2070.rds')

#crop to desired region
results.nwvi <- ssp370.2041.2070[ssp370.2041.2070$EST_ID %in% NWVI$EST_ID,]
results.nwvi <- left_join(NWVI, results.nwvi, by = "EST_ID")

ggplot(results, aes(fill = as.numeric(mean.tas))) +
  annotation_map_tile(type = "osm", zoomin = 3) +
  geom_sf(color = "black", linewidth = 0.05) +
  labs(title = "Percent Change in Mean Temperature, North + West Vancouver Island", subtitle = "SSP3 7.0, 2041 - 2070") + 
  scale_fill_gradient(low = '#f9dbbd', high = '#a53860', name = "Percent Change") +
  theme_void() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = c(0.1, 0.3),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA))


ggsave('nwvi.ssp370.2041.2070.png',
       width = 10,
       height = 10,
       dpi = 600)

results.swbc <- ssp370.2041.2070[ssp370.2041.2070$EST_ID %in% SWBC$EST_NO,]
names(SWBC)[1] <- "EST_ID"
results.swbc <- left_join(SWBC, results.swbc, by = "EST_ID")

ggplot(results.swbc, aes(fill = as.numeric(mean.tas))) +
  annotation_map_tile(type = "osm", zoomin = 0) +
  geom_sf(color = "black", linewidth = 0.05) +
  labs(title = "Percent Change in Mean Temperature, Southwestern BC", subtitle = "SSP3 7.0, 2041 - 2070") + 
  scale_fill_gradient(low = '#f9dbbd', high = '#a53860', name = "Percent Change") +
  theme_void() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = c(0.1, 0.3),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA))


ggsave('swbc.ssp370.2041.2070.png',
       width = 10,
       height = 10,
       dpi = 600)

results.ss <- ssp370.2041.2070[ssp370.2041.2070$EST_ID %in% SS$PMEP_EstuaryID,]
names(SS)[3] <- "EST_ID"
results.ss <- left_join(SS, results.ss, by = "EST_ID")

ggplot(results.ss, aes(fill = as.numeric(mean.tas))) +
  annotation_map_tile(type = "osm", zoomin = 0) +
  geom_sf(color = "black", linewidth = 0.05) +
  labs(title = "Percent Change in Mean Temperature, Salish Sea", subtitle = "SSP3 7.0, 2041 - 2070") + 
  scale_fill_gradient(low = '#f9dbbd', high = '#a53860', name = "Percent Change") +
  theme_void() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = c(0.1, 0.3),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA))


ggsave('ss.ssp370.2041.2070.png',
       width = 10,
       height = 10,
       dpi = 600)

results.cc <- ssp370.2041.2070[ssp370.2041.2070$EST_ID %in% CC$PMEP_EstuaryID,]
names(CC)[3] <- "EST_ID"
results.cc <- left_join(CC, results.cc, by = "EST_ID")

ggplot(results.cc, aes(fill = as.numeric(mean.tas))) +
  annotation_map_tile(type = "osm", zoomin = 0) +
  geom_sf(color = "black", linewidth = 0.05) +
  labs(title = "Percent Change in Mean Temperature, Central California", subtitle = "SSP3 7.0, 2041 - 2070") + 
  scale_fill_gradient(low = '#f9dbbd', high = '#a53860', name = "Percent Change") +
  theme_void() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = c(0.1, 0.3),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA))


ggsave('ss.ssp370.2041.2070.png',
       width = 10,
       height = 10,
       dpi = 600)

#function to plot maps -----------------------------------------------------------

plot.estuary.climate <- function(data, region, fillvar, title, sub, lowcol, highcol){
  
  #crop to desired region
  results <- data[data$EST_ID %in% region$EST_ID,]
  results <- left_join(region, results, by = "EST_ID")
  
  #plot
  ggplot(results, aes(fill = as.numeric(fillvar))) + #fill var doesnt work as it belongs to a different df than results
    annotation_map_tile(type = "osm", zoomin = 3) +
    geom_sf(color = "black", linewidth = 0.05) +
    labs(title = title, subtitle = sub) + 
    scale_fill_gradient(low = lowcol, high = highcol, name = "Percent Change") +
    theme_void() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position = c(0.1, 0.3),
          panel.background = element_rect(fill = "transparent", color = NA),
          plot.background = element_rect(fill = "transparent", color = NA))
  
  
}

plot.estuary.climate(data = ssp370.2041.2070, region = NWVI, fillvar = ssp370.2041.2070$mean.tas,
                     title = "Percent Change in Mean Temperature of Estuaries", sub = "North and West Vancouver Island, SSP3 7.0, 2041 - 2070",
                     lowcol = '#f9dbbd',highcol = '#d62828')

#plot each df/variable combo as an sf map with fill color corresponding to the variable

temp.colors <- c('#003049', '#d62828', '#f77f00', '#fcbf49', '#eae2b7', '#fdf0d5')
precip.colors <- c('#03045e', '#0077b6', '#00b4d8', '#90e0ef', '#caf0f8','#fdf0d5')

plot.estuary.climate <- function(data = ssp370.2041.2070, fillvar = ssp370.2041.2070$mean.tas){
  
  climate.estuary.sf <- left_join(data, NWVI, by = 'EST_ID')
  
  ggplot() +
    annotation_map_tile(type = "osm", zoomin = 0) +
    geom_sf(data = climate.estuary.sf$geometry, aes(fill = fillvar), color = "black", linewidth = 0.001) + 
    #coord_sf(datum = "ESRI:102001", lims_method = "geometry_bbox") +
    scale_fill_gradient(values = heat, name = "Percent Change") + 
    theme_void() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background = element_rect(fill = "transparent", color = NA),
          plot.background = element_rect(fill = "transparent", color = NA))
  
}

plot.estuary.climate(data = ssp126.2011.2040, fillvar = ssp126.2011.2040$mean.pr)


#data summarizing -------------------------------------------------------------

#PRECIPITATION data summarizing

pr.files <- list.files(path = 'Data/Precipitation/CHELSA/',
                       pattern = '.tif', full.names = TRUE)

pr <- sds(pr.files)

pr.mean <- app(pr, fun = "mean")
pr.min <- app(pr, fun = "min")
pr.max <- app(pr, fun = "max")
pr.var <- app(pr, fun = "var")
pr.range <- app(pr, fun = "range")


#TEMPERATURE data summarizing

tas.files <- list.files(path = 'Data/Temperature/CHELSA/',
                       pattern = '.tif', full.names = TRUE)

tas <- sds(tas.files)

tas.mean <- app(tas, fun = "mean") 
tas.min <- app(tas, fun = "min")
tas.max <- app(tas, fun = "max")
tas.var <- app(tas, fun = "var")
tas.range <- app(tas, fun = "range")

#plot summary stats for mean change and mean seasonality change-------------------------

pr.proj.feb.FRE <-rast('Data/FRE_subsets/precipitation_projfeb_FRE.tif')
pr.seas.FRE <- rast('Data/FRE_subsets/precipitation_seasonality_FRE.tif')
pr.norm.feb.FRE <- rast('Data/FRE_subsets/precipitation_normfeb_FRE.tif')

tas.proj.feb.FRE <- rast('Data/FRE_subsets/temperature_projfeb_FRE.tif')
tas.seas.FRE <- rast('Data/FRE_subsets/temperature_seasonality_FRE.tif')
tas.norm.feb.FRE <- rast('Data/FRE_subsets/temperature_normfeb_FRE.tif')

rain <- c("#9ad1d4","#007ea7","#003249")

heat <- c("#ff9b54", "#ff7f51","#ce4257", "#720026","#4f000b")

#percent change from normals to each year period for each ssp scenario

meanchange.plot <- function(data = subset(pr.proj.feb.FRE, c(1)), colors = rain, baseline = pr.norm.feb.FRE, title = "Percent Change 2011 - 2040, SSP1 2.6") {
  
  diff <- ((data - baseline)/baseline)*100
  
  ggplot() +
    ggtitle(title) +
    #annotation_map_tile(type = "osm", zoomin = 0) +
    geom_spatraster(data = diff) +
    #geom_sf(data = FRE, fill = NA, color = "black", linewidth = 0.2) +
    scale_fill_gradientn(colors = colors, na.value = NA) +
    coord_sf(datum = "ESRI:102001") +
    theme_void() +
    theme(plot.margin = unit(c(0.2, -0.5, -0.2, -0.5), "cm"), #top, right, bottom, left
          legend.position = c(0.9, 0.2),
          legend.key.size = unit(0.5, "cm"),
          legend.title = element_text(hjust = 0.5, face = "bold")) + 
    guides(fill = guide_colorbar(title.position = "top", ticks.colour = NA, barwidth = 10,
                                 barheight = 2, title="Percent Change",  direction = "horizontal"))
  
}

#plot mean change-------------------------------------------------------------

pr.base.plot <- ggplot() +
  ggtitle("Mean February Precipitation, 1981 - 2010") +
  #annotation_map_tile(type = "osm", zoomin = 0) +
  geom_spatraster(data = pr.norm.feb.FRE) +
  #geom_sf(data = FRE, fill = NA, color = "black", linewidth = 0.2) +
  scale_fill_gradientn(colors = rain, na.value = NA) +
  coord_sf(datum = "ESRI:102001") +
  theme_void() +
  theme(plot.margin = unit(c(0.2, -0.5, -0.2, -0.5), "cm"), #top, right, bottom, left
        legend.position = c(0.9, 0.3),
        legend.key.size = unit(0.5, "cm"),
        legend.title = element_text(hjust = 0.5, face = "bold")) + 
  guides(fill = guide_colorbar(title.position = "top", ticks.colour = NA, barwidth = 10,
                               barheight = 2, title="Mean Precipitation",  direction = "horizontal"))

tas.base.plot <- ggplot() +
  ggtitle("Mean February Temperature, 1981 - 2010") +
  #annotation_map_tile(type = "osm", zoomin = 0) +
  geom_spatraster(data = tas.norm.feb.FRE) +
  #geom_sf(data = FRE, fill = NA, color = "black", linewidth = 0.2) +
  scale_fill_gradientn(colors = heat, na.value = NA) +
  coord_sf(datum = "ESRI:102001") +
  theme_void() +
  theme(plot.margin = unit(c(0.2, -0.5, -0.2, -0.5), "cm"), #top, right, bottom, left
        legend.position = c(0.9, 0.3),
        legend.key.size = unit(0.5, "cm"),
        legend.title = element_text(hjust = 0.5, face = "bold")) + 
  guides(fill = guide_colorbar(title.position = "top", ticks.colour = NA, barwidth = 10,
                               barheight = 2, title="Mean Temperature",  direction = "horizontal"))

pr1 <- meanchange.plot(data = subset(pr.proj.feb.FRE, 1), colors = rain, baseline = pr.norm.feb.FRE, title = "Percent Change 2011 - 2040, SSP1 2.6")
pr2 <- meanchange.plot(data = subset(pr.proj.feb.FRE, 2), colors = rain, baseline = pr.norm.feb.FRE, title = "Percent Change 2041 - 2070, SSP1 2.6")
pr3 <- meanchange.plot(data = subset(pr.proj.feb.FRE, 3), colors = rain, baseline = pr.norm.feb.FRE, title = "Percent Change 2071 - 2100, SSP1 2.6")

ggarrange(pr.base.plot, pr1, pr2, pr3, nrow = 2, ncol = 2)

tas1 <- meanchange.plot(data = subset(tas.proj.feb.FRE, 1), colors = heat, baseline = tas.norm.feb.FRE, title = "Percent Change 2011 - 2040, SSP1 2.6")
tas2 <- meanchange.plot(data = subset(tas.proj.feb.FRE, 2), colors = heat, baseline = tas.norm.feb.FRE, title = "Percent Change 2041 - 2070, SSP1 2.6")
tas3 <- meanchange.plot(data = subset(tas.proj.feb.FRE, 3), colors = heat, baseline = tas.norm.feb.FRE, title = "Percent Change 2071 - 2100, SSP1 2.6")

ggarrange(tas.base.plot, tas1, tas2, tas3, nrow = 2, ncol = 2)


#plot seasonality change-----------------------------------------------------

pr.seas.base.plot <- ggplot() +
  ggtitle("Precipitation Seasonality, 1981 - 2010") +
  #annotation_map_tile(type = "osm", zoomin = 0) +
  geom_spatraster(data = subset(pr.seas.FRE, 1)) +
  #geom_sf(data = FRE, fill = NA, color = "black", linewidth = 0.2) +
  scale_fill_gradientn(colors = rain, na.value = NA) +
  coord_sf(datum = "ESRI:102001") +
  theme_void() +
  theme(plot.margin = unit(c(0.2, -0.5, -0.2, -0.5), "cm"), #top, right, bottom, left
        legend.position = c(0.9, 0.2),
        legend.key.size = unit(0.5, "cm"),
        legend.title = element_text(hjust = 0.5, face = "bold")) + 
  guides(fill = guide_colorbar(title.position = "top", ticks.colour = NA, barwidth = 10,
                               barheight = 2, title="Coefficient of Variation",  direction = "horizontal"))

tas.seas.base.plot <- ggplot() +
  ggtitle("Temperature Seasonality, 1981 - 2010") +
  #annotation_map_tile(type = "osm", zoomin = 0) +
  geom_spatraster(data = subset(tas.seas.FRE, 1)) +
  #geom_sf(data = FRE, fill = NA, color = "black", linewidth = 0.2) +
  scale_fill_gradientn(colors = heat, na.value = NA) +
  coord_sf(datum = "ESRI:102001") +
  theme_void() +
  theme(plot.margin = unit(c(0.2, -0.5, -0.2, -0.5), "cm"), #top, right, bottom, left
        legend.position = c(0.9, 0.2),
        legend.key.size = unit(0.5, "cm"),
        legend.title = element_text(hjust = 0.5, face = "bold")) + 
  guides(fill = guide_colorbar(title.position = "top", ticks.colour = NA, barwidth = 10,
                               barheight = 2, title="COefficient of Variation",  direction = "horizontal"))

pr.s.1 <- meanchange.plot(data = subset(pr.seas.FRE, 2), colors = rain, baseline = subset(pr.seas.FRE, 1), title = "Percent Change 2011 - 2040, SSP1 2.6")
pr.s.2 <- meanchange.plot(data = subset(pr.seas.FRE, 3), colors = rain, baseline = subset(pr.seas.FRE, 1), title = "Percent Change 2041 - 2070, SSP1 2.6")
pr.s.3 <- meanchange.plot(data = subset(pr.seas.FRE, 4), colors = rain, baseline = subset(pr.seas.FRE, 1), title = "Percent Change 2071 - 2100, SSP1 2.6")

ggarrange(pr.seas.base.plot, pr.s.1, pr.s.2, pr.s.3, nrow = 2, ncol = 2)

tas.s.1 <- meanchange.plot(data = subset(tas.seas.FRE, 2), colors = heat, baseline = subset(tas.seas.FRE, 1), title = "Percent Change 2011 - 2040, SSP1 2.6")
tas.s.2 <- meanchange.plot(data = subset(tas.seas.FRE, 3), colors = heat, baseline = subset(tas.seas.FRE, 1), title = "Percent Change 2041 - 2070, SSP1 2.6")
tas.s.3 <- meanchange.plot(data = subset(tas.seas.FRE, 4), colors = heat, baseline = subset(tas.seas.FRE, 1), title = "Percent Change 2071 - 2100, SSP1 2.6")

ggarrange(tas.seas.base.plot, tas.s.1, tas.s.2, tas.s.3, nrow = 2, ncol = 2)


#figures --------------------------------------------------------------------

#map mean temp/precip

estuaries <- readRDS("Data/Shapefiles/estuaries.rds")
pnw <- readRDS("Data/PNW_shapefile.rds")

rain <- c("#9ad1d4","#007ea7","#003249")

heat <- c("#ff9b54", "#ff7f51","#ce4257", "#720026","#4f000b")

ggplot() +
  annotation_map_tile(type = "osm", zoomin = 0) +
  geom_spatraster(data = pr.mean) +
  #geom_sf(data = estuaries, fill = NA, color = "black") + 
  geom_sf(data = pnw, fill = NA, color = "black", linewidth = 0.2) +
  scale_fill_gradientn('Mean Precipitation, 1979 - 2020', colors = rain, na.value = NA) +
  coord_sf(datum = "ESRI:102001") +
  theme_void() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        #panel.background = element_rect(fill = "transparent", color = NA),
        #plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2, -0.5, -0.2, -0.5), "cm"), #top, right, bottom, left
        legend.position = c(0.1, 0.3),
        legend.title = element_text(hjust = 0.5, face = "bold")) + 
  guides(fill = guide_colorbar(title.position = "top", ticks.colour = NA, barwidth = 2,
                               barheight = 10, title="Mean Precipitation",  direction = "vertical"))

ggsave("Figures/mean_precip.png",
       width = 6.13,
       height = 8.22, 
       dpi = 300)

ggplot() +
  geom_spatraster(data = tas.mean) +
  #geom_sf(data = estuaries, fill = NA, color = "black") + 
  geom_sf(data = pnw, fill = NA, color = "black", linewidth = 0.2) +
  scale_fill_gradientn('Mean Temperature, 1980 - 2020', colors = heat, na.value = NA) +
  coord_sf(datum = "ESRI:102001") +
  theme_void() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        #panel.background = element_rect(fill = "transparent", color = NA),
        #plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2, -0.5, -0.2, -0.5), "cm"), #top, right, bottom, left
        legend.position = c(0.3, 0.3),
        legend.title = element_text(hjust = 0.5, face = "bold")) + 
  guides(fill = guide_colorbar(title.position = "top", ticks.colour = NA, barwidth = 2,
                               barheight = 10, title="Mean Temperature (K/10)",  direction = "vertical"))

ggsave("Figures/mean_temp.png",
       width = 6.13,
       height = 8.22, 
       dpi = 300)
  

#map variance in temp/precip

ggplot() +
  geom_spatraster(data = log(pr.var)) +
  #geom_sf(data = estuaries, fill = NA, color = "black") + 
  geom_sf(data = pnw, fill = NA, color = "black", linewidth = 0.2) +
  scale_fill_gradientn('Mean Precipitation, 1979 - 2020', colors = rain, na.value = NA) +
  coord_sf(datum = "ESRI:102001") +
  theme_void() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        #panel.background = element_rect(fill = "transparent", color = NA),
        #plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2, -0.5, -0.2, -0.5), "cm"), #top, right, bottom, left
        legend.position = c(0.3, 0.3),
        legend.title = element_text(hjust = 0.5, face = "bold")) + 
  guides(fill = guide_colorbar(title.position = "top", ticks.colour = NA, barwidth = 2,
                               barheight = 10, title="log Variance in Precipitation",  direction = "vertical"))

ggsave("Figures/var_precip.png",
       width = 6.13,
       height = 8.22, 
       dpi = 300)

ggplot() +
  geom_spatraster(data = tas.var) +
  #geom_sf(data = estuaries, fill = NA, color = "black") + 
  geom_sf(data = pnw, fill = NA, color = "black", linewidth = 0.2) +
  scale_fill_gradientn('Mean Temperature, 1980 - 2020', colors = heat, na.value = NA) +
  coord_sf(datum = "ESRI:102001") +
  theme_void() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        #panel.background = element_rect(fill = "transparent", color = NA),
        #plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2, -0.5, -0.2, -0.5), "cm"), #top, right, bottom, left
        legend.position = c(0.3, 0.3),
        legend.title = element_text(hjust = 0.5, face = "bold")) + 
  guides(fill = guide_colorbar(title.position = "top", ticks.colour = NA, barwidth = 2,
                               barheight = 10, title="Variance in Temperature",  direction = "vertical"))

ggsave("Figures/var_temp.png",
       width = 6.13,
       height = 8.22, 
       dpi = 300)

#map extremes

ggplot() +
  geom_spatraster(data = pr.max) +
  #geom_sf(data = estuaries, fill = NA, color = "black") + 
  geom_sf(data = pnw, fill = NA, color = "black", linewidth = 0.2) +
  scale_fill_gradientn('Mean Precipitation, 1979 - 2020', colors = rain, na.value = NA) +
  coord_sf(datum = "ESRI:102001") +
  theme_void() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        #panel.background = element_rect(fill = "transparent", color = NA),
        #plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2, -0.5, -0.2, -0.5), "cm"), #top, right, bottom, left
        legend.position = c(0.3, 0.3),
        legend.title = element_text(hjust = 0.5, face = "bold")) + 
  guides(fill = guide_colorbar(title.position = "top", ticks.colour = NA, barwidth = 2,
                               barheight = 10, title="Max Precipitation",  direction = "vertical"))

ggsave("Figures/max_precip.png",
       width = 6.13,
       height = 8.22, 
       dpi = 300)

ggplot() +
  geom_spatraster(data = tas.max) +
  #geom_sf(data = estuaries, fill = NA, color = "black") + 
  geom_sf(data = pnw, fill = NA, color = "black", linewidth = 0.2) +
  scale_fill_gradientn('Mean Temperature, 1980 - 2020', colors = heat, na.value = NA) +
  coord_sf(datum = "ESRI:102001") +
  theme_void() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        #panel.background = element_rect(fill = "transparent", color = NA),
        #plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2, -0.5, -0.2, -0.5), "cm"), #top, right, bottom, left
        legend.position = c(0.3, 0.3),
        legend.title = element_text(hjust = 0.5, face = "bold")) + 
  guides(fill = guide_colorbar(title.position = "top", ticks.colour = NA, barwidth = 2,
                               barheight = 10, title="Max Temperature",  direction = "vertical"))

ggsave("Figures/max_temp.png",
       width = 6.13,
       height = 8.22, 
       dpi = 300)

ggplot() +
  geom_spatraster(data = pr.min) +
  #geom_sf(data = estuaries, fill = NA, color = "black") + 
  geom_sf(data = pnw, fill = NA, color = "black", linewidth = 0.2) +
  scale_fill_gradientn('Mean Precipitation, 1979 - 2020', colors = rain, na.value = NA) +
  coord_sf(datum = "ESRI:102001") +
  theme_void() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        #panel.background = element_rect(fill = "transparent", color = NA),
        #plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2, -0.5, -0.2, -0.5), "cm"), #top, right, bottom, left
        legend.position = c(0.3, 0.3),
        legend.title = element_text(hjust = 0.5, face = "bold")) + 
  guides(fill = guide_colorbar(title.position = "top", ticks.colour = NA, barwidth = 2,
                               barheight = 10, title="Min Precipitation",  direction = "vertical"))

ggsave("Figures/min_precip.png",
       width = 6.13,
       height = 8.22, 
       dpi = 300)

ggplot() +
  geom_spatraster(data = tas.min) +
  #geom_sf(data = estuaries, fill = NA, color = "black") + 
  geom_sf(data = pnw, fill = NA, color = "black", linewidth = 0.2) +
  scale_fill_gradientn('Mean Temperature, 1980 - 2020', colors = heat, na.value = NA) +
  coord_sf(datum = "ESRI:102001") +
  theme_void() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        #panel.background = element_rect(fill = "transparent", color = NA),
        #plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2, -0.5, -0.2, -0.5), "cm"), #top, right, bottom, left
        legend.position = c(0.3, 0.3),
        legend.title = element_text(hjust = 0.5, face = "bold")) + 
  guides(fill = guide_colorbar(title.position = "top", ticks.colour = NA, barwidth = 2,
                               barheight = 10, title="Min Temperature",  direction = "vertical"))

ggsave("Figures/min_temp.png",
       width = 6.13,
       height = 8.22, 
       dpi = 300)

#histogram of temp/precip values

terra::hist(pr.mean, main = "Mean Precipitation values")
terra::hist(tas.mean, main = "Mean temperature values")

#smooth of temp/precip over time



#OBSOLETE-----------------------------------------------------------------------
#import data files

pr.files <- list.files(path = 'Data/Precipitation/CHELSA/',
                       pattern = '.tif', full.names = TRUE)

tas.files <- list.files(path = 'Data/Temperature/CHELSA/',
                        pattern = '.tif', full.names = TRUE)

#save as single rasterstack file

pr.stack <- stack(pr.files)

saveRDS(pr.stack, "Data/Precipitation/pr_raster_stack.rds")

tas.stack <- stack(tas.files)

saveRDS(tas.stack, "Data/Temperature/tas_raster_stack.rds")

#convert rasters to dataframe

#precipitation
pr <- list()

for(i in 1:length(pr.files)){
  
  r <- rast(pr.files[i])
  
  df <- as.data.frame(r, xy = TRUE)
  
  df$month <- as.integer(substr(pr.files[i],
                                start = nchar(pr.files[i]) - nchar('mm_yyyy_V.2.1.tif') + 1,
                                stop = nchar(pr.files[i]) - nchar('_yyyy_V.2.1.tif')))
  
  df$year <- as.integer(substr(pr.files[i],
                               start = nchar(pr.files[i]) - nchar('yyyy_V.2.1.tif') + 1,
                               stop = nchar(pr.files[i]) - nchar('_V.2.1.tif')))
  
  names(df)[3] <- "precipitation"
  
  pr[[i]] <- df
  
}

pr <- do.call(rbind, pr)

#saveRDS(pr, "Data/Precipitation/pr_df.rds")
pr <- readRDS("Data/Precipitation/pr_df.rds")

#temperature
tas <- list()

for(i in 1:length(tas.files)){
  
  r <- rast(tas.files[i])
  
  df <- as.data.frame(r, xy = TRUE)
  
  df$month <- as.integer(substr(tas.files[i],
                                start = nchar(tas.files[i]) - nchar('mm_yyyy_V.2.1.tif') + 1,
                                stop = nchar(tas.files[i]) - nchar('_yyyy_V.2.1.tif')))
  
  df$year <- as.integer(substr(tas.files[i],
                               start = nchar(tas.files[i]) - nchar('yyyy_V.2.1.tif') + 1,
                               stop = nchar(tas.files[i]) - nchar('_V.2.1.tif')))
  
  names(df)[3] <- "temperature"
  
  tas[[i]] <- df
  
}

tas <- do.call(rbind, tas)

saveRDS(tas, "Data/Temperature/tas_df.rds")

#generate summary stats

#subset data to work within memory limits

pr1 <- pr[1:100000000,]

#remove heavy dataframe to save RAM, then proceed with next step

pr.stats <- unique(pr1[,1:2])

#load back in original dataframe
pr <- readRDS("Data/Precipitation/pr_df.rds")

#generate summary stats for each individual latlong point

pr.stats$mean <- base::mean(pr[!is.na(pr$x == pr.stats$x && pr$y == pr.stats$y), ])
pr.stats$var <- var(pr[pr$x == pr.stats$x && pr$y == pr.stats$y, ])
pr.stats$max <- max(pr[pr$x == pr.stats$x && pr$y == pr.stats$y, ])
pr.stats$min <- min(pr[pr$x == pr.stats$x && pr$y == pr.stats$y, ])
pr.stats$range <- range(pr[pr$x == pr.stats$x && pr$y == pr.stats$y, ])  

#subset data to work within memory limits

tas1 <- tas[1:100000000,]

tas.stats <- unique(tas1[,1:2])

tas.stats$mean <- mean(tas[tas$x == tas.stats$x && tas$y == tas.stats$y, ])
tas.stats$var <- var(tas[tas$x == tas.stats$x && tas$y == tas.stats$y, ])
tas.stats$max <- max(tas[tas$x == tas.stats$x && tas$y == tas.stats$y, ])
tas.stats$min <- min(tas[tas$x == tas.stats$x && tas$y == tas.stats$y, ])
tas.stats$range <- range(tas[tas$x == tas.stats$x && tas$y == tas.stats$y, ])







