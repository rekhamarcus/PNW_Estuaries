#test data

library(dplyr)
library(terra)
library(ggplot2)
library(raster)
library(tidyterra)
library(ggspatial)

setwd('C:/Users/rekhamarcus/OneDrive - University of Victoria/Wetlands/')

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







