#figures for results

library(dplyr)
library(terra)
library(sf)
library(ggplot2)
library(raster)
library(tidyterra)
library(ggspatial)
library(prettymapr)
library(ggpubr) # for arranging plots

setwd('C:/Users/rekha/OneDrive - University of Victoria/Wetlands/')

#read in results files
results <- readRDS("Data/Results/results.rds")
results <- results[!(results$SSP == "SSP3_7.0"),] #remove SSP3

results.summary <- readRDS("Data/Results/results_summary.rds")

r2041 <- results[(results$years == "2041_2070"),] #only results for mid century
results.summary.2041 <- readRDS("Data/Results/results_summary_2041.rds")
results.summary.2041.SSP1 <- readRDS("Data/Results/results_summary_2041_SSP1.rds")
results.summary.2041.SSP5 <- readRDS("Data/Results/results_summary_2041_SSP5.rds")

results$region <- as.factor(results$region)
results$SSP <- as.factor(results$SSP)

#read in estuaries file
estuaries <- readRDS("Data/Shapefiles/estuaries.rds")

#color palettes
ssp.temp <- c("#fcaa67", "#b0413e")
ssp.precip <- c("lightblue", "#0077b6")

#region labels
labels <- rev(c("WA, OR, Northern CA", "Southwest BC", "Salish Sea", "Northwest Vancouver Island", "North Central BC", "Haida Gwaii", "Central California"))

#plot results for mean - fig. 5 -------------------------------------------------

#ssp.colors <- c("#c9cba3", "#e26d5c")

#5a - change in mean temp per region

r2041$mean.tas <- as.numeric(r2041$mean.tas)

mean.tas.plot <- ggplot(r2041, aes(fill = SSP, x = mean.tas, y = region)) +
  geom_boxplot() +
  xlab("Percent Change in Mean Temperature") +
  scale_y_discrete(labels = labels) +
  scale_fill_manual(values = ssp.temp) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_text(size = 9, family = "sans", face = "bold"),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 8, family = "sans"),
        legend.background = element_rect(fill = "transparent"),
        legend.key.size = unit(0.7, 'cm'),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.text = element_text(size=6, family = "sans", face = "bold"),
        legend.position = "inside",
        legend.position.inside = c(0.9, 0.9))

#5b - change in mean pr per region

r2041$mean.pr <- as.numeric(r2041$mean.pr)

mean.pr.plot <- ggplot(r2041, aes(fill = SSP, x = mean.pr, y = region)) +
  geom_vline(xintercept = 0, color =) + 
  geom_boxplot() +
  xlab("Percent Change in Mean Precipitation") +
  scale_y_discrete(labels = labels) +
  scale_fill_manual(values = ssp.precip) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_text(size = 9, family = "sans", face = "bold"),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 8, family = "sans"),
        legend.background = element_rect(fill = "transparent"),
        legend.key.size = unit(0.7, 'cm'),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.text = element_text(size=6, family = "sans", face = "bold"),
        legend.position = "inside",
        legend.position.inside = c(0.9, 0.6))

#5c - temp latitude trends

#convert estuaries sf to points
es.points <- st_centroid(estuaries)

#convert to df preserving latlong
es.points.coords <- st_coordinates(es.points$geometry)
es.points.coords <- cbind(es.points.coords, es.points)

#bind latlong to results
results.coords <- left_join(results, es.points.coords, by = "EST_ID")
results.coords <- results.coords[,-c(24,25)]

names(results.coords)[22:23] <- c("Long", "Lat")

results.coords$Lat <- as.numeric(results.coords$Lat)

results.coords.2041.2070 <- results.coords[results.coords$years == "2041_2070",]

results.coords.nwps <- results.coords.2041.2070[!(results.coords.2041.2070$EST_ID %in% nwps),]

mean.tas.lat.plot <- ggplot(results.coords.2041.2070, aes(x = mean.tas, y = Lat, fill = SSP)) +
  geom_point(aes(colour = SSP), size = 0.5) + 
  geom_smooth(aes(colour = SSP, fill = SSP), method = "loess") + 
  xlab("Percent Change in Mean Temperature") +
  ylab("Latitude") +
  scale_color_manual(values = ssp.temp) +
  scale_fill_manual(values = ssp.temp) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title = element_text(size = 9, family = "sans", face = "bold"),
        axis.text = element_text(size = 8, family = "sans"),
        legend.background = element_rect(fill = "transparent"),
        legend.key.size = unit(0.7, 'cm'),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.position = "inside",
        legend.position.inside = c(0.9, 0.5),
        legend.text = element_text(size=6, family = "sans", face = "bold"))

#5d - pr latitude trends

mean.pr.lat.plot <- ggplot(results.coords.nwps, aes(x = mean.pr, y = Lat, fill = SSP)) +
  geom_point(aes(colour = SSP), size = 0.5) + 
  #geom_text(label = results.coords.sub$EST_ID, size = 1, check_overlap = TRUE) + 
  geom_smooth(aes(colour = SSP, fill = SSP), method = "loess") + 
  xlab("Percent Change in Mean Precipitation") +
  ylab("Latitude") +
  scale_color_manual(values = ssp.precip) +
  scale_fill_manual(values = ssp.precip) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title = element_text(size = 9, family = "sans", face = "bold"),
        axis.text = element_text(size = 8, family = "sans"),
        legend.background = element_rect(fill = "transparent"),
        legend.key.size = unit(0.7, 'cm'),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.position = "inside",
        legend.position.inside = c(0.9, 0.5),
        legend.text = element_text(size=6, family = "sans", face = "bold"))

mean1 <- ggarrange(mean.tas.lat.plot, mean.pr.lat.plot, ncol = 2, labels = c("c", "d"))

ggarrange(mean.tas.plot, mean.pr.plot, mean1, nrow = 3, labels = c("a", "b", ""))

ggsave("Figures/Results/mean.trends.png",
       width = 9.5,
       height = 8,
       dpi = 600)

#5e,f,g,h - maps of highest and lowest values in temp and precip

#plot results for sd - fig. 6 -------------------------------------------------

ssp.colors <- c("#c9cba3", "#e26d5c")
ssp.temp <- c("#fcaa67", "#b0413e")
ssp.precip <- c("lightblue", "#0077b6")


#6a - change in temp variability per region

results$SD.tas <- as.numeric(results$SD.tas)

sd.tas.plot <- ggplot(results, aes(fill = SSP, x = SD.tas, y = region)) +
  geom_boxplot() +
  xlab("Percent Change in Temperature Variability") +
  scale_y_discrete(labels = labels) +
  scale_fill_manual(values = ssp.temp) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_text(size = 9, family = "sans", face = "bold"),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 8, family = "sans"),
        legend.background = element_rect(fill = "transparent"),
        legend.position = "inside",
        legend.position.inside = c(0.9, 0.5),
        legend.key.size = unit(0.7, 'cm'),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.text = element_text(size=6, family = "sans", face = "bold"))

#6b - change in pr variability by region

results$SD.pr <- as.numeric(results$SD.pr)

sd.pr.plot <- ggplot(results, aes(fill = SSP, x = SD.pr, y = region)) +
  geom_boxplot() +
  xlab("Percent Change in Precipitation Variability") +
  scale_y_discrete(labels = labels) +
  scale_fill_manual(values = ssp.precip) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_text(size = 9, family = "sans", face = "bold"),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 8, family = "sans"),
        legend.background = element_rect(fill = "transparent"),
        legend.position = "inside",
        legend.position.inside = c(0.9, 0.3),
        legend.key.size = unit(0.7, 'cm'),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.text = element_text(size=6, family = "sans", face = "bold"))

#6c - latlong trends of estuaries

#convert estuaries sf to points
es.points <- st_centroid(estuaries)

#convert to df preserving latlong
es.points.coords <- st_coordinates(es.points$geometry)
es.points.coords <- cbind(es.points.coords, es.points)

#bind latlong to results
results.coords <- left_join(results, es.points.coords, by = "EST_ID")
results.coords <- results.coords[,-c(24,25)]

names(results.coords)[22:23] <- c("Long", "Lat")

results.coords$Lat <- as.numeric(results.coords$Lat)

results.coords.2041.2070 <- results.coords[results.coords$years == "2041_2070",]

results.coords.nocal <- results.coords.2041.2070[!(results.coords.2041.2070$EST_ID %in% nocal),]

sd.tas.lat.plot <- ggplot(results.coords.nocal, aes(x = SD.tas, y = Lat, fill = SSP)) +
  geom_point(aes(colour = SSP), size = 0.5) + 
  geom_smooth(aes(colour = SSP, fill = SSP), method = "loess") +
  #geom_text(label = results.coords.nocal$EST_ID, check_overlap = TRUE, size = 2) +
  xlab("Percent Change in Temperature Variability") +
  ylab("Latitude") +
  scale_color_manual(values = ssp.temp) +
  scale_fill_manual(values = ssp.temp) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title = element_text(size = 9, family = "sans", face = "bold"),
        axis.text = element_text(size = 8, family = "sans"),
        legend.background = element_rect(fill = "transparent"),
        legend.key.size = unit(0.7, 'cm'),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.position = "inside",
        legend.position.inside = c(0.9, 0.8),
        legend.text = element_text(size=6, family = "sans", face = "bold"))

#6d - pr latitude trends

sd.pr.lat.plot <- ggplot(results.coords.2041.2070, aes(x = SD.pr, y = Lat, fill = SSP)) +
  geom_point(aes(colour = SSP), size = 0.5) + 
  geom_smooth(aes(colour = SSP, fill = SSP), method = "loess") + 
  xlab("Percent Change in Precipitation Variability") +
  scale_color_manual(values = ssp.precip) +
  scale_fill_manual(values = ssp.precip) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_text(size = 9, family = "sans", face = "bold"),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 8, family = "sans"),
        legend.background = element_rect(fill = "transparent"),
        legend.key.size = unit(0.7, 'cm'),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.position = "inside",
        legend.position.inside = c(0.9, 0.3),
        legend.text = element_text(size=6, family = "sans", face = "bold"))

sd1 <- ggarrange(sd.tas.lat.plot, sd.pr.lat.plot, ncol = 2, labels = c("c", "d"))

ggarrange(sd.tas.plot, sd.pr.plot, sd1, nrow = 3, labels = c("a", "b", ""))

ggsave("Figures/Results/sd.trends.png",
       width = 9.5,
       height = 8,
       dpi = 600)


#plot results for extremes - fig. 7 -------------------------------------------------

#7a - change in temp extremes per region

results$ES0.99.tas <- as.numeric(results$ES0.99.tas)

ext.tas.plot <- ggplot(results, aes(fill = SSP, x = ES0.99.tas, y = region)) +
  geom_boxplot() +
  xlab("Percent Change in Magnitude of Extreme Temperatures") +
  scale_y_discrete(labels = labels) +
  scale_fill_manual(values = ssp.temp) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_text(size = 9, family = "sans", face = "bold"),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 8, family = "sans"),
        legend.background = element_rect(fill = "transparent"),
        legend.key.size = unit(0.7, 'cm'),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.text = element_text(size=6, family = "sans", face = "bold"),
        legend.position = "inside",
        legend.position.inside = c(0.9, 0.5))

#7b - change in pr extremes by region

results$ES0.99.pr <- as.numeric(results$ES0.99.pr)

ext.pr.plot <- ggplot(results, aes(fill = SSP, x = ES0.99.pr, y = region)) +
  geom_boxplot() +
  xlab("Percent Change in Magnitude of Extreme Precipitation") +
  scale_y_discrete(labels = labels) +
  scale_fill_manual(values = ssp.precip) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_text(size = 9, family = "sans", face = "bold"),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 8, family = "sans"),
        legend.background = element_rect(fill = "transparent"),
        legend.key.size = unit(0.7, 'cm'),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.text = element_text(size=6, family = "sans", face = "bold"),
        legend.position = "inside",
        legend.position.inside = c(0.9, 0.3))

#7c - latlong trends of estuaries

#convert estuaries sf to points
es.points <- st_centroid(estuaries)

#convert to df preserving latlong
es.points.coords <- st_coordinates(es.points$geometry)
es.points.coords <- cbind(es.points.coords, es.points)

#bind latlong to results
results.coords <- left_join(results, es.points.coords, by = "EST_ID")
results.coords <- results.coords[,-c(24,25)]

names(results.coords)[22:23] <- c("Long", "Lat")

results.coords$Lat <- as.numeric(results.coords$Lat)

results.coords.2041.2070 <- results.coords[results.coords$years == "2041_2070",]

ext.tas.lat.plot <- ggplot(results.coords.2041.2070, aes(x = ES0.99.tas, y = Lat, fill = SSP)) +
  geom_point(aes(colour = SSP), size = 0.5) + 
  geom_smooth(aes(colour = SSP, fill = SSP), method = "loess") + 
  xlab("Percent Change in magnitude of Extreme Temperatures") +
  ylab("Latitude") +
  scale_color_manual(values = ssp.temp) +
  scale_fill_manual(values = ssp.temp) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title = element_text(size = 9, family = "sans", face = "bold"),
        axis.text = element_text(size = 8, family = "sans"),
        legend.background = element_rect(fill = "transparent"),
        legend.key.size = unit(0.7, 'cm'),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.position = "inside",
        legend.position.inside = c(0.9, 0.75),
        legend.text = element_text(size=6, family = "sans", face = "bold"))

#7d - pr latitude trends

ext.pr.lat.plot <- ggplot(results.coords.2041.2070, aes(x = ES0.99.pr, y = Lat, fill = SSP)) +
  geom_point(aes(colour = SSP), size = 0.5) + 
  geom_smooth(aes(colour = SSP, fill = SSP), method = "loess") + 
  xlab("Percent Change in Magnitude of Extreme Precipitation") +
  ylab("Latitude") +
  scale_color_manual(values = ssp.precip) +
  scale_fill_manual(values = ssp.precip) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title = element_text(size = 9, family = "sans", face = "bold"),
        axis.text = element_text(size = 8, family = "sans"),
        legend.background = element_rect(fill = "transparent"),
        legend.key.size = unit(0.7, 'cm'),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.position = "inside",
        legend.position.inside = c(0.9, 0.75),
        legend.text = element_text(size=6, family = "sans", face = "bold"))

ext1 <- ggarrange(ext.tas.lat.plot, ext.pr.lat.plot, ncol = 2, labels = c("c", "d"))

ggarrange(ext.tas.plot, ext.pr.plot, ext1, nrow = 3, labels = c("a", "b", ""))

ggsave("Figures/Results/extreme.trends.png",
       width = 9.5,
       height = 8,
       dpi = 600)


#for loop to extract min/max for each region etc--------------------------------

regions <- unique(results$region)
ssps <- unique(results$SSP)

maxmin.table <- list()

for(i in 1:length(regions)){
  
  reg <- filter(results, results$region == regions[i])
  
  reg <- reg[,-c(6,8,9,12,14:20)]
  
  regmaxmin <- list()
  
  for(j in 1:length(ssps)){
    
    scen <- filter(reg, reg$SSP == ssps[j])
    
    a <- filter(scen, abs(scen$mean.tas) == max(abs(scen$mean.tas)))
    b <- filter(scen, abs(scen$mean.tas) == min(abs(scen$mean.tas)))
    c <- filter(scen, abs(scen$mean.pr) == max(abs(scen$mean.pr)))
    d <- filter(scen, abs(scen$mean.pr) == min(abs(scen$mean.pr)))
    e <- filter(scen, abs(scen$SD.tas) == max(abs(scen$SD.tas)))
    f <- filter(scen, abs(scen$SD.tas) == min(abs(scen$SD.tas)))
    g <- filter(scen, abs(scen$SD.pr) == max(abs(scen$SD.pr)))
    h <- filter(scen, abs(scen$SD.pr) == min(abs(scen$SD.pr)))
    k <- filter(scen, abs(scen$ES0.99.tas) == max(abs(scen$ES0.99.tas)))
    l <- filter(scen, abs(scen$ES0.99.tas) == min(abs(scen$ES0.99.tas)))
    m <- filter(scen, abs(scen$ES0.99.pr) == max(abs(scen$ES0.99.pr)))
    n <- filter(scen, abs(scen$ES0.99.pr) == min(abs(scen$ES0.99.pr)))
    
    regmaxmin[[j]] <- rbind(a, b, c, d, e, f, g, h, k, l, m, n)
    
  }
  
  maxmin.table[[i]] <- do.call(rbind, regmaxmin)
  
}

maxmin <- do.call(rbind, maxmin.table)

saveRDS(maxmin, "Data/Results/maxmin.results.rds")

maxmin <- readRDS("Data/Results/maxmin.results.rds")

write.csv(maxmin, "Data/Results/maxmin.results.csv")


#find n numbers for each region

ss <- results[results$region == "SS",] #166 estuaries
wonc <- results[results$region == "WONC",] #109 estuaries
cc <- results[results$region == "CC",] #31 estuaries
swbc <- results[results$region == "SWBC",] #65 estuaries
nwvi <- results[results$region == "NWVI",] #120 estuaries
hg <- results[results$region == "HG",] #47 estuaries
ncm <- results[results$region == "NCM",] #207 estuaries

#find weird salish sea estuaries

ss.wonky <- results.coords[which(results.coords$Lat > 47.5 & results.coords$Lat < 48.8 & results.coords$Long > -123.0 & results.coords$Long < -122),]

unique(ss.wonky$EST_ID)


#plot case study figures ----------------------------------------------------

#subset region
results.cc <- results[results$region == "CC",]
results.cc <- inner_join(results.cc, estuaries, by = "EST_ID")

results.cc <- st_as_sf(results.cc)

results.cc.ssp5 <- results.cc[results.cc$SSP == "SSP5_8.5",]
results.cc.ssp5 <- results.cc.ssp5[results.cc.ssp5$years == "2041_2070",]

#percent change in mean

ggplot(results.cc.ssp5, aes(fill = as.numeric(mean.tas))) +
  annotation_map_tile(type = "osm", zoom = 10) +
  geom_sf(color = "black", linewidth = 0.03) +
  labs(title = "Percent Change in Temperature", subtitle = "Central California, SSP5") + 
  scale_fill_gradient(low = "#fdf0d5", high = "#e63946", name = "Percent Change") +
  theme_void() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 6, face = "bold", hjust = 0.5), 
        legend.text = element_text(size = 6, face = "bold"),
        legend.title = element_text(size = 8, face = "bold", hjust = 0.5),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.position = "bottom") +
  guides(fill = guide_colorbar(title.position = "top", ticks.colour = NA,
                               barwidth = 15, barheight = 0.2,
                               title = "Percent Change",direction = "horizontal"))

ggsave("Figures/Results/tasmean_cc_ssp5.png",
       width = 7.7,
       height = 6.7,
       dpi = 600)

ggplot(results.cc.ssp5, aes(fill = as.numeric(mean.pr))) +
  annotation_map_tile(type = "osm", zoom = 10) +
  geom_sf(color = "black", linewidth = 0.03) +
  labs(title = "Percent Change in Precipitation", subtitle = "Central California, SSP5") + 
  scale_fill_gradient(low = "#caf0f8", high = "#03045e", name = "Percent Change") +
  theme_void() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 6, face = "bold", hjust = 0.5), 
        legend.text = element_text(size = 6, face = "bold"),
        legend.title = element_text(size = 8, face = "bold", hjust = 0.5),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.position = "bottom") +
  guides(fill = guide_colorbar(title.position = "top", ticks.colour = NA,
                               barwidth = 15, barheight = 0.2,
                               title = "Percent Change",direction = "horizontal"))

ggsave("Figures/Results/prmean_cc_ssp5.png",
       width = 7.7,
       height = 6.7,
       dpi = 600)

#percent change in sd

ggplot(results.cc.ssp5, aes(fill = as.numeric(SD.tas))) +
  annotation_map_tile(type = "osm", zoom = 10) +
  geom_sf(color = "black", linewidth = 0.03) +
  labs(title = "Percent Change in Temperature Variability", subtitle = "Central California, SSP5") + 
  scale_fill_gradient(low = "#fdf0d5", high = "#e63946", name = "Percent Change") +
  theme_void() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 6, face = "bold", hjust = 0.5), 
        legend.text = element_text(size = 6, face = "bold"),
        legend.title = element_text(size = 8, face = "bold", hjust = 0.5),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.position = "bottom") +
  guides(fill = guide_colorbar(title.position = "top", ticks.colour = NA,
                               barwidth = 15, barheight = 0.2,
                               title = "Percent Change",direction = "horizontal"))

ggsave("Figures/Results/tasSD_cc_ssp5.png",
       width = 7.7,
       height = 6.7,
       dpi = 600)

ggplot(results.cc.ssp5, aes(fill = as.numeric(SD.pr))) +
  annotation_map_tile(type = "osm", zoom = 10) +
  geom_sf(color = "black", linewidth = 0.03) +
  labs(title = "Percent Change in Precipitation Variability", subtitle = "Central California, SSP5") + 
  scale_fill_gradient(low = "#caf0f8", high = "#03045e", name = "Percent Change") +
  theme_void() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 6, face = "bold", hjust = 0.5), 
        legend.text = element_text(size = 6, face = "bold"),
        legend.title = element_text(size = 8, face = "bold", hjust = 0.5),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.position = "bottom") +
  guides(fill = guide_colorbar(title.position = "top", ticks.colour = NA,
                               barwidth = 15, barheight = 0.2,
                               title = "Percent Change",direction = "horizontal"))

ggsave("Figures/Results/prSD_cc_ssp5.png",
       width = 7.7,
       height = 6.7,
       dpi = 600)

#percent change in extremes

ggplot(results.cc.ssp5, aes(fill = as.numeric(ES0.99.tas))) +
  annotation_map_tile(type = "osm", zoom = 10) +
  geom_sf(color = "black", linewidth = 0.03) +
  labs(title = "Percent Change in Temperature Extremes", subtitle = "Central California, SSP5") + 
  scale_fill_gradient(low = "#fdf0d5", high = "#e63946", name = "Percent Change") +
  theme_void() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 6, face = "bold", hjust = 0.5), 
        legend.text = element_text(size = 6, face = "bold"),
        legend.title = element_text(size = 8, face = "bold", hjust = 0.5),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.position = "bottom") +
  guides(fill = guide_colorbar(title.position = "top", ticks.colour = NA,
                               barwidth = 15, barheight = 0.2,
                               title = "Percent Change",direction = "horizontal"))

ggsave("Figures/Results/tasES99_cc_ssp5.png",
       width = 7.7,
       height = 6.7,
       dpi = 600)

ggplot(results.cc.ssp5, aes(fill = as.numeric(ES0.99.pr))) +
  annotation_map_tile(type = "osm", zoom = 10) +
  geom_sf(color = "black", linewidth = 0.03) +
  labs(title = "Percent Change in Precipitation Extremes", subtitle = "Central California, SSP5") + 
  scale_fill_gradient(low = "#caf0f8", high = "#03045e", name = "Percent Change") +
  theme_void() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 6, face = "bold", hjust = 0.5), 
        legend.text = element_text(size = 6, face = "bold"),
        legend.title = element_text(size = 8, face = "bold", hjust = 0.5),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.position = "bottom") +
  guides(fill = guide_colorbar(title.position = "top", ticks.colour = NA,
                               barwidth = 15, barheight = 0.2,
                               title = "Percent Change",direction = "horizontal"))

ggsave("Figures/Results/prES99_cc_ssp5.png",
       width = 7.7,
       height = 6.7,
       dpi = 600)

#subset out problem areas ------------------------------------------------------

#subset out problematic regions to highlight overall trends
nwps <- seq(from = 1142, to = 1155, by = 1) #northwest puget sound
nwps[15] <- 1029
nwps[16] <- 1036

nocal <- c(2103:2109, 3000, 3001, 2094) #northern california

results.nwps <- results[!(results$EST_ID %in% nwps),]
results.nocal <- results[!(results$EST_ID %in% nocal),]

