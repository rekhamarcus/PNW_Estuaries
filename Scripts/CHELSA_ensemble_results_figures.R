#figures for results using ensemble models

library(dplyr)
library(terra)
library(sf)
library(ggplot2)
library(raster)
library(tidyterra)
library(ggspatial)
library(prettymapr)
library(ggpubr) # for arranging plots
library(reshape) #for melt function

setwd('C:/Users/rekha/OneDrive - University of Victoria/Wetlands/')
source("PNW_Estuaries/Functions/colors.R") #color palettes

#load in data -------------------------------------------------------------------

results <- readRDS("Data/Results/results.rds")

results <- results[!(results$SSP == "SSP3_7.0"),] #remove SSP3
results$SSP <- as.factor(results$SSP)
results$years <- as.factor(results$years)
results$region <- factor(results$region, levels = c("NCM", "HG", "NWVI", "SWBC", "SS", "WONC", "CC"))

results.summary <- readRDS("Data/Results/results_summary.rds")

r2041 <- results[(results$years == "2041_2070"),] #only results for mid century
results.summary.2041 <- readRDS("Data/Results/results_summary_2041.rds")
results.summary.2041.SSP1 <- readRDS("Data/Results/results_summary_2041_SSP1.rds")
results.summary.2041.SSP5 <- readRDS("Data/Results/results_summary_2041_SSP5.rds")

write.csv(r2041, "Data/Results/r2041.csv")

estuaries <- readRDS("Data/Shapefiles/estuaries.rds")

#color palettes
ssp.temp <- c("#fcaa67", "#b0413e")
ssp.precip <- c("lightblue", "#0077b6")

#region labels
labels <- rev(c("North Central BC", "Haida Gwaii", "Northwest Vancouver Island", "Southwest BC", "Salish Sea", "WA, OR, Northern CA", "Central California"))

#test/validate all models and check outputs ------------------------------------

vars <- results[,c(4:9, 22)]

df.m <- reshape2::melt(vars, id.var = "model")

variables <- c("Mean precipitation", "Precipitation standard deviation", "Precipitation expected shortfall",
            "Mean temperature", "Temperature standard deviation", "Temperature expected shortfall")

ggplot(df.m, aes(fill = model, x = variable, y = value)) +
  geom_hline(aes(yintercept = 0), colour = "gray") + 
  geom_boxplot() + 
  scale_fill_manual(values = colors5) +
  xlab("Results Variable") +
  ylab("Percent Change from Historical Mean") + 
  scale_x_discrete(labels = variables) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_text(size = 9, family = "sans", face = "bold"),
        axis.title.y = element_text(size = 9, family = "sans", face = "bold"),
        axis.text = element_text(size = 8, family = "sans"),
        legend.background = element_rect(fill = "transparent"),
        legend.key.size = unit(0.7, 'cm'),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.text = element_text(size=8, family = "sans"),
        legend.title = element_text(size = 9, family = "sans", face = "bold"),
        legend.position = "inside",
        legend.position.inside = c(0.1, 0.8))

ggsave("Figures/model.results.png",
       width = 10.5,
       height = 5,
       dpi = 600)


#mean results ------------------------------------------------------------------

#plot estuaries of interest for this variable

mean.est <- c(211, 3005, 2086, 188, 1152)

mean.estuaries <- estuaries[estuaries$EST_ID %in% mean.est, ]
mean.estuary.centers <- st_centroid(mean.estuaries)

mean.est.plot <- ggplot() +
  coord_sf(xlim = c(-133.130120075859, -121.937071717489), ylim = c(38.7578990718796, 55.9376038918338)) +
  annotation_map_tile(type = "hotstyle", zoomin = 1) +
  geom_sf(data = mean.estuary.centers, color = "#c9184a", fill = "#c9184a", size = 2, shape = 23) + 
  theme_void() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

ggsave("Figures/Results/mean.estuaries.map.png",
       width = 3.05,
       height = 7.86,
       dpi = 600)

#change in mean temp per region

mean.tas.plot <- ggplot(r2041, aes(fill = SSP, x = mean.tas, y = region)) +
  geom_vline(xintercept = 0, color = "grey") +
  geom_boxplot() +
  xlab("Percent Change in Mean Temperature") +
  scale_y_discrete(limits = rev(c("NCM", "HG", "NWVI", "SWBC", "SS", "WONC", "CC")), labels = labels) +
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
        legend.key.size = unit(0.7, 'cm'),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.text = element_text(size=6, family = "sans", face = "bold"),
        legend.position = "inside",
        legend.position.inside = c(0.9, 0.6))

#change in mean pr per region

mean.pr.plot <- ggplot(results, aes(fill = SSP, x = mean.pr, y = region)) +
  geom_vline(xintercept = 0, color = "grey") +
  geom_boxplot() +
  xlab("Percent Change in Mean Monthly Precipitation") +
  scale_y_discrete(limits = rev(c("NCM", "HG", "NWVI", "SWBC", "SS", "WONC", "CC")), labels = labels) +
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
        legend.key.size = unit(0.7, 'cm'),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.text = element_text(size=6, family = "sans", face = "bold"),
        legend.position = "inside",
        legend.position.inside = c(0.9, 0.6))

ggarrange(mean.tas.plot, mean.pr.plot, nrow = 2, labels = c("b", "c"))

ggsave("Figures/Results/mean.trends.2041.png",
       width = 10,
       height = 5,
       dpi = 600)

#SD results ------------------------------------------------------------------

SD.est <- c(1016, 2106, 3030, 181, 252, 1152)

SD.estuaries <- estuaries[estuaries$EST_ID %in% SD.est, ]
SD.estuary.centers <- st_centroid(SD.estuaries)

ggplot() +
  annotation_map_tile(type = "hotstyle", zoomin = 1) +
  geom_sf(data = SD.estuary.centers, color = "#c9184a", fill = "#c9184a", size = 2, shape = 23) + 
  theme_void() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

ggsave("Figures/Results/SD.estuaries.map.png",
       width = 3.05,
       height = 7.86,
       dpi = 600)

#change in SD temp per region

SD.tas.plot <- ggplot(r2041, aes(fill = SSP, x = SD.tas, y = region)) +
  geom_vline(xintercept = 0, color = "grey") +
  geom_boxplot() +
  xlab("Percent Change in Standard Deviation of Temperature") +
  scale_y_discrete(limits = rev(c("NCM", "HG", "NWVI", "SWBC", "SS", "WONC", "CC")), labels = labels) +
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
        legend.key.size = unit(0.7, 'cm'),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.text = element_text(size=6, family = "sans", face = "bold"),
        legend.position = "inside",
        legend.position.inside = c(0.9, 0.6))

#change in SD pr per region

SD.pr.plot <- ggplot(r2041, aes(fill = SSP, x = SD.pr, y = region)) +
  geom_vline(xintercept = 0, color = "grey") +
  geom_boxplot() +
  xlab("Percent Change in Standard Deviation of Precipitation") +
  scale_y_discrete(limits = rev(c("NCM", "HG", "NWVI", "SWBC", "SS", "WONC", "CC")), labels = labels) +
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
        legend.key.size = unit(0.7, 'cm'),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.text = element_text(size=6, family = "sans", face = "bold"),
        legend.position = "inside",
        legend.position.inside = c(0.9, 0.6))

#mean1 <- ggarrange(tas.years.plot, pr.years.plot, ncol = 2, labels = c("a", "b"))

ggarrange(SD.tas.plot, SD.pr.plot, nrow = 2, labels = c("b", "c"))

ggsave("Figures/Results/SD.trends.2041.png",
         width = 10,
         height = 5,
         dpi = 600)

#ES results ------------------------------------------------------------------

es.est <- c(1014, 203, 172, 1152, 181)

es.estuaries <- estuaries[estuaries$EST_ID %in% es.est, ]
es.estuary.centers <- st_centroid(es.estuaries)

ggplot() +
  annotation_map_tile(type = "hotstyle", zoomin = 1) +
  geom_sf(data = es.estuary.centers, color = "#c9184a", fill = "#c9184a", size = 2, shape = 23) + 
  geom_sf(data = mean.estuary.centers, alpha = 0) + #extend plot by plotting invisible sf layer with larger extent
  theme_void() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

ggsave("Figures/Results/es.estuaries.map.png",
       width = 3.05,
       height = 7.86,
       dpi = 600)

#change in ES temp per region

es.tas.plot <- ggplot(r2041, aes(fill = SSP, x = ES0.99.tas, y = region)) +
  geom_vline(xintercept = 0, color = "grey") +
  geom_boxplot() +
  xlab("Percent Change in Expected Shortfall of Temperature") +
  scale_y_discrete(limits = rev(c("NCM", "HG", "NWVI", "SWBC", "SS", "WONC", "CC")), labels = labels) +
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
        legend.key.size = unit(0.7, 'cm'),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.text = element_text(size=6, family = "sans", face = "bold"),
        legend.position = "inside",
        legend.position.inside = c(0.9, 0.6))

#change in ES pr per region

es.pr.plot <- ggplot(results, aes(fill = SSP, x = ES0.99.pr, y = region)) +
  geom_vline(xintercept = 0, color = "grey") +
  geom_boxplot() +
  xlab("Percent Change in Expected Shortfall of Precipitation") +
  scale_y_discrete(limits = rev(c("NCM", "HG", "NWVI", "SWBC", "SS", "WONC", "CC")), labels = labels) +
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
        legend.key.size = unit(0.7, 'cm'),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.text = element_text(size=6, family = "sans", face = "bold"),
        legend.position = "inside",
        legend.position.inside = c(0.9, 0.6))

#mean1 <- ggarrange(tas.years.plot, pr.years.plot, ncol = 2, labels = c("a", "b"))

ggarrange(es.tas.plot, es.pr.plot, nrow = 2, labels = c("b", "c"))

ggsave("Figures/Results/ES.trends.2041.png",
       width = 10,
       height = 5,
       dpi = 600)

#ES 95 results --------------------------------------------------------------------

#map results

es95.est <- c(1014, 203, 172, 1152, 182)

es95.estuaries <- estuaries[estuaries$EST_ID %in% es95.est, ]
es95.estuary.centers <- st_centroid(es95.estuaries)

ggplot() +
  annotation_map_tile(type = "hotstyle", zoomin = 1) +
  geom_sf(data = es95.estuary.centers, color = "#c9184a", fill = "#c9184a", size = 2, shape = 23) + 
  geom_sf(data = mean.estuary.centers, alpha = 0) + #extend plot by plotting invisible sf layer with larger extent
  theme_void() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

ggsave("Figures/Results/es.estuaries.map.png",
       width = 3.05,
       height = 7.86,
       dpi = 600)

#change in ES temp per region

es95.tas.plot <- ggplot(r2041, aes(fill = SSP, x = ES0.95.tas, y = region)) +
  geom_vline(xintercept = 0, color = "grey") +
  geom_boxplot() +
  xlab("Percent Change in Expected Shortfall of Temperature") +
  scale_y_discrete(limits = rev(c("NCM", "HG", "NWVI", "SWBC", "SS", "WONC", "CC")), labels = labels) +
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
        legend.key.size = unit(0.7, 'cm'),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.text = element_text(size=6, family = "sans", face = "bold"),
        legend.position = "inside",
        legend.position.inside = c(0.9, 0.6))

#change in ES pr per region

es95.pr.plot <- ggplot(results, aes(fill = SSP, x = ES0.95.pr, y = region)) +
  geom_vline(xintercept = 0, color = "grey") +
  geom_boxplot() +
  xlab("Percent Change in Expected Shortfall of Precipitation") +
  scale_y_discrete(limits = rev(c("NCM", "HG", "NWVI", "SWBC", "SS", "WONC", "CC")), labels = labels) +
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
        legend.key.size = unit(0.7, 'cm'),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.text = element_text(size=6, family = "sans", face = "bold"),
        legend.position = "inside",
        legend.position.inside = c(0.9, 0.6))

#mean1 <- ggarrange(tas.years.plot, pr.years.plot, ncol = 2, labels = c("a", "b"))

ggarrange(es95.tas.plot, es95.pr.plot, nrow = 2, labels = c("b", "c"))

ggsave("Figures/Results/ES95.trends.2041.png",
       width = 10,
       height = 5,
       dpi = 600)


#extra plots -----------------------------------------------------------

#change in mean temp per ssp/time period

var.tas <- results[,c(1,2,7)]

tas.years.plot <- ggplot(var.tas, aes(fill = SSP, x = years, y = mean.tas)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_boxplot() + 
  xlab("Time period") +
  ylab("Percent Change from Historical Mean") + 
  scale_fill_manual(values = ssp.temp) + 
  #scale_x_discrete(labels = variables) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_text(size = 9, family = "sans", face = "bold"),
        axis.title.y = element_text(size = 9, family = "sans", face = "bold"),
        axis.text = element_text(size = 8, family = "sans"),
        legend.background = element_rect(fill = "transparent"),
        legend.key.size = unit(0.7, 'cm'),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.text = element_text(size=8, family = "sans"),
        legend.title = element_text(size = 9, family = "sans", face = "bold"),
        legend.position = "inside",
        legend.position.inside = c(0.15, 0.85))

#change in mean pr per ssp/time period

var.pr <- results[,c(1,2,4)]

pr.years.plot <- ggplot(var.pr, aes(fill = SSP, x = years, y = mean.pr)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_boxplot() + 
  xlab("Time period") +
  ylab("Percent Change from Historical Mean") + 
  scale_fill_manual(values = ssp.precip) + 
  #scale_x_discrete(labels = variables) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_text(size = 9, family = "sans", face = "bold"),
        axis.title.y = element_text(size = 9, family = "sans", face = "bold"),
        axis.text = element_text(size = 8, family = "sans"),
        legend.background = element_rect(fill = "transparent"),
        legend.key.size = unit(0.7, 'cm'),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.text = element_text(size=8, family = "sans"),
        legend.title = element_text(size = 9, family = "sans", face = "bold"),
        legend.position = "inside",
        legend.position.inside = c(0.15, 0.85))

