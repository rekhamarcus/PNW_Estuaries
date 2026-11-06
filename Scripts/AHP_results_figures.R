#figures for chapter 2 results - AHP

library('tidyr')
library('dplyr')
library('ggplot2')
library('terra')
library('sf')
library('ggpubr')
library('ggridges')
library('forcats') #for reordering data

setwd("C:/Users/rekha/OneDrive - University of Victoria/Wetlands")

source("PNW_Estuaries/Functions/colors.R") #color palettes

#load in data

maintaining <- read.csv("PNW_Estuaries/AHP/maintaining/estuary_assessment_maintaining_scores.csv")
improving <- read.csv("PNW_Estuaries/AHP/improving/estuary_assessment_improving_scores.csv")
shorebirds <- read.csv("PNW_Estuaries/AHP/shorebirds/estuary_assessment_shorebirds_scores.csv")
waterfowl <- read.csv("PNW_Estuaries/AHP/waterfowl/estuary_assessment_waterfowl_scores.csv")

weight.comp <- read.csv("PNW_Estuaries/AHP/variable_weight_comparison.csv")

estuaries <- readRDS("Data/shapefiles/estuaries.rds")
names <- read.csv("PNW_Estuaries/Data/estuary_names.csv")

#figure 4 - boxplots comparing weights ----------------------------------------

#group weightings into 4 categories
maintaining.weights <- weight.comp[,c(1,6,7,9,10,11,14)]
improving.weights <- weight.comp[,c(1,6,7,10,11,12,14)]
shorebird.weights <- weight.comp[,c(1,6,7,8,10,11,13,14)]
waterfowl.weights <- weight.comp[,c(1,6,7,8,10,11,12,13,14)]
model.weights <- weight.comp[,c(1:5)]

write.csv(maintaining.weights, "Ranking/AHP/maintaining_weights.csv")
write.csv(improving.weights, "Ranking/AHP/improving_weights.csv")
write.csv(shorebird.weights, "Ranking/AHP/shorebirds_weights.csv")
write.csv(waterfowl.weights, "Ranking/AHP/waterfowl_weights.csv")

#add labels
maintaining.weights$model <- "maintaining"
improving.weights$model <- "improving"
shorebird.weights$model <- "shorebirds"
waterfowl.weights$model <- "waterfowl"

#pivot tables into long format
maintaining.weights <- pivot_longer(maintaining.weights, cols = c(2:7))
improving.weights <- pivot_longer(improving.weights, cols = c(2:7))
shorebird.weights <- pivot_longer(shorebird.weights, cols = c(2:8))
waterfowl.weights <- pivot_longer(waterfowl.weights, cols = c(2:9))
model.weights <- pivot_longer(model.weights, cols = c(2:5))
names(model.weights)[2] <- "model"

#join into single dataset
weights <- rbind(maintaining.weights, improving.weights, shorebird.weights, waterfowl.weights)

#reorder factor levels for proper plotting

weights <- mutate(weights, name = fct_relevel(Variable, 
                          "Mean temperature", "Temperature variability",
                          "Temperature extremes", "Mean precipitation",
                          "Precipitation variability", "Precipitation extremes"))

model.weights <- mutate(model.weights, name = fct_relevel(Variable, 
                                              "Mean temperature", "Temperature variability",
                                              "Temperature extremes", "Mean precipitation",
                                              "Precipitation variability", "Precipitation extremes"))

#subset out different variable groupings for plots
clim.vars <- c("Mean temperature", "Temperature variability", "Temperature extremes", 
               "Mean precipitation", "Precipitation variability", "Precipitation extremes")
weights.clim <- weights[which(weights$Variable %in% clim.vars),]
model.weights.clim <- model.weights[which(model.weights$Variable %in% clim.vars),]

bird.vars <- c("Bird richness", "Bird relative abundance")
weights.bird <- weights[which(weights$Variable %in% bird.vars),]
model.weights.bird <- model.weights[which(model.weights$Variable %in% bird.vars),]

general.vars <- c("Climate impacts", "Bird presence", "Size", "Sea level rise erosion risk")
weights.general <- weights[which(weights$Variable %in% general.vars),]
model.weights.general <- model.weights[which(model.weights$Variable %in% general.vars),]

#create boxplot for climate data

weights.clim.boxplot <- ggplot() +
  geom_boxplot(data = weights.clim, aes(fill = model, x = name, y = value), coef = Inf) + 
  geom_point(data = model.weights.clim, aes(fill = model, x = name, y = value), shape = 8, position = position_dodge(width = 0.75)) +
  scale_fill_manual(values = colors4) +
  ylab("Calculated weight") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 9, family = "sans", face = "bold"),
        axis.text = element_text(size = 8, family = "sans"),
        legend.background = element_rect(fill = "transparent"),
        legend.key.size = unit(0.7, 'cm'),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.text = element_text(size=8, family = "sans"),
        legend.title = element_text(size = 9, family = "sans", face = "bold"),
        legend.position = "none")

#create boxplot for bird data

weights.bird.boxplot <- ggplot() +
  geom_boxplot(data = weights.bird, aes(fill = model, x = Variable, y = value), coef = Inf) + 
  geom_point(data = model.weights.bird, aes(fill = model, x = Variable, y = value), shape = 8, position = position_dodge(width = 0.75)) +
  scale_fill_manual(values = colors4) +
  ylab("Calculated weight") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 9, family = "sans", face = "bold"),
        axis.text = element_text(size = 8, family = "sans"),
        legend.background = element_rect(fill = "transparent"),
        legend.key.size = unit(0.7, 'cm'),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.text = element_text(size=8, family = "sans"),
        legend.title = element_text(size = 9, family = "sans", face = "bold"),
        legend.position = "none")

#create boxplot for general data categories

weights.general.boxplot <- ggplot() +
  geom_boxplot(data = weights.general, aes(fill = model, x = Variable, y = value), coef = Inf) + 
  geom_point(data = model.weights.general, aes(fill = model, x = Variable, y = value), shape = 8, position = position_dodge(width = 0.75)) +
  scale_fill_manual(values = colors4) +
  ylab("Calculated weight") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 9, family = "sans", face = "bold"),
        axis.text = element_text(size = 8, family = "sans"),
        legend.background = element_rect(fill = "transparent"),
        legend.key.size = unit(0.7, 'cm'),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.text = element_text(size=8, family = "sans"),
        legend.title = element_text(size = 9, family = "sans", face = "bold"),
        legend.position = "none")

sub <- ggarrange(weights.general.boxplot, weights.bird.boxplot, labels = c("a", "b"), widths = c(2, 1))

ggarrange(sub, weights.clim.boxplot, nrow = 2, labels = c("","c"), common.legend = TRUE)

ggsave("Figures/AHP/AHP.weights.png",
       width = 8,
       height = 7,
       dpi = 600)
  

#figure 5 - ridgeline plots comparing model results by region ------------------

#add in regions to datasets

#subset all results by region
SWBC <- readRDS("Data/Shapefiles/Regions/southwestern_BC.rds")
NWVI <- readRDS("Data/Shapefiles/Regions/northwest_VI.rds")
NCM <- readRDS("Data/Shapefiles/Regions/northcentral_BC.rds")
HG <- readRDS("Data/Shapefiles/Regions/haida_gwaii.rds")
SS <- readRDS("Data/Shapefiles/Regions/salish_sea.rds")
WONC <- readRDS("Data/Shapefiles/Regions/WA_OR_northernCA.rds")
CC <- readRDS("Data/Shapefiles/Regions/central_CA.rds")

#add labels to plots
labels <- rev(c("North Central BC", "Haida Gwaii, BC", "Northwest Vancouver Island, BC", "Southwest BC", "Puget Sound, WA", "WA, OR, Northern CA", "Central California"))
labels <- rev(c("North Central BC", "Haida Gwaii, BC", expression(atop("Northwest", paste("Vancouver Island, BC"))), "Southwest BC", "Puget Sound, WA", "WA, OR, Northern CA", "Central California"))

regions <- function(dataset) {
  
  results.SWBC <- dataset[dataset$EST_ID %in% SWBC$EST_NO,]
  results.NWVI <- dataset[dataset$EST_ID %in% NWVI$EST_NO,]
  results.NCM <- dataset[dataset$EST_ID %in% NCM$EST_NO,]
  results.HG <- dataset[dataset$EST_ID %in% HG$EST_NO,]
  results.SS <- dataset[dataset$EST_ID %in% SS$PMEP_EstuaryID,]
  results.WONC <- dataset[dataset$EST_ID %in% WONC$PMEP_EstuaryID,]
  results.CC <- dataset[dataset$EST_ID %in% CC$PMEP_EstuaryID,]
  
  #add in regions to results dataset
  results.SWBC$region <- "SWBC"
  results.NWVI$region <- "NWVI"
  results.NCM$region <- "NCM"
  results.HG$region <- "HG"
  results.SS$region <- "SS"
  results.WONC$region <- "WONC"
  results.CC$region <- "CC"
  
  #save all results in single file
  rbind(results.SWBC, results.NWVI, results.NCM, results.HG, results.SS, results.WONC, results.CC) 
  
}

maintaining <- regions(maintaining)
improving <- regions(improving)
shorebirds <- regions(shorebirds)
waterfowl <- regions(waterfowl)

maintaining.ridges <- ggplot(maintaining, aes(x = cumulative_score, y = region, fill = cumulative_score)) +
  geom_density_ridges(color = "black", fill = "#fb8500") +
  theme_ridges() + 
  labs(title = "Maintaining") +
  scale_y_discrete(labels = labels, expand = expansion(mult = 0)) +
  scale_x_continuous(expand = expansion(mult = 0)) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_text(size = 12, family = "sans", face = "bold"),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 12, family = "sans", face = "bold"),
        axis.text = element_text(size = 10, family = "sans"))

improving.ridges <- ggplot(improving, aes(x = cumulative_score, y = region, fill = cumulative_score)) +
  geom_density_ridges(color = "black", fill = "#e63946") +
  theme_ridges() + 
  labs(title = "Improving") +
  scale_y_discrete(labels = labels, expand = expansion(mult = 0)) +
  scale_x_continuous(expand = expansion(mult = 0)) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_text(size = 12, family = "sans", face = "bold"),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 12, family = "sans", face = "bold"),
        axis.text = element_text(size = 10, family = "sans"))

shorebirds.ridges <- ggplot(shorebirds, aes(x = cumulative_score, y = region, fill = cumulative_score)) +
  geom_density_ridges(color = "black", fill = "#ffb703") +
  theme_ridges() + 
  labs(title = "Shorebirds") +
  scale_y_discrete(labels = labels, expand = expansion(mult = 0)) +
  scale_x_continuous(expand = expansion(mult = 0)) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_text(size = 12, family = "sans", face = "bold"),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 12, family = "sans", face = "bold"),
        axis.text = element_text(size = 10, family = "sans"))

waterfowl.ridges <- ggplot(waterfowl, aes(x = cumulative_score, y = region, fill = cumulative_score)) +
  geom_density_ridges(color = "black", fill = "#457b9d") +
  theme_ridges() + 
  labs(title = "Waterfowl") +
  scale_y_discrete(labels = labels, expand = expansion(mult = 0)) +
  scale_x_continuous(expand = expansion(mult = 0)) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_text(size = 12, family = "sans", face = "bold"),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 12, family = "sans", face = "bold"),
        axis.text = element_text(size = 10, family = "sans"))

#add in boxplot of scores in all 4 models for comparison

#bind together all datasets

maintaining$model <- "maintaining"
improving$model <- "improving"
shorebirds$model <- "shorebirds"
waterfowl$model <- "waterfowl"

all.models <- rbind(maintaining, improving, shorebirds, waterfowl)

all.boxplot <- ggplot(all.models, aes(x=cumulative_score, y=model, fill=model)) +
  geom_boxplot() +
  labs(title = "Overall Scores") +
  scale_fill_manual(values = colors4) +
  theme_bw() + 
  theme(legend.position="none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(size = 12, family = "sans", face = "bold"),
        axis.text = element_text(size = 10, family = "sans"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

ridges <- ggarrange(improving.ridges, maintaining.ridges, shorebirds.ridges, waterfowl.ridges, nrow = 2, ncol = 2, labels = c("a", "b", "c", "d"))
ggarrange(ridges, all.boxplot, nrow = 2, labels = c("", "e"), heights = c(3, 1))

ggsave("Figures/figure5_ridges.png",
       width = 8,
       height = 11,
       dpi = 600)


#OUTDATED ------------------------------------------------------------------------

#figure 4 - boxplots comparing weights ----------------------------------------

#group weightings into 2 categories
cumulative <- weight.comp[,c(1:5)]
individual <- weight.comp[,c(1,6:14)]

#add labels
cumulative$type <- "cumulative"
individual$type <- "individual"

#pivot tables into long format
cumulative <- pivot_longer(cumulative, cols = c(2:5))
individual <- pivot_longer(individual, cols = c(2:10))

#join into single dataset
weights <- rbind(cumulative, individual)

#subset out different variable groupings for plots
clim.vars <- c("Mean temperature", "Temperature variability", "Temperature extremes", 
               "Mean precipitation", "Precipitation variability", "Precipitation extremes")
weights.clim <- weights[which(weights$Variable %in% clim.vars),]

bird.vars <- c("Bird richness", "Bird relative abundance")
weights.bird <- weights[which(weights$Variable %in% bird.vars),]

general.vars <- c("Climate impacts", "Bird presence", "Size", "Sea level rise erosion risk")
weights.general <- weights[which(weights$Variable %in% general.vars),]

#create boxplot for climate data

weights.clim.boxplot <- ggplot(weights.clim, aes(fill = type, x = Variable, y = value)) +
  geom_boxplot() + 
  scale_fill_manual(values = colors2) +
  ylab("Calculated weight") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 9, family = "sans", face = "bold"),
        axis.text = element_text(size = 8, family = "sans"),
        legend.background = element_rect(fill = "transparent"),
        legend.key.size = unit(0.7, 'cm'),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.text = element_text(size=8, family = "sans"),
        legend.title = element_text(size = 9, family = "sans", face = "bold"),
        legend.position = "none")

#create boxplot for bird data

weights.bird.boxplot <- ggplot(weights.bird, aes(fill = type, x = Variable, y = value)) +
  geom_boxplot() + 
  scale_fill_manual(values = colors2) +
  ylab("Calculated weight") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 9, family = "sans", face = "bold"),
        axis.text = element_text(size = 8, family = "sans"),
        legend.background = element_rect(fill = "transparent"),
        legend.key.size = unit(0.7, 'cm'),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.text = element_text(size=8, family = "sans"),
        legend.title = element_text(size = 9, family = "sans", face = "bold"),
        legend.position = "none")

#create boxplot for general data categories

weights.general.boxplot <- ggplot(weights.general, aes(fill = type, x = Variable, y = value)) +
  geom_boxplot() + 
  scale_fill_manual(values = colors2) +
  ylab("Calculated weight") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 9, family = "sans", face = "bold"),
        axis.text = element_text(size = 8, family = "sans"),
        legend.background = element_rect(fill = "transparent"),
        legend.key.size = unit(0.7, 'cm'),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.text = element_text(size=8, family = "sans"),
        legend.title = element_text(size = 9, family = "sans", face = "bold"),
        legend.position = "none")

sub <- ggarrange(weights.general.boxplot, weights.bird.boxplot, labels = c("a", "b"), widths = c(2, 1))

ggarrange(sub, weights.clim.boxplot, nrow = 2, labels = c("","c"), common.legend = TRUE)

ggsave("Figures/AHP.weights.png",
       width = 8,
       height = 7,
       dpi = 600)
#figures 4a - 7a - maps of top estuaries --------------------------------------

#subset top 5 estuaries by score

maintaining.top <- c(2030, 1025, 2022, 391, 1018)
maintaining.top.est <- estuaries[which(estuaries$EST_ID %in% maintaining.top),]
maintaining.top.est.centers <- st_centroid(maintaining.top.est)

improving.top <- c(2030, 391, 1025, 218, 1043)
improving.top.est <- estuaries[which(estuaries$EST_ID %in% improving.top),]
improving.top.est.centers <- st_centroid(improving.top.est)

shorebirds.top <- c(1025, 2022, 391, 218, 2023)
shorebirds.top.est <- estuaries[which(estuaries$EST_ID %in% shorebirds.top),]
shorebirds.top.est.centers <- st_centroid(shorebirds.top.est)

waterfowl.top <- c(2030, 1025, 391, 1043, 9)
waterfowl.top.est <- estuaries[which(estuaries$EST_ID %in% waterfowl.top),]
waterfowl.top.est.centers <- st_centroid(waterfowl.top.est)

estuaries.centers <- st_centroid(estuaries)

#create maps of priority estuaries
ggplot() +
  annotation_map_tile(type = "hotstyle", zoomin = 1) +
  geom_sf(data = estuaries.centers, alpha = 0) + #plot all estuaries invisibly to extent plot boundaries
  geom_sf(data = maintaining.top.est.centers, color = "#fb8500", fill = "#fb8500", size = 1, shape = 23) +
  theme_void() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.2, 0.1), 
        legend.title = element_blank(),
        legend.background = element_rect(color = "transparent", fill = "white"))

ggsave("Figures/AHP/maintaining.top5.estuaries.map.png",
       width = 3.05,
       height = 7.86,
       dpi = 600)

ggplot() +
  annotation_map_tile(type = "hotstyle", zoomin = 1) +
  geom_sf(data = estuaries.centers, alpha = 0) + #plot all estuaries invisibly to extent plot boundaries
  geom_sf(data = improving.top.est.centers, color = "#e63946", fill = "#e63946", size = 1, shape = 23) +
  theme_void() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.2, 0.1), 
        legend.title = element_blank(),
        legend.background = element_rect(color = "transparent", fill = "white"))

ggsave("Figures/AHP/improving.top5.estuaries.map.png",
       width = 3.05,
       height = 7.86,
       dpi = 600)

ggplot() +
  annotation_map_tile(type = "hotstyle", zoomin = 1) +
  geom_sf(data = estuaries.centers, alpha = 0) + #plot all estuaries invisibly to extent plot boundaries
  geom_sf(data = shorebirds.top.est.centers, fill = "#ffb703", color = "#ffb703", size = 1, shape = 23) +
  theme_void() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.2, 0.1), 
        legend.title = element_blank(),
        legend.background = element_rect(color = "transparent", fill = "white"))

ggsave("Figures/AHP/shorebirds.top5.estuaries.map.png",
       width = 3.05,
       height = 7.86,
       dpi = 600)

ggplot() +
  annotation_map_tile(type = "hotstyle", zoomin = 1) +
  geom_sf(data = estuaries.centers, alpha = 0) + #plot all estuaries invisibly to extent plot boundaries
  geom_sf(data = waterfowl.top.est.centers, fill = "#457b9d", color = "#457b9d", size = 1, shape = 23) +
  theme_void() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.2, 0.1), 
        legend.title = element_blank(),
        legend.background = element_rect(color = "transparent", fill = "white"))

ggsave("Figures/AHP/waterfowl.top5.estuaries.map.png",
       width = 3.05,
       height = 7.86,
       dpi = 600)

#figures 4b - 7b - density plot of scores ------------------------------------

ggplot(maintaining) +
  geom_density(aes(x = score), color = "#fb8500", fill = "#fb8500", alpha = 0.5) +
  scale_y_continuous(expand = expansion(mult = 0)) +
  scale_x_continuous(expand = expansion(mult = 0)) +
  xlab("Calculated estuary scores for 'Maintaining' model") +
  ylab("Density") +
  #xlim(0, 20) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_text(size = 9, family = "sans", face = "bold"),
        axis.title.y = element_text(size = 9, family = "sans", face = "bold"),
        axis.text = element_text(size = 8, family = "sans"))

ggsave("Figures/AHP/maintaining.density.png",
       width = 6,
       height = 5,
       dpi = 600)

ggplot(improving) +
  geom_density(aes(x = score), color = "#e63946", fill = "#e63946", alpha = 0.5) +
  scale_y_continuous(expand = expansion(mult = 0)) +
  scale_x_continuous(expand = expansion(mult = 0)) +
  xlab("Calculated estuary scores for 'Improving' model") +
  ylab("Density") +
  #xlim(0, 20) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_text(size = 9, family = "sans", face = "bold"),
        axis.title.y = element_text(size = 9, family = "sans", face = "bold"),
        axis.text = element_text(size = 8, family = "sans"))

ggsave("Figures/AHP/improving.density.png",
       width = 6,
       height = 5,
       dpi = 600)

ggplot(shorebirds) +
  geom_density(aes(x = score), color = "#ffb703", fill = "#ffb703", alpha = 0.5) +
  scale_y_continuous(expand = expansion(mult = 0)) +
  scale_x_continuous(expand = expansion(mult = 0)) +
  xlab("Calculated estuary scores for 'Shorebirds' model") +
  ylab("Density") +
  #xlim(0, 20) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_text(size = 9, family = "sans", face = "bold"),
        axis.title.y = element_text(size = 9, family = "sans", face = "bold"),
        axis.text = element_text(size = 8, family = "sans"))

ggsave("Figures/AHP/shorebirds.density.png",
       width = 6,
       height = 5,
       dpi = 600)

ggplot(waterfowl) +
  geom_density(aes(x = score), color = "#457b9d", fill = "#457b9d", alpha = 0.5) +
  scale_y_continuous(expand = expansion(mult = 0)) +
  scale_x_continuous(expand = expansion(mult = 0)) +
  xlab("Calculated estuary scores for 'Waterfowl' model") +
  ylab("Density") +
  #xlim(0, 20) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_text(size = 9, family = "sans", face = "bold"),
        axis.title.y = element_text(size = 9, family = "sans", face = "bold"),
        axis.text = element_text(size = 8, family = "sans"))

ggsave("Figures/AHP/waterfowl.density.png",
       width = 6,
       height = 5,
       dpi = 600)

#figures 4c - 7c - ridgeline plot of scores by region --------------------------

#add in regions to datasets

#subset all results by region
SWBC <- readRDS("Data/Shapefiles/Regions/southwestern_BC.rds")
NWVI <- readRDS("Data/Shapefiles/Regions/northwest_VI.rds")
NCM <- readRDS("Data/Shapefiles/Regions/northcentral_BC.rds")
HG <- readRDS("Data/Shapefiles/Regions/haida_gwaii.rds")
SS <- readRDS("Data/Shapefiles/Regions/salish_sea.rds")
WONC <- readRDS("Data/Shapefiles/Regions/WA_OR_northernCA.rds")
CC <- readRDS("Data/Shapefiles/Regions/central_CA.rds")

#add labels to plots
labels <- rev(c("North Central BC", "Haida Gwaii", "Northwest Vancouver Island", "Southwest BC", "Salish Sea", "WA, OR, Northern CA", "Central California"))

regions <- function(dataset) {
  
  results.SWBC <- dataset[dataset$EST_ID %in% SWBC$EST_NO,]
  results.NWVI <- dataset[dataset$EST_ID %in% NWVI$EST_NO,]
  results.NCM <- dataset[dataset$EST_ID %in% NCM$EST_NO,]
  results.HG <- dataset[dataset$EST_ID %in% HG$EST_NO,]
  results.SS <- dataset[dataset$EST_ID %in% SS$PMEP_EstuaryID,]
  results.WONC <- dataset[dataset$EST_ID %in% WONC$PMEP_EstuaryID,]
  results.CC <- dataset[dataset$EST_ID %in% CC$PMEP_EstuaryID,]
  
  #add in regions to results dataset
  results.SWBC$region <- "SWBC"
  results.NWVI$region <- "NWVI"
  results.NCM$region <- "NCM"
  results.HG$region <- "HG"
  results.SS$region <- "SS"
  results.WONC$region <- "WONC"
  results.CC$region <- "CC"
  
  #save all results in single file
  rbind(results.SWBC, results.NWVI, results.NCM, results.HG, results.SS, results.WONC, results.CC) 
  
}

maintaining <- regions(maintaining)
improving <- regions(improving)
shorebirds <- regions(shorebirds)
waterfowl <- regions(waterfowl)

ggplot(maintaining, aes(x = score, y = region, fill = score)) +
  geom_density_ridges(color = "black", fill = "#fb8500") +
  theme_ridges() + 
  scale_y_discrete(labels = labels, expand = expansion(mult = 0)) +
  scale_x_continuous(expand = expansion(mult = 0)) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_text(size = 12, family = "sans", face = "bold"),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 10, family = "sans"))

ggsave("Figures/AHP/maintaining.regions.scores.png",
       width = 7,
       height = 8,
       dpi = 600)

ggplot(improving, aes(x = score, y = region, fill = score)) +
  geom_density_ridges(color = "black", fill = "#e63946") +
  theme_ridges() + 
  scale_y_discrete(labels = labels, expand = expansion(mult = 0)) +
  scale_x_continuous(expand = expansion(mult = 0)) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_text(size = 12, family = "sans", face = "bold"),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 10, family = "sans"))

ggsave("Figures/AHP/improving.regions.scores.png",
       width = 7,
       height = 8,
       dpi = 600)

ggplot(shorebirds, aes(x = score, y = region, fill = score)) +
  geom_density_ridges(color = "black", fill = "#ffb703") +
  theme_ridges() + 
  scale_y_discrete(labels = labels, expand = expansion(mult = 0)) +
  scale_x_continuous(expand = expansion(mult = 0)) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_text(size = 12, family = "sans", face = "bold"),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 10, family = "sans"))

ggsave("Figures/AHP/shorebirds.regions.scores.png",
       width = 7,
       height = 8,
       dpi = 600)

ggplot(waterfowl, aes(x = score, y = region, fill = score)) +
  geom_density_ridges(color = "black", fill = "#457b9d") +
  theme_ridges() + 
  scale_y_discrete(labels = labels, expand = expansion(mult = 0)) +
  scale_x_continuous(expand = expansion(mult = 0)) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_text(size = 12, family = "sans", face = "bold"),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 10, family = "sans"))

ggsave("Figures/AHP/waterfowl.regions.scores.png",
       width = 7,
       height = 8,
       dpi = 600)

#figure 5 - boxplot of scores comparisons by region -----------------------------

#add in regions to datasets

#subset all results by region
SWBC <- readRDS("Data/Shapefiles/Regions/southwestern_BC.rds")
NWVI <- readRDS("Data/Shapefiles/Regions/northwest_VI.rds")
NCM <- readRDS("Data/Shapefiles/Regions/northcentral_BC.rds")
HG <- readRDS("Data/Shapefiles/Regions/haida_gwaii.rds")
SS <- readRDS("Data/Shapefiles/Regions/salish_sea.rds")
WONC <- readRDS("Data/Shapefiles/Regions/WA_OR_northernCA.rds")
CC <- readRDS("Data/Shapefiles/Regions/central_CA.rds")

#add labels to plots
labels <- rev(c("North Central BC", "Haida Gwaii", "Northwest Vancouver Island", "Southwest BC", "Salish Sea", "WA, OR, Northern CA", "Central California"))

regions <- function(dataset) {
  
  results.SWBC <- dataset[dataset$EST_ID %in% SWBC$EST_NO,]
  results.NWVI <- dataset[dataset$EST_ID %in% NWVI$EST_NO,]
  results.NCM <- dataset[dataset$EST_ID %in% NCM$EST_NO,]
  results.HG <- dataset[dataset$EST_ID %in% HG$EST_NO,]
  results.SS <- dataset[dataset$EST_ID %in% SS$PMEP_EstuaryID,]
  results.WONC <- dataset[dataset$EST_ID %in% WONC$PMEP_EstuaryID,]
  results.CC <- dataset[dataset$EST_ID %in% CC$PMEP_EstuaryID,]
  
  #add in regions to results dataset
  results.SWBC$region <- "SWBC"
  results.NWVI$region <- "NWVI"
  results.NCM$region <- "NCM"
  results.HG$region <- "HG"
  results.SS$region <- "SS"
  results.WONC$region <- "WONC"
  results.CC$region <- "CC"
  
  #save all results in single file
  rbind(results.SWBC, results.NWVI, results.NCM, results.HG, results.SS, results.WONC, results.CC) 
  
}

maintaining <- regions(maintaining)
improving <- regions(improving)
shorebirds <- regions(shorebirds)
waterfowl <- regions(waterfowl)

#bind together all datasets

maintaining$model <- "maintaining"
improving$model <- "improving"
shorebirds$model <- "shorebirds"
waterfowl$model <- "waterfowl"

all.models <- rbind(maintaining, improving, shorebirds, waterfowl)

#create boxplot of scores by region

models.boxplot <- function(region = "NCM", title = "North & Central BC") {
  
  region <- all.models[which(all.models$region == region),]
  
  ggplot(region, aes(x=model, y=score, fill=model)) +
    geom_boxplot() +
    ylim(c(0, 0.9)) +
    labs(title = title) +
    scale_fill_manual(values = colors4) +
    theme_bw() + 
    theme(legend.position="none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "transparent", color = NA),
          plot.background = element_rect(fill = "transparent", color = NA),
          plot.title = element_text(size = 12, family = "sans", face = "bold"),
          axis.text = element_text(size = 10, family = "sans"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
}

ncm.boxplot <- models.boxplot(region = "NCM", title = "North & Central BC")
hg.boxplot <- models.boxplot(region = "HG", title = "Haida Gwaii")
nwvi.boxplot <- models.boxplot(region = "NWVI", title = "Northwestern Vancouver Island")
swbc.boxplot <- models.boxplot(region = "SWBC", title = "Southwestern BC")
ss.boxplot <- models.boxplot(region = "SS", title = "Salish Sea, WA")
wonc.boxplot <- models.boxplot(region = "WONC", title = "WA, OR, & Northern CA Coast")
cc.boxplot <- models.boxplot(region = "CC", title = "Central California")

#plot scores in all regions 

all.boxplot <- ggplot(all.models, aes(x=model, y=score, fill=model)) +
  geom_boxplot() +
  ylim(c(0, 0.9)) +
  labs(title = "Study Area Overall") +
  scale_fill_manual(values = colors4) +
  theme_bw() + 
  theme(legend.position="none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(size = 12, family = "sans", face = "bold"),
        axis.text = element_text(size = 10, family = "sans"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

#arrange all plots together

ggarrange(all.boxplot, ncm.boxplot, hg.boxplot, nwvi.boxplot, swbc.boxplot,
          ss.boxplot, wonc.boxplot, cc.boxplot, ncol = 2, nrow = 4, 
          labels = c("a", "b", "c", "d", "e", "f", "g", "h"))

ggsave("Figures/model.region.results.png",
       width = 7.8,
       height = 9,
       dpi = 600)
