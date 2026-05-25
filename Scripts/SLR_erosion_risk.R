#SLR risk score for estuaries

library('dplyr')
library('terra')
library('sf')
library('ggplot2')
library('tidyterra')
library('ggspatial')
library('gdalUtilities')
library('prettymapr')

setwd("C:/Users/rekha/OneDrive - University of Victoria/Wetlands")

#load in datasets --------------------------------------------------------------

estuaries <- readRDS("Data/Shapefiles/estuaries.rds")

slr1 <- rast("SLR/SLR_data/raster_slr_126_2050_q0.5.nc")
slr5 <- rast("SLR/SLR_data/raster_slr_585_2050_q0.5.nc")

bottoms <- readRDS("SLR/Results/estuary_bottom_types.rds")

#fix column names for esi/bc bottom types with the same names
names(bottoms) <- c("EST_ID", "Area_HA", "ncells" , "8A", "8D", "6B", "5", "9A", "3A", "10A",
                    "2A", "8B", "8C", "1A", "1C", "4", "7", "6A", "10B", "6C", "9B", "1B", "U",
                    "10C", "10D", "1", "2", "3","4.1", "5.1", "6", "7.1", "8", "9", "10", "11", 
                    "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24",
                    "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36")

#calculate vulnerability to erosion score --------------------------------------

#create groups of vulnerability levels for bottom types - for more information on these
#groupings, see ESI_coast_classes_crosswalk.xlsx, Erosion_risk_coastal_classes_Singh2020.docx,
#and SLR_erosion_risk_index.xlsx
v1 <- c("1A", "1C", "8A", "8B", "1B", "1", "4.1", "2", "5.1", "33", "3")
v2 <- c("2A", "8C", "32")
v3 <- c("8D", "9B", "0", "8")
v4 <- c("6B", "6C", "6A", "10A", "10B", "10C", "10D", "6", "9", "7.1", "10", "22", "31", "21", "23", "13", "22", "34", "35")
v5 <- c("3A", "4", "5", "7", "9A", "16", "19", "17", "20", "11", "14", "29", "28", "24", "26", "18", "27", "30", "12", "15", "25")

#sum vulnerability * proportion of area covered by bottom type

risk <- list()

for(i in 1:nrow(estuaries)){
  
  s1 <- crop(slr1, estuaries[i,]) %>% mask(estuaries[i,])
  crs(s1) <- "EPSG:4269"
  
  s5 <- crop(slr5, estuaries[i,]) %>% mask(estuaries[i,])
  crs(s5) <- "EPSG:4269"
  
  eb <- bottoms[bottoms$EST_ID == estuaries$EST_ID[i],]
  
  e1 <- 1*(sum(which(!is.na(eb[,which(names(eb) %in% v1)])))/eb$ncells)
  e2 <- 2*(sum(which(!is.na(eb[,which(names(eb) %in% v2)])))/eb$ncells)
  e3 <- 3*(sum(which(!is.na(eb[,which(names(eb) %in% v3)])))/eb$ncells)
  e4 <- 4*(sum(which(!is.na(eb[,which(names(eb) %in% v4)])))/eb$ncells)
  e5 <- 5*(sum(which(!is.na(eb[,which(names(eb) %in% v5)])))/eb$ncells)
  
  #extract how much slr is expected to rise for this estuary
  r <- data.frame(EST_ID = estuaries$EST_ID[i],
                  slr.ssp1 = median(values(s1)),
                  slr.ssp5 = median(values(s5)),
                  vulnerability = sum(e1, e2, e3, e4, e5))
  
  risk[[i]] <- r
  
}

RISK <- do.call(rbind, risk)

#standardize SLR values for ssp1 and ssp5

RISK$ssl.1 <- case_when(sign(RISK$slr.ssp1) == 1 ~ RISK$slr.ssp1/254.5835, #scale between 0 and 1
                        sign(RISK$slr.ssp1) == -1 ~ 0, #remove negative values
                        sign(RISK$slr.ssp1) == 0 ~ 0) #remove neutral values

RISK$ssl.5 <- case_when(sign(RISK$slr.ssp5) == 1 ~ RISK$slr.ssp5/287.295,
                        sign(RISK$slr.ssp5) == -1 ~ 0,
                        sign(RISK$slr.ssp5) == 0 ~ 0)

#calculate overall risk score 

RISK$risk1 <- RISK$ssl.1*RISK$vulnerability
RISK$risk5 <- RISK$ssl.5*RISK$vulnerability

#bind to estuary shapefiles
RISK.estuaries <- full_join(RISK, estuaries) %>% st_as_sf()

saveRDS(RISK.estuaries, "SLR/Results/estuary.slr.risk.rds")

#plot results ------------------------------------------------------------------

RISK.centroids <- st_centroid(RISK.estuaries)

#risk of erosion from sea level rise, SSP1

ggplot(RISK.centroids, aes(fill = as.numeric(log(risk1)))) + 
  annotation_map_tile(type = "hotstyle", zoomin = 1) + 
  geom_sf(aes(colour = as.numeric(log(risk1))), shape = 23, size = 0.5) + 
  labs(title = "Risk of erosion due to sea level rise, SSP 1") + 
  scale_fill_gradient(low = "#457b9d", high = "#e63946", name = "log Erosion Risk") +
  scale_colour_gradient(low = "#457b9d", high = "#e63946", name = "log Erosion Risk") +
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
        legend.position = "inside", 
        legend.position.inside = c(0.2, 0.2))

ggsave("Figures/SLR/ssp1_risk.png",
       width = 3.05,
       height = 7.86,
       dpi = 600)

#risk of erosion from sea level rise, SSP5

ggplot(RISK.centroids, aes(fill = as.numeric(log(risk5)))) + 
  annotation_map_tile(type = "hotstyle", zoomin = 1) + 
  geom_sf(aes(colour = as.numeric(log(risk5))), shape = 23, size = 0.5) + 
  labs(title = "Risk of erosion due to sea level rise, SSP 5") + 
  scale_fill_gradient(low = "#457b9d", high = "#e63946", name = "log Erosion Risk") +
  scale_colour_gradient(low = "#457b9d", high = "#e63946", name = "log Erosion Risk") +
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
        legend.position = "inside", 
        legend.position.inside = c(0.2, 0.2))

ggsave("Figures/SLR/ssp5_risk.png",
       width = 3.05,
       height = 7.86,
       dpi = 600)

