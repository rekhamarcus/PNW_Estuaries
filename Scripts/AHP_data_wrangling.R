#wrangling all data for use by AHP

library("dplyr")
library('terra')
library('sf') #need to be able to join sf and dataframes

setwd("C:/Users/rekha/OneDrive - University of Victoria/Wetlands")

#load in all data -------------------------------------------------------------

#climate data
climate <- read.csv("Data/Results/r2041.csv")
ssp1 <- climate[which(climate$SSP == "SSP1_2.6"),]
ssp5 <- climate[which(climate$SSP == "SSP5_8.5"),]

#bird data
birds <- read.csv("Data/Bird_data/PBHJV_bird_estuary_data.csv")

#slr data
slr <- readRDS("SLR/Results/estuary.slr.risk.rds")

#bind all data together by estuary into 4 categories ---------------------------------------------

# 1 - maintaining - climate min SSP 5, all birds, slr SSP 5

maintaining.clim <- ssp5 %>% group_by(EST_ID) %>% summarize(mean.pr = min(mean.pr),
                                                            SD.pr = min(SD.pr),
                                                            ES0.99.pr = min(ES0.99.pr),
                                                            mean.tas = min(mean.tas),
                                                            SD.tas = min(SD.tas),
                                                            ES0.99.tas = min(ES0.99.tas))

#take absolute value of climate variables to evaluate estuaries on total change, not direction
maintaining.clim$mean.pr.abs <- abs(maintaining.clim$mean.pr)
maintaining.clim$SD.pr.abs <- abs(maintaining.clim$SD.pr)
maintaining.clim$ES0.99.pr.abs <- abs(maintaining.clim$ES0.99.pr)
maintaining.clim$mean.tas.abs <- abs(maintaining.clim$mean.tas)
maintaining.clim$SD.tas.abs <- abs(maintaining.clim$SD.tas)
maintaining.clim$ES0.99.tas.abs <- abs(maintaining.clim$ES0.99.tas)

maintaining <- full_join(maintaining.clim, birds)
maintaining <- full_join(maintaining, slr)
maintaining <- maintaining[,c(1:15,26,27)]

# 2 - improving - climate max SSP 1, all birds, max SSP 1

improving.clim <- ssp1 %>% group_by(EST_ID) %>% summarize(mean.pr = max(mean.pr),
                                                            SD.pr = max(SD.pr),
                                                            ES0.99.pr = max(ES0.99.pr),
                                                            mean.tas = max(mean.tas),
                                                            SD.tas = max(SD.tas),
                                                            ES0.99.tas = max(ES0.99.tas))

#take absolute value of climate variables to evaluate estuaries on total change, not direction
improving.clim$mean.pr.abs <- abs(improving.clim$mean.pr)
improving.clim$SD.pr.abs <- abs(improving.clim$SD.pr)
improving.clim$ES0.99.pr.abs <- abs(improving.clim$ES0.99.pr)
improving.clim$mean.tas.abs <- abs(improving.clim$mean.tas)
improving.clim$SD.tas.abs <- abs(improving.clim$SD.tas)
improving.clim$ES0.99.tas.abs <- abs(improving.clim$ES0.99.tas)

improving <- full_join(improving.clim, birds)
improving <- full_join(improving, slr)
improving <- improving[,c(1:15,25,27)]

# 3 - shorebirds - climate median, shorebirds, slr median

median.clim <- climate %>% group_by(EST_ID) %>% summarize(mean.pr = median(mean.pr),
                                                          SD.pr = median(SD.pr),
                                                          ES0.99.pr = median(ES0.99.pr),
                                                          mean.tas = median(mean.tas),
                                                          SD.tas = median(SD.tas),
                                                          ES0.99.tas = median(ES0.99.tas))

#take absolute value of climate variables to evaluate estuaries on total change, not direction
median.clim$mean.pr.abs <- abs(median.clim$mean.pr)
median.clim$SD.pr.abs <- abs(median.clim$SD.pr)
median.clim$ES0.99.pr.abs <- abs(median.clim$ES0.99.pr)
median.clim$mean.tas.abs <- abs(median.clim$mean.tas)
median.clim$SD.tas.abs <- abs(median.clim$SD.tas)
median.clim$ES0.99.tas.abs <- abs(median.clim$ES0.99.tas)

slr$meanrisk <- (slr$risk1 + slr$risk5)/2

shorebirds <- full_join(median.clim, birds)
shorebirds <- full_join(shorebirds, slr)
shorebirds <- shorebirds[,c(1:13,16:17,29,27)]

# 4 - waterfowl - climate median, waterfow, slr median

waterfowl <- full_join(median.clim, birds)
waterfowl <- full_join(waterfowl, slr)
waterfowl <- waterfowl[,c(1:13,18:19,29,27)]

# normalize all data ---------------------------------------

#range normalizing to ensure all values fall between 0 and 1

for(i in 8:17){
  
  maintaining[,i+10] <-  
           (maintaining[,i] - min(maintaining[,i], na.rm = TRUE))/ 
           (max(maintaining[,i], na.rm = TRUE) - min(maintaining[,i], na.rm = TRUE))
  
  names(maintaining)[i+10] <- paste(names(maintaining)[i], ".norm", sep = "")
  
}

write.csv(maintaining, "Ranking/AHP/estuary_assessment_maintaining_data.csv")

for(i in 8:17){
  
  improving[,i+10] <-  
    (improving[,i] - min(improving[,i], na.rm = TRUE))/ 
    (max(improving[,i], na.rm = TRUE) - min(improving[,i], na.rm = TRUE))
  
  names(improving)[i+10] <- paste(names(improving)[i], ".norm", sep = "")
  
}

write.csv(improving, "Ranking/AHP/estuary_assessment_improving_data.csv")

for(i in 8:17){
  
  shorebirds[,i+10] <-  
    (shorebirds[,i] - min(shorebirds[,i], na.rm = TRUE))/ 
    (max(shorebirds[,i], na.rm = TRUE) - min(shorebirds[,i], na.rm = TRUE))
  
  names(shorebirds)[i+10] <- paste(names(shorebirds)[i], ".norm", sep = "")
  
}

write.csv(shorebirds, "Ranking/AHP/estuary_assessment_shorebird_data.csv")

for(i in 8:17){
  
  waterfowl[,i+10] <-  
    (waterfowl[,i] - min(waterfowl[,i], na.rm = TRUE))/ 
    (max(waterfowl[,i], na.rm = TRUE) - min(waterfowl[,i], na.rm = TRUE))
  
  names(waterfowl)[i+10] <- paste(names(waterfowl)[i], ".norm", sep = "")
  
}

write.csv(waterfowl, "Ranking/AHP/estuary_assessment_waterfowl_data.csv")



