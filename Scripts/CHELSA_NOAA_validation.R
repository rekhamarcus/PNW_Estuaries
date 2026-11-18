##'File name: CHELSA_ensemble_results_figures
##'Author: Rekha Marcus
##'Thesis: Chapter 1
##'
##'This script contains the code used to compare CHELSA historical modelled data
##'to NOAA historical observed value data on order to validate the calculation
##'of expected shortfall in Chapter 1. This script also includes code for the associated
##'figures in Chapter 1: Figure 4, and supplements S3 and S4.

#load packages ---------------------------------------------------------------

library('dplyr')
library('terra') #for working with spatial data
library('sf') #for working with spatial data
library('extRemes') #for modelling extreme events
library('data.table') #for matrix.futures function
library('tidyr') #for pivot_longer function
library('ggplot2')
library('ggpubr')
library('ggspatial')
library('multcomp') #for stats testing
library('boot') #for bootstrap stats testing

#load in functions for analysis
source("PNW_Estuaries/Functions/crop.estuary.R") #function to crop data files to a single estuary

colors <- c("#457b9d", "#219ebc", "#ffb703", "#fb8500")

#load in data ----------------------------------------------------------------

estuaries <- readRDS("Data/Shapefiles/estuaries.rds")
estuaries <- st_transform(estuaries, crs = st_crs("EPSG:4326"))

#load in observed data and crop to study area
tas.obs <- rast('Data/Temperature/air.mon.mean.nc') %>% rotate()
tas.obs <- crop(tas.obs, pnw)
tas.obs <- mask(tas.obs, pnw)
saveRDS(tas.obs, 'Data/Temperature/NOAA_monthly_timeseries_historical/PNW_air_mon_mean_GHCN_CAMS.rds')

pr.obs <- rast("Data/Precipitation/precip.mon.total.v401.nc") %>% rotate()
pr.obs <- crop(pr.obs, pnw)
pr.obs <- mask(pr.obs, pnw)
saveRDS(pr.obs, 'Data/Precipitation/NOAA_monthly_timeseries_historical/PNW_precip_mon_total_v401.rds')

#load in modelled data
tas <- list.files('Data/Temperature/CHELSA_monthly_timeseries_historical', pattern = ".tif", full.names = T)
pr <- list.files('Data/Precipitation/CHELSA_monthly_timeseries_historical', pattern = ".tif", full.names = T)

tas.obs <- readRDS('Data/Temperature/NOAA_monthly_timeseries_historical/PNW_air_mon_mean_GHCN_CAMS.rds')
pr.obs <- readRDS('Data/Precipitation/NOAA_monthly_timeseries_historical/PNW_precip_mon_total_v401.rds')

#run backcast comparison ------------------------------------------------------

compare.backcast <- list()

for(i in 1:nrow(estuaries)){
  
  #crop data to single estuary

  tas.estuary <- crop.estuary(files = tas, shape = estuaries[i,]) #historical modelled data
  
  possibleError <- tryCatch(
    tas.obs.estuary <- crop(tas.obs, estuaries[i,]),
    error=function(e) e)
  
  if(inherits(possibleError, "error")) next
  
  tas.obs.estuary <- mask(tas.obs.estuary, estuaries [i,]) #historical observed data
  
  pr.estuary <- crop.estuary(files = pr, shape = estuaries[i,])
  pr.obs.estuary <- crop(pr.obs, estuaries[i,])
  pr.obs.estuary <- mask(pr.obs.estuary, estuaries [i,])
  
  #extract block maxima as well as means from modelled historical data - will print 'Joining with `by = join_by(year)`'
  
  #extract max/mean of temperatures
  t <- as.data.frame(minmax(tas.estuary))
  maxt <- t[2,]
  
  x <- pivot_longer(maxt, cols = c(1:491))
  x$year <- substr(x$name,
                   start = nchar(x$name) - nchar('yyyy_V.2.1') + 1,
                   stop = nchar(x$name) - nchar('_V.2.1'))
  
  tas.max <- group_by(x, year) %>%
    summarise(tasmax = (max(value)/10) - 273.15) #convert units from K*10 to C
  
  a <- global(tas.estuary, 'mean', na.rm=TRUE)
  a$name <- rownames(a)
  a$year <- substr(a$name,
                   start = nchar(a$name) - nchar('yyyy_V.2.1') + 1,
                   stop = nchar(a$name) - nchar('_V.2.1'))
  
  tas.mean <- group_by(a, year) %>%
    summarise(tasmean = (mean(mean)/10) - 273.15) #convert units from K*10 to C
  
  c <- global(tas.estuary, 'sd', na.rm=TRUE)
  c$name <- rownames(c)
  c$year <- substr(c$name,
                   start = nchar(c$name) - nchar('yyyy_V.2.1') + 1,
                   stop = nchar(c$name) - nchar('_V.2.1'))
  
  #extract  max/mean of precipitation
  
  p <- as.data.frame(minmax(pr.estuary))
  maxp <- p[2,]
  
  z <- pivot_longer(maxp, cols = c(1:486))
  z$year <- substr(z$name,
                   start = nchar(z$name) - nchar('yyyy_V.2.1') + 1,
                   stop = nchar(z$name) - nchar('_V.2.1'))
  
  pr.max <- group_by(z, year) %>%
    summarise(prmax = max(value)/100)
  
  b <- global(pr.estuary, 'mean', na.rm=TRUE)
  b$name <- rownames(b)
  b$year <- substr(b$name,
                   start = nchar(b$name) - nchar('yyyy_V.2.1') + 1,
                   stop = nchar(b$name) - nchar('_V.2.1'))
  
  pr.mean <- group_by(b, year) %>%
    summarise(prmean = mean(mean)/100) #convert units to mm
  
  #join all data together
  max.mean.modelled <- right_join(tas.mean, tas.max)
  max.mean.modelled <- right_join(max.mean.modelled, pr.max)
  max.mean.modelled <- right_join(max.mean.modelled, pr.mean)
  
  #extract means of observed historical data - will print 'Joining with `by = join_by(year)`'
  
  names(tas.obs.estuary) <- time(tas.obs.estuary)
  d <- global(tas.obs.estuary, 'mean', na.rm=TRUE)
  d$date <- rownames(d)
  d$year <- year(d$date)
  
  tas.mean.observed <- group_by(d, year) %>%
    summarise(tasobsmean = mean(mean)  - 273.15) #convert from K to C
  tas.mean.observed <- tas.mean.observed[tas.mean.observed$year > 1980, ] #make sure to compare to modelled data time ranges
  
  names(pr.obs.estuary) <- time(pr.obs.estuary)
  e <- global(pr.obs.estuary, 'mean', na.rm=TRUE)
  e$date <- rownames(e)
  e$year <- year(e$date)
  
  pr.mean.observed <- group_by(e, year) %>%
    summarise(probsmean = mean(mean)*10) #convert from cm to mm 
  pr.mean.observed <- pr.mean.observed[pr.mean.observed$year > 1980, ] #make sure to compare to modelled data time ranges
  
 #construct df to hold results
  
  compare <- data.frame(EST_ID = estuaries$EST_ID[i],
                        EStas.modelled.sum = NA,
                        EStas.modelled = NA,
                        EStas.observed.sum = NA,
                        EStas.observed = NA,
                        ESpr.modelled.sum = NA,
                        ESpr.modelled = NA,
                        ESpr.observed.sum = NA,
                        ESpr.observed = NA,
                        tas.shape = NA,
                        tas.modelled.shape = NA,
                        tas.observed.shape = NA,
                        pr.shape = NA,
                        pr.modelled.shape = NA,
                        pr.observed.shape = NA)
  
  #compare modelled vs observed, raw data vs summary stats results if available
  
  if(!is.na(mean(max.mean.modelled$tasmean))){
    
    #backcast temperature data
    
    #construct 2 models: modelled maxima data, modelled mean data
    
    model <- fevd(max.mean.modelled$tasmax, data = max.mean.modelled, type = "GEV", method = "GMLE", time.units = "years")
    model.modelled <- fevd(max.mean.modelled$tasmean, data = max.mean.modelled, type = "GEV", method = "GMLE", time.units = "years")
    
    #calculate expected shortfall of the 99th percentile for modelled and observed summary and raw data
    
    p <- seq(0.99, 0.9999, length.out = 1000)
    EStas.modelled.sum <- qevd(p, loc = mean(max.mean.modelled$tasmean), scale = sd(max.mean.modelled$tasmean), shape = model$results$par[3])
    EStas.modelled <- qevd(p, loc = model.modelled$results$par[1], scale = model.modelled$results$par[2], shape = model.modelled$results$par[3])
    
    compare$EStas.modelled.sum <- sum(p*EStas.modelled.sum)/1000
    compare$EStas.modelled <- sum(p*EStas.modelled)/1000
    
    compare$tas.shape <- model$results$par[3]
    compare$tas.modelled.shape <- model.modelled$results$par[3]
    
    #backcast precipitation data
    
    model <- fevd(max.mean.modelled$prmax, data = max.mean.modelled, type = "GEV", method = "GMLE", time.units = "years")
    model.modelled <- fevd(max.mean.modelled$prmean, data = max.mean.modelled, type = "GEV", method = "GMLE", time.units = "years")
    
    p <- seq(0.99, 0.9999, length.out = 1000)
    ESpr.modelled.sum <- qevd(p, loc = mean(max.mean.modelled$prmean), scale = sd(max.mean.modelled$prmean), shape = model$results$par[3])
    ESpr.modelled <- qevd(p, loc = model.modelled$results$par[1], scale = model.modelled$results$par[2], shape = model$results$par[3])
    
    compare$ESpr.modelled.sum <- sum(p*ESpr.modelled.sum)/1000
    compare$ESpr.modelled <- sum(p*ESpr.modelled)/1000
    
    compare$pr.shape <- model$results$par[3]
    compare$pr.modelled.shape <- model.modelled$results$par[3]
    
  }
  
  #model observed data if available for estuary
  
  if(!is.na(mean(tas.mean.observed$tasobsmean))) {
    
    model.observed <- fevd(tas.mean.observed$tasobsmean, data = tas.mean.observed, type = "GEV", method = "GMLE", time.units = "years")
    
    compare$tas.observed.shape <- model.observed$results$par[3]
    
    EStas.observed.sum <- qevd(p, loc = mean(tas.mean.observed$tasobsmean), scale = sd(tas.mean.observed$tasobsmean), shape = model$results$par[3])
    EStas.observed <- qevd(p, loc = model.observed$results$par[1], scale = model.observed$results$par[2], shape = model.observed$results$par[3])
    
    compare$EStas.observed.sum <- sum(p*EStas.observed.sum)/1000
    compare$EStas.observed <- sum(p*EStas.observed)/1000
    
  }
  
  if(!is.na(mean(pr.mean.observed$probsmean))) {
    
    model.observed <- fevd(pr.mean.observed$probsmean, data = pr.mean.observed, type = "GEV", method = "GMLE", time.units = "years")
    
    compare$pr.observed.shape <- model.observed$results$par[3]
    
    ESpr.observed.sum <- qevd(p, loc = mean(pr.mean.observed$probsmean), scale = sd(pr.mean.observed$probsmean), shape = model$results$par[3])
    ESpr.observed <- qevd(p, loc = model.observed$results$par[1], scale = model.observed$results$par[2], shape = model.observed$results$par[3])
    
    compare$ESpr.observed.sum <- sum(p*ESpr.observed.sum)/1000
    compare$ESpr.observed <- sum(p*ESpr.observed)/1000
    
  }
  
  #save resulting dataframe
  
  compare.backcast[[i]] <- compare
  
  print (i)
  
}

COMPARE <- do.call(rbind, compare.backcast)

saveRDS(COMPARE, "Data/Results/validation.compare.GHCN_CAMSs.rds")

COMPARE <- readRDS("Data/Results/validation.compare.GHCN_CAMSs.rds")

#Figure 4 - backcast comparison -----------------------------------------------

#plot residuals of each approach

COMPARE$tas.resid <- COMPARE$EStas.modelled - COMPARE$EStas.observed
COMPARE$tas.resid.sum <- COMPARE$EStas.modelled.sum - COMPARE$EStas.observed.sum

COMPARE$pr.resid <- COMPARE$ESpr.modelled - COMPARE$ESpr.observed
COMPARE$pr.resid.sum <- COMPARE$ESpr.modelled.sum - COMPARE$ESpr.observed.sum

resid.tas <- ggplot(COMPARE) +
  geom_histogram(aes(x = tas.resid, fill = "Residuals (Raw values)"), color = "#457b9d", alpha = 0.6) +
  geom_histogram(aes(x = tas.resid.sum, fill = "Residuals (summary stats)"), color = "#e63946", alpha = 0.6) +
  scale_y_continuous(expand = expansion(mult = 0)) +
  #scale_x_continuous(expand = expansion(mult = 0)) +
  scale_fill_manual(values = c("#457b9d", "#e63946")) +
  xlab("Temperature Residuals (C)") +
  ylab("Frequency") +
  theme_bw() + 
  theme(legend.position = "inside",
        legend.position.inside = c(0.75, 0.8),
        axis.title.x = element_text(size = 15, family = "sans", face = "bold"),
        axis.title.y = element_text(size = 15, family = "sans", face = "bold"),
        legend.text = element_text(size=10, family = "sans", face = "bold"),
        axis.text = element_text(size = 12, family = "sans"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank())

ggsave('Figures/Results/temp.validation.residuals.png',
       width = 6,
       height = 6,
       dpi = 600)

resid.pr <- ggplot(COMPARE) +
  geom_histogram(aes(x = pr.resid, fill = "Residuals (Raw values)"), color = "#457b9d", alpha = 0.6) +
  geom_histogram(aes(x = pr.resid.sum, fill = "Residuals (summary stats)"), color = "#e63946", alpha = 0.6) +
  scale_y_continuous(expand = expansion(mult = 0)) +
  #scale_x_continuous(expand = expansion(mult = 0)) +
  scale_fill_manual(values = c("#457b9d", "#e63946")) +
  xlab("Precipitation Residuals (mm)") +
  ylab("Frequency") +
  xlim(-500, 500) +
  theme_bw() + 
  theme(legend.position = "inside",
        legend.position.inside = c(0.25, 0.8),
        axis.title.x = element_text(size = 15, family = "sans", face = "bold"),
        axis.title.y = element_text(size = 15, family = "sans", face = "bold"),
        legend.text = element_text(size=10, family = "sans", face = "bold"),
        axis.text = element_text(size = 12, family = "sans"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank())

#plot ES results as box plots

es.tas <- COMPARE[,c(2:5)]

tas.m <- pivot_longer(es.tas, cols = c(1:4))

variables <- c("Modelled\nData", "Modelled\nSummary Stats", "Observed\nData", "Observed\nSummary Stats")

estas.boxplot <- ggplot(tas.m, aes(fill = name, x = name, y = value)) +
  geom_boxplot() + 
  stat_summary(fun = mean, color = "red") +
  stat_summary(fun.min = function(x) mean(x) - sd(x), fun.max = function(x) mean(x) + sd(x), 
    geom = "errorbar", color = "red", width = .3) +
  scale_fill_manual(values = colors) +
  xlab("") +
  ylab("Expected Shortfall of \nTemperature (C)") + 
  scale_x_discrete(labels = variables) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_text(size = 15, family = "sans", face = "bold"),
        axis.title.y = element_text(size = 15, family = "sans", face = "bold"),
        axis.text = element_text(size = 12, family = "sans"),
        legend.position = "none")

es.pr <- COMPARE[,c(6:9)]

pr.m <- pivot_longer(es.pr, cols = c(1:4))

espr.boxplot <- ggplot(pr.m, aes(fill = name, x = name, y = value)) +
  geom_boxplot() + stat_summary(fun = mean, color = "red") +
  stat_summary(fun.min = function(x) mean(x) - sd(x), fun.max = function(x) mean(x) + sd(x), 
               geom = "errorbar", color = "red", width = .3) +
  scale_fill_manual(values = colors) +
  xlab("") +
  ylab("Expected Shortfall of \nPrecipitation (mm) ") + 
  scale_x_discrete(labels = variables) + 
  ylim(-100, 800) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_text(size = 15, family = "sans", face = "bold"),
        axis.title.y = element_text(size = 15, family = "sans", face = "bold"),
        axis.text = element_text(size = 12, family = "sans"),
        legend.position = "none")

ggarrange(estas.boxplot, espr.boxplot, resid.tas, resid.pr, ncol = 2, nrow = 2, labels = c("a", "b", "c", "d"))

ggsave('Figures/Results/validation.boxplot.residuals.png',
       width = 11.7,
       height = 7.8,
       dpi = 600)

#Figure S3 - results where precipitation values have high residuals -----------

#something weird about precip raw data, observed way higher than expected by orders of magnitude
#find which regions these estuaries belong to, map them?

weirdprecip <- COMPARE[which(COMPARE$pr.resid < -500),]
weirdprecip <- weirdprecip$EST_ID

weirdest <- estuaries[estuaries$EST_ID %in% weirdprecip,]
weirdest.cent <-st_centroid(weirdest)

ggplot() + 
  annotation_map_tile(type = "hotstyle", zoomin = 1) + 
  geom_sf(data = weirdest.cent, fill = "white", shape = 23, size = 4) +
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
        plot.background = element_rect(fill = "transparent", color = NA))

ggsave("Figures/Results/S1_weird_estuaries.png",
       width = 5.3,
       height = 7.6,
       dpi = 600)


#summary stats --------------------------------------------------

#calculate mean of each category
mean(COMPARE$EStas.modelled.sum, na.rm = TRUE) #12.4366
mean(COMPARE$EStas.modelled, na.rm = TRUE) #11.89321
mean(COMPARE$EStas.observed.sum, na.rm = TRUE) #10.6788
mean(COMPARE$EStas.observed, na.rm = TRUE) #10.19623

mean(COMPARE$ESpr.modelled.sum, na.rm = TRUE) #325.692
mean(COMPARE$ESpr.modelled, na.rm = TRUE) #293.6604
mean(COMPARE$ESpr.observed.sum, na.rm = TRUE) #287.2993
mean(COMPARE$ESpr.observed, na.rm = TRUE) #451.4674

#calculate standard deviation of each category
sd(COMPARE$EStas.modelled.sum, na.rm = TRUE) # 1.177184
sd(COMPARE$EStas.modelled, na.rm = TRUE) #1.320388
sd(COMPARE$EStas.observed.sum, na.rm = TRUE) #2.291166
sd(COMPARE$EStas.observed, na.rm = TRUE) #2.218278

sd(COMPARE$ESpr.modelled.sum, na.rm = TRUE) #142.485
sd(COMPARE$ESpr.modelled, na.rm = TRUE) #136.2467
sd(COMPARE$ESpr.observed.sum, na.rm = TRUE) #111.4509
sd(COMPARE$ESpr.observed, na.rm = TRUE) #1101.349

#calculate standard error of each category
sd(COMPARE$EStas.modelled.sum, na.rm = TRUE)/sqrt(length(COMPARE$EStas.modelled.sum)) #0.04304211
sd(COMPARE$EStas.modelled, na.rm = TRUE)/sqrt(length(COMPARE$EStas.modelled)) #0.04827815
sd(COMPARE$EStas.observed.sum, na.rm = TRUE)/sqrt(length(COMPARE$EStas.observed.sum)) #0.08377333
sd(COMPARE$EStas.observed, na.rm = TRUE)/sqrt(length(COMPARE$EStas.observed)) #0.08110828

sd(COMPARE$ESpr.modelled.sum, na.rm = TRUE)/sqrt(length(COMPARE$ESpr.modelled.sum)) #5.209766
sd(COMPARE$ESpr.modelled, na.rm = TRUE)/sqrt(length(COMPARE$ESpr.modelled)) #4.981672
sd(COMPARE$ESpr.observed.sum, na.rm = TRUE)/sqrt(length(COMPARE$ESpr.observed.sum)) #4.07505
sd(COMPARE$ESpr.observed, na.rm = TRUE)/sqrt(length(COMPARE$ESpr.observed)) #40.26

#calculate mean and sd for residuals

mean(COMPARE$tas.resid, na.rm = TRUE) #1.6969
sd(COMPARE$tas.resid, na.rm = TRUE) #1.7835

mean(COMPARE$tas.resid.sum, na.rm = TRUE) #1.7577
sd(COMPARE$tas.resid.sum, na.rm = TRUE) #1.8275

mean(COMPARE$pr.resid, na.rm = TRUE) #-142.4132
sd(COMPARE$pr.resid, na.rm = TRUE) #1098.551

mean(COMPARE$pr.resid.sum, na.rm = TRUE) #54.6959
sd(COMPARE$pr.resid.sum, na.rm = TRUE) #65.1269

#supplementary information 3 (weird precipitation estuaries) -------------------

#something weird about precip raw data, observed way higher than expected by orders of magnitude
#find which regions these estuaries belong to, map them?

weirdprecip <- COMPARE[which(COMPARE$pr.resid < -500),]
weirdprecip <- weirdprecip$EST_ID

weirdest <- estuaries[estuaries$EST_ID %in% weirdprecip,]
weirdest.cent <-st_centroid(weirdest)

weirdest.map <- ggplot() + 
  annotation_map_tile(type = "hotstyle", zoomin = 1) + 
  geom_sf(data = weirdest.cent, fill = "white", shape = 23, size = 4) +
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
        plot.background = element_rect(fill = "transparent", color = NA))

weird.resid.pr <- ggplot(COMPARE) +
  geom_histogram(aes(x = pr.resid, fill = "Residuals (Raw values)"), color = "#457b9d", alpha = 0.6) +
  geom_histogram(aes(x = pr.resid.sum, fill = "Residuals (summary stats)"), color = "#e63946", alpha = 0.6) +
  scale_y_continuous(expand = expansion(mult = 0)) +
  #scale_x_continuous(expand = expansion(mult = 0)) +
  scale_fill_manual(values = c("#457b9d", "#e63946")) +
  xlab("Precipitation Residuals") +
  ylab("Frequency") +
  #xlim(-500, 500) +
  theme_bw() + 
  theme(legend.position = "inside",
        legend.position.inside = c(0.25, 0.8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_text(size = 15, family = "sans", face = "bold"),
        axis.title.y = element_text(size = 15, family = "sans", face = "bold"),
        legend.text = element_text(size=10, family = "sans", face = "bold"),
        axis.text = element_text(size = 12, family = "sans"),
        legend.title = element_blank())

weird.espr.boxplot <- ggplot(pr.m, aes(fill = name, x = name, y = value)) +
  geom_boxplot() + stat_summary(fun = mean, color = "red") +
  stat_summary(fun.min = function(x) mean(x) - sd(x), fun.max = function(x) mean(x) + sd(x), 
               geom = "errorbar", color = "red", width = .3) +
  scale_fill_manual(values = colors) +
  xlab("") +
  ylab("Expected Shortfall") + 
  scale_x_discrete(labels = variables) + 
  #ylim(-100, 800) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_text(size = 15, family = "sans", face = "bold"),
        axis.title.y = element_text(size = 15, family = "sans", face = "bold"),
        legend.text = element_text(size=10, family = "sans", face = "bold"),
        axis.text = element_text(size = 12, family = "sans"),
        legend.position = "none")

a <- ggarrange(weird.espr.boxplot, weird.resid.pr, nrow = 2, labels = c("a", "b"))
ggarrange(a, weirdest.map, ncol = 2, labels = c("", "c"))

ggsave("Figures/Results/S3_weird_estuaries.png",
       width = 10,
       height = 7,
       dpi = 600)

#supplementary S4 (bootstrap stats testing) ----------------------------

#separate temp and precip to test datasets separately - need to separate all 4 test stats
ESpr.kw <- COMPARE[,c(1, 6:9)]
#ESpr.kw <- pivot_longer(ESpr.kw, cols = c(2:5))

EStas.kw <- COMPARE[,c(1:5)]
#EStas.kw <- pivot_longer(EStas.kw, cols = c(2:5))

boot.tas <- data.frame(KW.test.stat = NA,
                       p = NA)

for(i in 1:100) {
  
  d <- EStas.kw[sample(nrow(EStas.kw), 30),] %>% pivot_longer(cols = c(2:5))
  test <- kruskal.test(value ~ name, data = d)
  boot.tas[i,1] <- as.numeric(test$statistic)
  boot.tas[i,2] <- test$p.value
  
}

#mean KW value: 23.38876, mean p value: 0.0006986924

boot.pr <- data.frame(KW.test.stat = NA,
                      p = NA)

for(i in 1:100) {
  
  d <- ESpr.kw[sample(nrow(ESpr.kw), 30),] %>% pivot_longer(cols = c(2:5))
  test <- kruskal.test(value ~ name, data = d)
  boot.pr[i,1] <- test$statistic
  boot.pr[i,2] <- test$p.value
  
}

#mean KW value: 3.350953, mean p value: 0.4165543

#generate plots

temp.kw.hist <- ggplot(boot.tas) +
  geom_histogram(aes(x = KW.test.stat), color = "black", fill = "#e63946") +
  geom_vline(xintercept = 7.815, color = "gray") +
  scale_y_continuous(expand = expansion(mult = 0)) +
  xlab("Kruskal-Wallis test statistic") +
  xlim(-5, 55) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 15, family = "sans", face = "bold"),
        axis.title.y = element_blank(),
        legend.text = element_text(size=10, family = "sans", face = "bold"),
        axis.text = element_text(size = 12, family = "sans"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

precip.kw.hist <- ggplot(boot.pr) +
  geom_histogram(aes(x = KW.test.stat), color = "black", fill = "#457b9d") +
  geom_vline(xintercept = 7.815, color = "gray") +
  scale_y_continuous(expand = expansion(mult = 0)) +
  xlab("Kruskal-Wallis test statistic") +
  xlim(0, 10) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 15, family = "sans", face = "bold"),
        axis.text = element_text(size = 12, family = "sans"),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

temp.p.hist <- ggplot(boot.tas) +
  geom_histogram(aes(x = p), color = "black", fill = "#e63946") +
  scale_y_continuous(expand = expansion(mult = 0)) +
  xlab("Kruskal-Wallis test p-value") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 15, family = "sans", face = "bold"),
        axis.title.y = element_blank(),
        legend.text = element_text(size=10, family = "sans", face = "bold"),
        axis.text = element_text(size = 12, family = "sans"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

precip.p.hist <- ggplot(boot.pr) +
  geom_histogram(aes(x = p), color = "black", fill = "#457b9d") +
  scale_y_continuous(expand = expansion(mult = 0)) +
  xlab("Kruskal-Wallis test p-value") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 15, family = "sans", face = "bold"),
        legend.text = element_text(size=10, family = "sans", face = "bold"),
        axis.text = element_text(size = 12, family = "sans"),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggarrange(temp.kw.hist, precip.kw.hist, temp.p.hist, precip.p.hist, labels = c("a", "b", "c", "d"))

ggsave("Figures/Results/S4_KW_test_validation.png",
       width = 9.5,
       height = 7,
       dpi = 600)