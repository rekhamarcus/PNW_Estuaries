#back casting for data validation

library('dplyr')
library('terra') #for working with spatial data
library('sf') #for working with spatial data
library('extRemes') #for modelling extreme events
library('data.table') #for matrix.futures function
library('tidyr') #for pivot_longer function
library('ggplot2')
library('ggpubr')
library('ggspatial')

setwd("C:/Users/rekha/OneDrive - University of Victoria/Wetlands")

#load in functions for analysis
source("PNW_Estuaries/Functions/crop.estuary.R") #function to crop data files to a single estuary

estuaries <- readRDS("Data/Shapefiles/estuaries.rds")
estuaries <- st_transform(estuaries, crs = st_crs("EPSG:4326"))

#pnw <- readRDS("Data/Shapefiles/PNW_shapefile.rds")
#pnw <- st_transform(pnw, crs = st_crs("EPSG:4326"))

##' this historical observed data was obtained from https://psl.noaa.gov/data/gridded/data.UDel_AirT_Precip.html
##' these are 50 sq km resolution observations, and they seem to be quite inaccurate - 1900 monthly temperature in portland in june/july was
##' 26 degrees C, but these data say it was 12 degrees C. finding other data!
##' 
##' https://psl.noaa.gov/data/gridded/data.ghcncams.html - GHCN_CAMS Gridded 2m Temperature (Land) for better temperature data?
##' 

#load in observed historical data - first wrangle and crop it to study area
#tas.obs <- rast('Data/Temperature/air.mon.mean.v401.nc') %>% rotate()
#tas.obs <- crop(tas.obs, pnw)
#tas.obs <- mask(tas.obs, pnw)
#saveRDS(tas.obs, 'Data/Temperature/NOAA_monthly_timeseries_historical/PNW_air_mon_mean_v401.rds')

#try new temperature data
#tas.obs <- rast('Data/Temperature/air.mon.mean.nc') %>% rotate()
#tas.obs <- crop(tas.obs, pnw)
#tas.obs <- mask(tas.obs, pnw)
#saveRDS(tas.obs, 'Data/Temperature/NOAA_monthly_timeseries_historical/PNW_air_mon_mean_GHCN_CAMS.rds')

#pr.obs <- rast("Data/Precipitation/precip.mon.total.v401.nc") %>% rotate()
#pr.obs <- crop(pr.obs, pnw)
#pr.obs <- mask(pr.obs, pnw)
#saveRDS(pr.obs, 'Data/Precipitation/NOAA_monthly_timeseries_historical/PNW_precip_mon_total_v401.rds')

tas <- list.files('Data/Temperature/CHELSA_monthly_timeseries_historical', pattern = ".tif", full.names = T)
pr <- list.files('Data/Precipitation/CHELSA_monthly_timeseries_historical', pattern = ".tif", full.names = T)

tas.obs <- readRDS('Data/Temperature/NOAA_monthly_timeseries_historical/PNW_air_mon_mean_GHCN_CAMS.rds')
pr.obs <- readRDS('Data/Precipitation/NOAA_monthly_timeseries_historical/PNW_precip_mon_total_v401.rds')

compare.backcast <- list()

for(i in 125:nrow(estuaries)){
  
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

#Plots ------------------------------------------------------------------------

#plot distribution of tas/pr extremes of 4 results

temp.density <- ggplot(COMPARE) +
  geom_density(aes(x = EStas.modelled.sum, color = "Modelled Summary Stats"), fill = "#023047", alpha = 0.4) +
  geom_density(aes(x = EStas.modelled, color = "Modelled Data"), fill = "#219ebc", alpha = 0.4) +
  geom_density(aes(x = EStas.observed.sum, color = "Observed Summary Stats"), fill = "#ffb703", alpha = 0.4) +
  geom_density(aes(x = EStas.observed, color = "Observed Data"), fill = "#fb8500", alpha = 0.4) +
  scale_color_manual(values = c("black", "black", "black", "black")) + 
  scale_y_continuous(expand = expansion(mult = 0)) +
  scale_x_continuous(expand = expansion(mult = 0)) +
  xlab("Expected Shortfall of Temperature at 99th percentile") +
  ylab("Density") +
  xlim(0, 20) +
  theme_bw() + 
  theme(legend.position = "inside",
        legend.position.inside = c(0.2, 0.8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank())

pr.density <- ggplot(COMPARE) +
  geom_density(aes(x = ESpr.modelled.sum, color = "Modelled Summary Stats"), fill = "#023047", alpha = 0.4) +
  geom_density(aes(x = ESpr.modelled, color = "Modelled Data"), fill = "#219ebc", alpha = 0.4) +
  geom_density(aes(x = ESpr.observed.sum, color = "Observed Summary Stats"), fill = "#ffb703", alpha = 0.4) +
  geom_density(aes(x = ESpr.observed, color = "Observed Data"), fill = "#fb8500", alpha = 0.4) +
  scale_color_manual(values = c("black", "black", "black", "black")) + 
  scale_y_continuous(expand = expansion(mult = 0)) +
  scale_x_continuous(expand = expansion(mult = 0)) +
  xlab("Expected Shortfall of Precipitation at 99th percentile") +
  ylab("Density") +
  xlim(-100, 800) +
  theme_bw() + 
  theme(legend.position = "inside",
        legend.position.inside = c(0.8, 0.8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank())

ggarrange(temp.density, pr.density, ncol = 2, labels = c("a", "b"))

ggsave('Figures/Results/ES.observation.validation.png',
       width = 10,
       height = 5,
       dpi = 600)

#plot distribution of tail shapes for all 3 approaches (extremes, modelled means, observed means)

temp.tails <- ggplot(COMPARE) +
  geom_density(aes(x = tas.shape, color = "Extreme data model"), fill = "#457b9d", alpha = 0.4) +
  geom_density(aes(x = tas.modelled.shape, color = "Mean modelled data model"), fill = "#e63946", alpha = 0.4) +
  geom_density(aes(x = tas.observed.shape, color = "Mean observed data model"), fill = "#ffb703", alpha = 0.4) +
  scale_color_manual(values = c("black", "black", "black", "black")) + 
  scale_y_continuous(expand = expansion(mult = 0)) +
  scale_x_continuous(expand = expansion(mult = 0)) +
  xlab("Tail Shape of Temperature Data") +
  ylab("Density") +
  xlim(-0.6, 0) + 
  theme_bw() + 
  theme(legend.position = "inside",
        legend.position.inside = c(0.25, 0.8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank())

pr.tails <- ggplot(COMPARE) +
  geom_density(aes(x = pr.shape, color = "Extreme data model"), fill = "#457b9d", alpha = 0.4) +
  geom_density(aes(x = pr.modelled.shape, color = "Mean modelled data model"), fill = "#e63946", alpha = 0.4) +
  geom_density(aes(x = pr.observed.shape, color = "Mean observed data model"), fill = "#ffb703", alpha = 0.4) +
  scale_color_manual(values = c("black", "black", "black", "black")) + 
  scale_y_continuous(expand = expansion(mult = 0)) +
  scale_x_continuous(expand = expansion(mult = 0)) +
  xlab("Tail Shape of Precipitation Data") +
  ylab("Density") +
  xlim(-0.9, 0.15) + 
  theme_bw() + 
  theme(legend.position = "inside",
        legend.position.inside = c(0.25, 0.8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank())

ggarrange(temp.tails, pr.tails, ncol = 2, labels = c("a", "b"))

ggsave('Figures/Results/tail.validation.png',
       width = 10,
       height = 5,
       dpi = 600)

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
  xlab("Temperature Residuals") +
  ylab("Frequency") +
  theme_bw() + 
  theme(legend.position = "inside",
        legend.position.inside = c(0.75, 0.8),
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
  xlab("Precipitation Residuals") +
  ylab("Frequency") +
  xlim(-500, 500) +
  theme_bw() + 
  theme(legend.position = "inside",
        legend.position.inside = c(0.75, 0.8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank())

ggsave('Figures/Results/precip.validation.residuals.png',
       width = 6,
       height = 6,
       dpi = 600)

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

ggarrange(temp.density, pr.density, resid.tas, resid.pr, ncol = 2, nrow = 2, labels = c("a", "b", "c", "d"))

ggsave('Figures/Results/validation.residuals.png',
       width = 11.7,
       height = 7.8,
       dpi = 600)

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

#calculate standard error of each category
sd(COMPARE$EStas.modelled.sum, na.rm = TRUE)/sqrt(length(COMPARE$EStas.modelled.sum)) #0.04304211
sd(COMPARE$EStas.modelled, na.rm = TRUE)/sqrt(length(COMPARE$EStas.modelled)) #0.04827815
sd(COMPARE$EStas.observed.sum, na.rm = TRUE)/sqrt(length(COMPARE$EStas.observed.sum)) #0.08377333
sd(COMPARE$EStas.observed, na.rm = TRUE)/sqrt(length(COMPARE$EStas.observed)) #0.08110828

sd(COMPARE$ESpr.modelled.sum, na.rm = TRUE)/sqrt(length(COMPARE$ESpr.modelled.sum)) #5.209766
sd(COMPARE$ESpr.modelled, na.rm = TRUE)/sqrt(length(COMPARE$ESpr.modelled)) #4.981672
sd(COMPARE$ESpr.observed.sum, na.rm = TRUE)/sqrt(length(COMPARE$ESpr.observed.sum)) #4.07505
