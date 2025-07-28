#subsetting/wrangling data

library('dplyr')
library('terra')
library('sf')
library('tidyr')
library('data.table')

setwd("C:/Users/rekhamarcus/OneDrive - University of Victoria/Wetlands")

fre <- readRDS('Data/Shapefiles/fraser_river_estuary.rds')
fre <- st_transform(fre, crs = st_crs("EPSG:4326")) #reproject to be able to crop data

sbe <- readRDS('Data/Shapefiles/subset_bay_estuary.rds')
sbe <- st_transform(sbe, crs = st_crs("EPSG:4326")) #reproject to be able to crop data

#subset data to test modelling---------------------------------------------------

#function to crop data to fraser river estuary
crop.estuary <- function(files = pr.2002, shape = fre) {
  
  SUB <- list()
  
  for(i in 1:length(files)) {
    
    r <- rast(files[i])
    c <- crop(r, shape)
    c <- mask(c, shape)
    
    SUB[[i]] <- c
    
  }
  
  rast(SUB)
  
}

tas <- list.files('Data/Temperature/CHELSA_monthly_timeseries_historical', pattern = ".tif", full.names = T)
tas.fre <- crop.estuary(files = tas)
writeRaster(tas.fre, "Data/FRE_Subsets/temperature_FRE.tif", overwrite = TRUE)
tas.sbe <- crop.estuary(files = tas, shape = sbe)
writeRaster(tas.sbe, "Data/SBE_Subsets/temperature_SBE.tif", overwrite = TRUE)

tas.proj <- list.files('Data/Temperature/CHELSA_monthly_projections', pattern = ".tif", full.names = T)
tas.proj.fre <- crop.estuary(files = tas.proj)
writeRaster(tas.proj.fre, "Data/FRE_Subsets/temperature_proj_FRE.tif")
tas.proj.sbe <- crop.estuary(files = tas.proj, shape = sbe)
writeRaster(tas.proj.sbe, "Data/SBE_Subsets/temperature_proj_SBE.tif", overwrite = TRUE)

bio4 <- list.files('Data/Temperature/CHELSA_seasonality_projections', pattern = ".tif", full.names = T)
bio4.fre <- crop.estuary(files = bio4)
writeRaster(bio4.fre, "Data/FRE_Subsets/temperature_seasonality_FRE.tif")
bio4.sbe <- crop.estuary(files = bio4, shape = sbe)
writeRaster(bio4.sbe, "Data/SBE_Subsets/temperature_seasonality_SBE.tif", overwrite = TRUE)

pr <- list.files('Data/Precipitation/CHELSA_monthly_timeseries_historical', pattern = ".tif", full.names = T)
pr.fre <- crop.estuary(files = pr)
writeRaster(pr.fre, "Data/FRE_Subsets/precipitation_FRE.tif", overwrite = TRUE) 
pr.sbe <- crop.estuary(files = pr, shape = sbe)
writeRaster(pr.sbe, "Data/SBE_Subsets/precipitation_SBE.tif", overwrite = TRUE)

pr.proj <- list.files('Data/Precipitation/CHELSA_monthly_projections', pattern = ".tif", full.names = T)
pr.proj.fre <- crop.estuary(files = pr.proj)
writeRaster(pr.proj.fre, "Data/FRE_Subsets/precipitation_proj_FRE.tif")
pr.proj.sbe <- crop.estuary(files = pr.proj, shape = sbe)
writeRaster(pr.proj.sbe, "Data/SBE_Subsets/precipitation_proj_SBE.tif", overwrite = TRUE)

bio15 <- list.files('Data/Precipitation/CHELSA_seasonality_projections', pattern = ".tif", full.names = T)
bio15.fre <- crop.estuary(files = bio15)
writeRaster(bio15.fre, "Data/FRE_Subsets/precipitation_seasonality_FRE.tif")
bio15.sbe <- crop.estuary(files = bio15, shape = sbe)
writeRaster(bio15.sbe, "Data/SBE_Subsets/precipitation_seasonality_SBE.tif", overwrite = TRUE)

#extract block maxima from historical data --------------------------------------------

block.maxima <- function(temp = tas.fre, precip = pr.fre){

  #extract min/mas of temperatures
    t <- as.data.frame(minmax(temp))
    mint <- t[1,]
    maxt <- t[2,]
    
    x <- pivot_longer(maxt, cols = c(1:491))
    x$year <- substr(x$name,
                     start = nchar(x$name) - nchar('yyyy_V.2.1') + 1,
                     stop = nchar(x$name) - nchar('_V.2.1'))
    
    tas.max <- group_by(x, year) %>%
      summarise(tasmax = (max(value)/10) - 273.15) #convert units from K*10 to C
    
    y <- pivot_longer(mint, cols = c(1:491))
    y$year <- substr(y$name,
                     start = nchar(y$name) - nchar('yyyy_V.2.1') + 1,
                     stop = nchar(y$name) - nchar('_V.2.1'))
    
    tas.min <- group_by(y, year) %>%
      summarise(tasmin = (min(value)/10) - 273.15)
    
  #extract  min/max of precipitation
    p <- as.data.frame(minmax(precip))
    minp <- p[1,]
    maxp <- p[2,]
    
    w <- pivot_longer(minp, cols = c(1:486))
    w$year <- substr(w$name,
                     start = nchar(w$name) - nchar('yyyy_V.2.1') + 1,
                     stop = nchar(w$name) - nchar('_V.2.1'))
    
    pr.max <- group_by(w, year) %>%
      summarise(prmax = max(value)/100)
    
    z <- pivot_longer(maxp, cols = c(1:486))
    z$year <- substr(z$name,
                     start = nchar(z$name) - nchar('yyyy_V.2.1') + 1,
                     stop = nchar(z$name) - nchar('_V.2.1'))
    
    pr.min <- group_by(z, year) %>%
      summarise(prmin = min(value)/100)
    
  #join all data together
    extremes <- right_join(tas.min, tas.max)
    extremes <- right_join(extremes, pr.max)
    extremes <- right_join(extremes, pr.min)
    
    extremes

}

fre.extremes <- block.maxima(temp = tas.fre, precip = pr.fre)
saveRDS(fre.extremes, 'Data/FRE_subsets/FRE_extremes.rds')

sbe.extremes <- block.maxima(temp = tas.sbe, precip = pr.sbe)
saveRDS(sbe.extremes, 'Data/SBE_subsets/SBE_extremes.rds')

#wrangle projection data to matrix --------------------------------------------

matrix.futures.temp <- function(hist = rast("Data/SBE_Subsets/temperature_SBE.tif"), sd.file = "Data/SBE_Subsets/temperature_seasonality_SBE.tif", mean.file = 'Data/SBE_Subsets/temperature_proj_SBE.tif') {
  
  #take the mean cv for each layer in the raster stack
  x <- global(sd, 'mean', na.rm=TRUE) # keep this dataframe and add on yearly averages
  x <- x[2:10,]
  
  #take mean precip for each layer in the raster stack
  y <- global(proj, 'mean', na.rm=TRUE)
  y <- setDT(y, keep.rownames = "file")
  
  #add time period to dataframe
  y$period <- paste(substr(y$file,
                           start = nchar(y$file) - nchar('yyyy_yyyy_norm') + 1,
                           stop = nchar(y$file) - nchar('_norm')), 
                    substr(y$file,
                           start = nchar(y$file) - nchar('sspxxx_tas_mm_yyyy_yyyy_norm') + 1,
                           stop = nchar(y$file) - nchar('_tas_mm_yyyy_yyyy_norm')), sep = "_")
  
  #group by time period and take mean
  y <- group_by(y, period) %>%
    summarise(mean = mean(mean))
  
  #add SD into dataframe
  y$SD <- x/100
  
  #add in historical data (we want the mean, not mean of extremes)
 
  y[10,1] <- "1980_2019_ref"
  
  d <- as.data.frame(mean(hist)/10 - 273.15) 
  
  #ifelse to deal with estuaries that only have 1 raster cell
  if(nrow(d) > 1) {
    d.sd <- app(hist, "sd")
    y[10,3] <- global(d.sd, "mean", na.rm = TRUE)/10
    y[10,2] <- mean(d$mean)
  } else {
    d <- as.data.frame(hist/10 - 273.15) %>%
      pivot_longer(cols = 1:nlyr(hist)) 
    d <- d[,2]
    names(d) <- "mean"
    y[10,3] <- sd(d$mean)
    y[10,2] <- mean(d$mean)
  }
   
  y
  
}

matrix.futures.pr <- function(hist = pr.estuary, sd = bio15.estuary, proj = pr.proj.estuary) {
  
  #take the mean cv for each layer in the raster stack
  x <- global(sd, 'mean', na.rm=TRUE) # keep this dataframe and add on yearly averages
  x <- x[2:10,]
  
  #take mean precip for each layer in the raster stack
  y <- global(proj, 'mean', na.rm=TRUE)
  y <- setDT(y, keep.rownames = "file")
  
  #add time period to dataframe
  y$period <- paste(substr(y$file,
                           start = nchar(y$file) - nchar('yyyy_yyyy_norm') + 1,
                           stop = nchar(y$file) - nchar('_norm')), 
                    substr(y$file,
                           start = nchar(y$file) - nchar('sspxxx_pr_mm_yyyy_yyyy_norm') + 1,
                           stop = nchar(y$file) - nchar('_pr_mm_yyyy_yyyy_norm')), sep = "_")
  
  #group by time period and take mean
  y <- group_by(y, period) %>%
    summarise(mean = mean(mean))
  
  #add SD into dataframe
  y$SD <- x 
  
  #add in historical data (we want the mean, not mean of extremes)
  
  y[10,1] <- "1980_2019_ref"
  
  d <- as.data.frame(mean(hist))
  
  if(nrow(d) > 1) {
    d.sd <- app(hist, "sd")
    y[10,3] <- global(d.sd, "mean", na.rm = TRUE)/100
  } else {
    d <- as.data.frame(hist) %>% 
      pivot_longer(cols = 1:486) 
    d <- d[,2]
    names(d) <- "mean"
    y[10,3] <- values(app(hist, "sd")/100)
    
  }

  y[10,2] <- mean(d$mean)/100
  
  y
  
}

pr.proj.fre <- matrix.futures.pr(hist = rast("Data/FRE_Subsets/precipitation_FRE.tif"), sd.file = "Data/FRE_Subsets/precipitation_seasonality_FRE.tif", mean.file = 'Data/FRE_Subsets/precipitation_proj_FRE.tif')
saveRDS(pr.proj.fre, "Data/FRE_Subsets/Results/pr_proj_FRE.RDS")

pr.proj.sbe <- matrix.futures.pr(hist = rast("Data/SBE_Subsets/precipitation_SBE.tif"), sd.file = "Data/SBE_Subsets/precipitation_seasonality_SBE.tif", mean.file = 'Data/SBE_Subsets/precipitation_proj_SBE.tif')
saveRDS(pr.proj.sbe, "Data/SBE_Subsets/Results/pr_proj_SBE.RDS")

tas.proj.fre <- matrix.futures.temp(hist = rast("Data/FRE_Subsets/temperature_FRE.tif"), sd.file = "Data/FRE_Subsets/temperature_seasonality_FRE.tif", mean.file = "Data/FRE_Subsets/temperature_proj_FRE.tif")
saveRDS(tas.proj.fre, "Data/FRE_Subsets/Results/tas_proj_FRE.RDS")

tas.proj.sbe <- matrix.futures.temp(hist = rast("Data/SBE_Subsets/temperature_SBE.tif"), sd.file = "Data/SBE_Subsets/temperature_seasonality_SBE.tif", mean.file = "Data/SBE_Subsets/temperature_proj_SBE.tif")
saveRDS(tas.proj.sbe, "Data/SBE_Subsets/Results/tas_proj_SBE.RDS")
