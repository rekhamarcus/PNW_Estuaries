#functions to create a matrix of temperature and precipitation data for use modelling

##'documentation on the units used differs between technical documentation on EnviDat 
##'(https://www.envidat.ch/#/metadata/chelsa-climatologies) and documentation on the 
##'CHELSA website (https://www.chelsa-climate.org/datasets, including chelsa-monthly, 
##'chelsa-bioclim, and chelsa-climatologies). below is what i have determined are the 
##'correct units for each variable used here:
##'
##'chelsa-monthly (downscaled historical data):
##'tas: K/10
##'pr: (kg m-2/month)/100 (kg m-2/month is equivalent to 1 mm of rainfall)
##'
##'chelsa-climatologies (projected means):
##'tas(ukesm, mpi, mri, and ipsl): K
##'tas (gfdl): C
##'pr: kg m-2/month
##'
##'chelsa-bioclim (projected seasonalities): 
##'bio4: C/100 (standard deviation)
##'bio15: kg m-2/month (coefficient of variation, calculated using (100*SD)/mean)
##'

matrix.futures.temp <- function(hist = tas.estuary, sd = bio4.estuary, proj = tas.proj.estuary) {
  
  #take the mean sd for each layer in the raster stack
  x <- global(sd, 'mean', na.rm=TRUE) # keep this dataframe and add on yearly averages
  
  #take mean temp for each layer in the raster stack
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
    summarise(mean = mean(mean)) #documentation states these units are in K, these numbers indicate C
  
  #add SD into dataframe
  y <- cbind(y, x)
  names(y)[3] <- "SD"
  y$SD <- y$SD/100 #change units from C/100 to C
  
  y <- y[c(1:9),] #remove historical summary data
  
  #add in historical data
  
  y[10,1] <- "1980_2019_ref"
  
  d <- as.data.frame(mean(hist)/10 - 273.15) # documentation doesn't indicate /10, but otherwise values are incorrect
  
  #ifelse to deal with estuaries that only have 1 raster cell
  if(nrow(d) > 1) {
    d.sd <- app((hist/10) - 237.15, "sd")
    y[10,3] <- mean(values(d.sd, na.rm = TRUE))
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

matrix.futures.temp.K <- function(hist = tas.estuary, sd = bio4.estuary, proj = tas.proj.estuary) {
  
  #take the mean sd for each layer in the raster stack
  x <- global(sd, 'mean', na.rm=TRUE) # keep this dataframe and add on yearly averages
  
  #take mean temp for each layer in the raster stack
  y <- global(proj, 'mean', na.rm=TRUE)
  y <- setDT(y, keep.rownames = "file")
  
  #add time period to dataframe
  y$period <- paste(substr(y$file,
                           start = nchar(y$file) - nchar('yyyy_yyyy_norm'),
                           stop = nchar(y$file) - nchar('_norm')), 
                    substr(y$file,
                           start = nchar(y$file) - nchar('sspxxx_tas_mm_yyyy_yyyy_norm'),
                           stop = nchar(y$file) - nchar('_tas_mm_yyyy_yyyy_norm')), sep = "")
  
  #group by time period and take mean
  y <- group_by(y, period) %>%
    summarise(mean = mean(mean)- 273.15) #convert from K to C
  
  #add SD into dataframe
  y <- cbind(y, x)
  names(y)[3] <- "SD"
  y$SD <- y$SD/100 #change units from C/100 to C
  
  y <- y[c(1:9),] #remove historical summary data
  
  #add in historical data
  
  y[10,1] <- "1980_2019_ref"
  
  d <- as.data.frame(mean(hist)/10 - 273.15) # documentation doesn't indicate /10, but otherwise values are incorrect
  
  #ifelse to deal with estuaries that only have 1 raster cell
  if(nrow(d) > 1) {
    d.sd <- app((hist/10) - 237.15, "sd")
    y[10,3] <- mean(values(d.sd, na.rm = TRUE))
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
  
  #take mean precip for each layer in the raster stack
  y <- global(proj, 'mean', na.rm=TRUE)
  y <- setDT(y, keep.rownames = "file")
  
  #add time period to dataframe
  y$period <- paste(substr(y$file,
                           start = nchar(y$file) - nchar('yyyy_yyyy_norm'),
                           stop = nchar(y$file) - nchar('_norm')), 
                    substr(y$file,
                           start = nchar(y$file) - nchar('sspxxx_pr_mm_yyyy_yyyy_norm'),
                           stop = nchar(y$file) - nchar('_pr_mm_yyyy_yyyy_norm')), sep = "")
  
  #group by time period and take mean
  y <- group_by(y, period) %>%
    summarise(mean = mean(mean))
  
  #add SD into dataframe
  y <- cbind(y, x)
  names(y)[3] <- "SD"
  y$SD <- (y$SD*y$mean)/100 #convert from coefficient of variation to standard deviation 
  
  #add in historical data
  
  y[10,1] <- "1980_2019_ref"
  
  d <- as.data.frame(mean(hist))
  
  if(nrow(d) > 1) {
    d.sd <- app(hist, "sd")
    y[10,3] <- as.double(mean(values(d.sd, na.rm = TRUE))/100)
  } else {
    if(nrow(d) == 0){
      y[10,3] <- NA
    } else {
      d <- as.data.frame(hist) %>% 
        pivot_longer(cols = 1:486) 
      d <- d[,2]
      names(d) <- "mean"
      
      y[10,3] <- values(app(hist, "sd")/100, na.rm = TRUE)
    }
    
  }
  
  y[10,2] <- mean(d$mean)/100
  
  y
  
}
