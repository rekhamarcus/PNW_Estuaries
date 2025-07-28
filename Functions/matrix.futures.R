#functions to create a matrix of temperature and precipitation data for use modelling

matrix.futures.temp <- function(hist = tas.estuary, sd = bio4.estuary, proj = tas.proj.estuary) {
  
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
