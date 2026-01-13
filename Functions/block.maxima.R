#function to extract block maxima from temperature and precipitation data

block.maxima <- function(temp = tas.estuary, precip = pr.estuary){
  
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
  test <- as.data.frame(precip, xy = TRUE) %>%
    pivot_longer(cols = c(3:488))
  
  p <- as.data.frame(minmax(precip))
  minp <- p[1,]
  maxp <- p[2,]
  
  w <- pivot_longer(minp, cols = c(1:486))
  w$year <- substr(w$name,
                   start = nchar(w$name) - nchar('yyyy_V.2.1') + 1,
                   stop = nchar(w$name) - nchar('_V.2.1'))
  
  pr.min <- group_by(w, year) %>%
    summarise(prmin = min(value)/100)
  
  z <- pivot_longer(maxp, cols = c(1:486))
  z$year <- substr(z$name,
                   start = nchar(z$name) - nchar('yyyy_V.2.1') + 1,
                   stop = nchar(z$name) - nchar('_V.2.1'))
  
  pr.max <- group_by(z, year) %>%
    summarise(prmax = max(value)/100)
  
  #join all data together
  extremes <- right_join(tas.min, tas.max)
  extremes <- right_join(extremes, pr.max)
  extremes <- right_join(extremes, pr.min)
  
  extremes
  
}
