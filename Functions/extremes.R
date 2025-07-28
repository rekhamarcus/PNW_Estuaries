#functions to calculate changes in maxima and minima of future extreme events 

extremes.max <- function(data = sbe.extremes, var = sbe.extremes$prmax, results = sbe.pr.results, var.name = "pra") {
  
  #run model
  model <- fevd(var, data = data, type = "GEV", method = "GMLE", time.units = "years")
  
  #extract distribution shape & append to projection data
  results$shape <- model$results$par[3]
  
  #calculate Es and RP for each scenario
  for(i in 1:nrow(results)){
    
    #calculate expected shortfall at 20 year event
    p <- seq(0.95, 0.9999, length.out = 1000)
    q <- qevd(p, loc = results$mean[i], scale = results$SD[i], shape = results$shape[i])
    
    results$ES0.95[i] <- sum(p*q)/1000 #ask gerald to confirm this calculation
    
    #calculate expected shortfall at 100 year event
    p <- seq(0.99, 0.9999, length.out = 1000)
    q <- qevd(p, loc = results$mean[i], scale = results$SD[i], shape = results$shape[i])
    
    results$ES0.99[i] <- sum(p*q)/1000 #ask gerald to confirm this calculation
    
    #calculate return periods
    results$RP20[i] <- qevd(0.95, loc = results$mean[i], scale = results$SD[i], shape = results$shape[i])
    results$RP100[i] <- qevd(0.99, loc = results$mean[i], scale = results$SD[i], shape = results$shape[i])
    
  }
  
  names(results[2]) <- var.name
  
  results
  
}

extremes.min <- function(data = sbe.extremes, var = sbe.extremes$prmin, results = sbe.pr.results, var.name = "pra", precip = FALSE) {
  
  #run model
  model <- fevd(var ~ 1, data = data, type = "GEV", method = "GMLE", time.units = "years")
  
  #extract loc/scale/shape & append to projection data
  results$shape <- model$results$par[3]
  
  if(precip == TRUE){
    for(i in 1:nrow(results)){
      #yearly probability of a month with 0 precipitation, expressed as a percent
      results$drought[i] <- pevd((-100:0), loc = results$mean[i], scale = results$SD[i], shape = results$shape[i])*100
    }
  } else {
    for(i in 1:nrow(results)){
      
      #calculate expected shortfall at 20 year event
      p <- seq(0.00001, 0.05, length.out = 1000)
      q <- qevd(p, loc = results$mean[i], scale = results$SD[i], shape = results$shape[i])
      
      results$ES0.05[i] <- sum(p*q)/1000 #ask gerald to confirm this calculation
      
      #calculate expected shortfall at 100 year event
      p <- seq(0.00001, 0.01, length.out = 1000)
      q <- qevd(p, loc = results$mean[i], scale = results$SD[i], shape = results$shape[i])
      
      #find probability of pr being less than or equal to 0 to calculate RP of droughts/ES of droughts
      pevd(p, loc = results$mean[i], scale = results$SD[i], shape = results$shape[i] )
      
      results$ES0.01[i] <- sum(p*q)/1000 #ask gerald to confirm this calculation
      
      #calculate return periods
      results$RP20[i] <- qevd(0.05, loc = results$mean[i], scale = results$SD[i], shape = results$shape[i])
      results$RP100[i] <- qevd(0.01, loc = results$mean[i], scale = results$SD[i], shape = results$shape[i])
    }
  }
  
  names(results[2]) <- var.name
  
  results
  
}
