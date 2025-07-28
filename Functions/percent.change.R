#function to calculate percent change of given variables in a dataframe

percent.change <- function(r1 = prmax.extremes, r2 =  tasmax.extremes, r3 = tasmin.extremes, r4 = prmin.extremes){
  
  for(i in 1:nrow(pct.change)){
    pct.change$mean.pr[i] <- ((r1$mean[i] - r1[10,2])/r1[10,2])*100
    pct.change$SD.pr[i] <- ((r1$SD[i] - r1[10,3])/r1[10,3])*100
    pct.change$ES0.95.pr[i] <- ((r1$ES0.95[i] - r1[10,5])/r1[10,5])*100
    pct.change$ES0.99.pr[i] <- ((r1$ES0.99[i] - r1[10,6])/r1[10,6])*100
    pct.change$RP20.prmax[i] <- ((r1$RP20[i] - r1[10,7])/r1[10,7])*100
    pct.change$RP100.prmax[i] <- ((r1$RP100[i] - r1[10,8])/r1[10,8])*100
  }
  
  for(i in 1:nrow(pct.change)){
    pct.change$mean.tas[i] <- ((r2$mean[i] - r2[10,2])/r2[10,2])*100
    pct.change$SD.tas[i] <- ((r2$SD[i] - r2[10,3])/r2[10,3])*100
    pct.change$ES0.95.tas[i] <- ((r2$ES0.95[i] - r2[10,5])/r2[10,5])*100
    pct.change$ES0.99.tas[i] <- ((r2$ES0.99[i] - r2[10,6])/r2[10,6])*100
    pct.change$RP20.tasmax[i] <- ((r2$RP20[i] - r2[10,7])/r2[10,7])*100
    pct.change$RP100.tasmax[i] <- ((r2$RP100[i] - r2[10,8])/r2[10,8])*100
  }
  
  for(i in 1:nrow(pct.change)){
    pct.change$drought[i] <- ((r4$drought[i] - r4[10,5])/r4[10,5])*100
  }
  
  for(i in 1:nrow(pct.change)){
    pct.change$ES0.05.tas[i] <- ((r3$ES0.05[i] - r3[10,5])/r3[10,5])*100
    pct.change$ES0.01.tas[i] <- ((r3$ES0.01[i] - r3[10,6])/r3[10,6])*100
    pct.change$RP20.tasmin[i] <- ((r3$RP20[i] - r3[10,7])/r3[10,7])*100
    pct.change$RP100.tasmin[i] <- ((r3$RP100[i] - r3[10,8])/r3[10,8])*100
  }
  
  pct.change
  
}
