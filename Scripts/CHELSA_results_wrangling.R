#group results of analysis

library(dplyr)

setwd('C:/Users/rekhamarcus/OneDrive - University of Victoria/Wetlands/')

#import data

r <- readRDS('Data/Results/results.rds')

estuaries <- unique(r$EST_ID)

SUM <- list()

for(i in 1:length(estuaries)){
  
  #extract estuary
  e <- r[r$EST_ID == estuaries[i],]
  
  #set up dataframe 
  df <- data.frame(EST_ID = estuaries[i],
                   max.tas.mean.change = NA,
                   min.tas.mean.change = NA,
                   range.tas.mean.change = NA,
                   max.tas.SD.change = NA,
                   min.tas.SD.change = NA,
                   range.tas.SD.change = NA,
                   max.tas.ext.change = NA,
                   min.tas.ext.change = NA,
                   range.tas.ext.change = NA,
                   max.pr.mean.change = NA,
                   min.pr.mean.change = NA,
                   range.pr.mean.change = NA,
                   max.pr.SD.change = NA,
                   min.pr.SD.change = NA,
                   range.pr.SD.change = NA,
                   max.pr.ext.change = NA,
                   min.pr.ext.change = NA,
                   range.pr.ext.change = NA)
  
  #find max and min
  
  df$max.tas.mean.change <- max(e$mean.tas)
  df$max.tas.SD.change <- max(e$SD.tas)
  df$max.tas.ext.change <- max(e$ES0.99.tas)
  df$max.pr.mean.change <- max(e$mean.pr)
  df$max.pr.SD.change <- max(e$SD.pr)
  df$max.pr.ext.change <- max(e$ES0.99.pr)
  
  df$min.tas.mean.change <- min(e$mean.tas)
  df$min.tas.SD.change <- min(e$SD.tas)
  df$min.tas.ext.change <- min(e$ES0.99.tas)
  df$min.pr.mean.change <- min(e$mean.pr)
  df$min.pr.SD.change <- min(e$SD.pr)
  df$min.pr.ext.change <- min(e$ES0.99.pr)
  
  #find range/difference
  
  df$range.tas.mean.change <- df$max.tas.mean.change - df$min.tas.mean.change
  df$range.tas.SD.change <-  df$max.tas.SD.change -  df$min.tas.SD.change
  df$range.tas.ext.change <-  df$max.tas.ext.change - df$min.tas.ext.change
  df$range.pr.mean.change <- df$max.pr.mean.change - df$min.pr.mean.change
  df$range.pr.SD.change <-  df$max.pr.SD.change -  df$min.pr.SD.change
  df$range.pr.ext.change <-  df$max.pr.ext.change - df$min.pr.ext.change
  
  SUM[[i]] <- df
  
}

results.summary <- do.call(rbind, SUM)

saveRDS(results.summary, "Data/Results/results_summary.rds")
write.csv(results.summary, "Data/Results/results_summary.csv")
