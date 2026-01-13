#modelling changes in mean and extremes for all estuaries

library('dplyr')
library('terra') #for working with spatial data
library('sf') #for working with spatial data
library('extRemes') #for modelling extreme events
library('data.table') #for matrix.futures function
library('tidyr') #for pivot_longer function

setwd("C:/Users/rekhamarcus/OneDrive - University of Victoria/Wetlands")

#load in functions for analysis
source("PNW_Estuaries/Functions/block.maxima.R") #function to extract block maxima from historical data
source("PNW_Estuaries/Functions/crop.estuary.R") #function to crop data files to a single estuary
source("PNW_Estuaries/Functions/matrix.futures.R") #functions to wrangle future projected raster data into matrices
source("PNW_Estuaries/Functions/extremes.R") #functions to model and calculate changes in extreme events
source("PNW_Estuaries/Functions/percent.change.R") #function to calculate percent change of mean, SD, and extremes

#load in data

tas <- list.files('Data/Temperature/CHELSA_monthly_timeseries_historical', pattern = ".tif", full.names = T)
tas.proj <- list.files('Data/Temperature/CHELSA_ukesm1-0-ll_projections/monthly_projections', pattern = ".tif", full.names = T)
bio4 <- list.files('Data/Temperature/CHELSA_ukesm1-0-ll_projections/seasonality_projections', pattern = ".tif", full.names = T)

pr <- list.files('Data/Precipitation/CHELSA_monthly_timeseries_historical', pattern = ".tif", full.names = T)
pr.proj <- list.files('Data/Precipitation/CHELSA_ukesm1-0-ll_projections/monthly_projections', pattern = ".tif", full.names = T)
bio15 <- list.files('Data/Precipitation/CHELSA_ukesm1-0-ll_projections/seasonality_projections', pattern = ".tif", full.names = T)

estuaries <- readRDS("Data/Shapefiles/estuaries.rds")
estuaries <- st_transform(estuaries, crs = st_crs("EPSG:4326"))
#estuary.subset <- readRDS("Data/Shapefiles/estuary_subset.rds")
#estuary.subset <- st_transform(estuary.subset, crs = st_crs("EPSG:4326"))

#set up data that will result from each forloop iteration

RESULTS <- list()

for(i in 1:nrow(estuaries)){

#crop data to single estuary

tas.estuary <- crop.estuary(files = tas, shape = estuaries[i,])
tas.proj.estuary <- crop.estuary(files = tas.proj, shape = estuaries[i,])
bio4.estuary <- crop.estuary(files = bio4, shape = estuaries[i,])

pr.estuary <- crop.estuary(files = pr, shape = estuaries[i,])
pr.proj.estuary <- crop.estuary(files = pr.proj, shape = estuaries[i,])
bio15.estuary <- crop.estuary(files = bio15, shape = estuaries[i,])

#extract block maxima from historical data - will print 'Joining with `by = join_by(year)`'

extremes <- block.maxima(temp = tas.estuary, pr = pr.estuary)    

#wrangle future data into matrix

pr.results <- matrix.futures.pr(hist = pr.estuary, sd = bio15.estuary, proj = pr.proj.estuary)
tas.results <- matrix.futures.temp.K(hist = tas.estuary, sd = bio4.estuary, proj = tas.proj.estuary)
#tas.results <- matrix.futures.temp(hist = tas.estuary, sd = bio4.estuary, proj = tas.proj.estuary) # - FOR GFDL ONLY (tas data in C)

#calculate future extremes
if(!is.nan(extremes$prmax[1])) {
prmax.extremes <- extremes.max(data = extremes, var = extremes$prmax, results = pr.results, var.name = "pra_max")
}
if(!is.nan(extremes$tasmax[1])) {
tasmax.extremes <- extremes.max(data = extremes, var = extremes$tasmax, results = tas.results, var.name = "tas_max")
}
if(!is.nan(extremes$prmin[1])) {
prmin.extremes <- extremes.min(data = extremes, var = extremes$prmin, results = pr.results, var.name = "pra_min", precip = TRUE)
}
if(!is.nan(extremes$tasmin[1])) {
tasmin.extremes <- extremes.min(data = extremes, var = -extremes$tasmin, results = tas.results, var.name = "tas_min")
}

#create mega df with all percent changes per estuary

pct.change <- as.data.frame(pr.results[,1])
pct.change$EST_ID <- estuaries$EST_ID[i] #need this for full forloop process

possibleError <- tryCatch(
pct.change.estuary <- percent.change(r1 = prmax.extremes, r2 = tasmax.extremes, r3 = tasmin.extremes, r4 = prmin.extremes),
error=function(e) e)

if(inherits(possibleError, "error")) next

pct.change.estuary <- pct.change.estuary[c(1:9),]

#add in ssp scenarios
ssp126 <- pct.change.estuary[pct.change.estuary$`pr.results[, 1]` == "2011-2040_ssp126_" | 
                                pct.change.estuary$`pr.results[, 1]` == "2041-2070_ssp126_" |
                                pct.change.estuary$`pr.results[, 1]` == "2071-2100_ssp126_",]
ssp126$years <- substr(ssp126$`pr.results[, 1]`,
                       start = nchar(ssp126$`pr.results[, 1]`) - nchar('yyyy_yyyy_sspxxx_') + 1,
                       stop = nchar(ssp126$`pr.results[, 1]`) - nchar('_sspxxx_'))
ssp126$SSP <- "SSP1_2.6"

ssp370 <- pct.change.estuary[pct.change.estuary$`pr.results[, 1]` == "2011-2040_ssp370_" | 
                               pct.change.estuary$`pr.results[, 1]` == "2041-2070_ssp370_" |
                               pct.change.estuary$`pr.results[, 1]` == "2071-2100_ssp370_",]
ssp370$years <- substr(ssp370$`pr.results[, 1]`,
                       start = nchar(ssp370$`pr.results[, 1]`) - nchar('yyyy_yyyy_sspxxx_') + 1,
                       stop = nchar(ssp370$`pr.results[, 1]`) - nchar('_sspxxx_'))
ssp370$SSP <- "SSP3_7.0"

ssp585 <- pct.change.estuary[pct.change.estuary$`pr.results[, 1]` == "2011-2040_ssp585_" | 
                               pct.change.estuary$`pr.results[, 1]` == "2041-2070_ssp585_" |
                               pct.change.estuary$`pr.results[, 1]` == "2071-2100_ssp585_",]
ssp585$years <- substr(ssp585$`pr.results[, 1]`,
                       start = nchar(ssp585$`pr.results[, 1]`) - nchar('yyyy_yyyy_sspxxx_') + 1,
                       stop = nchar(ssp585$`pr.results[, 1]`) - nchar('_sspxxx_'))
ssp585$SSP <- "SSP5_8.5"

#merge all data together

results <- rbind(ssp126, ssp370, ssp585)
results <- results[,c(21, 20, 2:4, 6, 9:10, 12, 5, 7:8, 11, 13:19)]

RESULTS[[i]] <- results

print (i)

}

results <- do.call(rbind, RESULTS)

#wrangle datasets for analysis --------------------------------------

results <- results[,c(20, 1:19)]

#subset all results by region
SWBC <- readRDS("Data/Shapefiles/Regions/southwestern_BC.rds")
NWVI <- readRDS("Data/Shapefiles/Regions/northwest_VI.rds")
NCM <- readRDS("Data/Shapefiles/Regions/northcentral_BC.rds")
HG <- readRDS("Data/Shapefiles/Regions/haida_gwaii.rds")
SS <- readRDS("Data/Shapefiles/Regions/salish_sea.rds")
WONC <- readRDS("Data/Shapefiles/Regions/WA_OR_northernCA.rds")
CC <- readRDS("Data/Shapefiles/Regions/central_CA.rds")

results.SWBC <- results[results$EST_ID %in% SWBC$EST_NO,]
#saveRDS(results.SWBC, "Data/Results/results.SWBC.rds")
results.NWVI <- results[results$EST_ID %in% NWVI$EST_NO,]
#saveRDS(results.NWVI, "Data/Results/results.NWVI.rds")
results.NCM <- results[results$EST_ID %in% NCM$EST_NO,]
#saveRDS(results.NCM, "Data/Results/results.NCM.rds")
results.HG <- results[results$EST_ID %in% HG$EST_NO,]
#saveRDS(results.HG, "Data/Results/results.HG.rds")
results.SS <- results[results$EST_ID %in% SS$PMEP_EstuaryID,]
#saveRDS(results.SS, "Data/Results/results.SS.rds")
results.WONC <- results[results$EST_ID %in% WONC$PMEP_EstuaryID,]
#saveRDS(results.WONC, "Data/Results/results.WONC.rds")
results.CC <- results[results$EST_ID %in% CC$PMEP_EstuaryID,]
#saveRDS(results.CC, "Data/Results/results.CC.rds")

#add in regions to results dataset
results.SWBC$region <- "SWBC"
results.NWVI$region <- "NWVI"
results.NCM$region <- "NCM"
results.HG$region <- "HG"
results.SS$region <- "SS"
results.WONC$region <- "WONC"
results.CC$region <- "CC"

#save all results in single file
results <- rbind(results.SWBC, results.NWVI, results.NCM, results.HG, results.SS, results.WONC, results.CC)

results$model <- "ukesm"

saveRDS(results, "Data/Results/Results_ukesm_model/results_ukesm.rds")

#load in datasets & bind them together -----------------------------------------

results.gfdl <- readRDS('Data/Results/Results_gfdl_model/results_gfdl.rds')
results.ipfl <- readRDS('Data/Results/Results_ipsl_model/results_ipsl.rds')
results.mpi <- readRDS('Data/Results/Results_mpi_model/results_mpi.rds')
results.mri <- readRDS('Data/Results/Results_mri_model/results_mri.rds')
results.ukesm <- readRDS('Data/Results/Results_ukesm_model/results_ukesm.rds')

results <- rbind(results.gfdl, results.ipfl, results.mpi, results.mri, results.ukesm)
saveRDS(results, "Data/Results/results.rds")

#fix incorrect year markers bruh ---------------------------------------------------

results.8.5 <- results[results$SSP == "SSP5_8.5",]
results.8.5 <- results.8.5[!results.8.5$model == "gfdl",]

results.8.5$years <- paste("2", results.8.5$years, sep = "")
results.8.5$years <- substr(results.8.5$years, 1, nchar(results.8.5$years) - 1)

results.gfdl <- results[results$model == "gfdl",]
results.else <- results[!results$SSP == "SSP5_8.5",]
results.else <- results.else[!results.else$model == "gfdl",]

results <- rbind(results.8.5, results.gfdl, results.else)

results$years[results$years == "2011-2040"] <- "2011_2040"
results$years[results$years == "2041-2070"] <- "2041_2070"
results$years[results$years == "2071-2100"] <- "2071_2100"

saveRDS(results, "Data/Results/results.rds")

#fix fuckass list formatting wtf - OUTDATED -----------------------------------------

results$mean.pr <- as.data.frame(unlist(results$mean.pr))
results$SD.pr <- as.data.frame(unlist(results$SD.pr))
results$ES0.95.pr <- as.data.frame(unlist(results$ES0.95.pr))
results$ES0.99.pr <- as.data.frame(unlist(results$ES0.99.pr))
results$RP20.prmax <- as.data.frame(unlist(results$RP20.prmax))
results$RP100.prmax <- as.data.frame(unlist(results$RP100.prmax))
results$mean.tas <- as.data.frame(unlist(results$mean.tas))
results$SD.tas <- as.data.frame(unlist(results$SD.tas))
results$ES0.95.tas <- as.data.frame(unlist(results$ES0.95.tas))
results$ES0.99.tas <- as.data.frame(unlist(results$ES0.99.tas))
results$RP20.tasmax <- as.data.frame(unlist(results$RP20.tasmax))
results$RP100.tasmin <- as.data.frame(unlist(results$RP100.tasmin))
results$RP100.tasmax <- as.data.frame(unlist(results$RP100.tasmax))
results$ES0.05.tas <- as.data.frame(unlist(results$ES0.05.tas))
results$ES0.01.tas <- as.data.frame(unlist(results$ES0.01.tas))
results$RP20.tasmin <- as.data.frame(unlist(results$RP20.tasmin))
results$drought <- as.data.frame(unlist(results$drought))

results <- do.call(cbind, results)

names(results)[4:20] <- c("mean.pr", "SD.pr", "ES0.95", "ES0.99.pr", "RP20.prmax", "RP100.prmax",
                          "mean.tas", "SD.tas", "ES0.95.tas", "ES0.99.tas", "RP20.tasmax", "RP100.tasmax",
                          "drought", "ES0.05.tas", "ES0.01.tas", "RP20.tasmin", "RP100.tasmin")

