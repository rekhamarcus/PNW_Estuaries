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
tas.proj <- list.files('Data/Temperature/CHELSA_monthly_projections', pattern = ".tif", full.names = T)
bio4 <- list.files('Data/Temperature/CHELSA_seasonality_projections', pattern = ".tif", full.names = T)

pr <- list.files('Data/Precipitation/CHELSA_monthly_timeseries_historical', pattern = ".tif", full.names = T)
pr.proj <- list.files('Data/Precipitation/CHELSA_monthly_projections', pattern = ".tif", full.names = T)
bio15 <- list.files('Data/Precipitation/CHELSA_seasonality_projections', pattern = ".tif", full.names = T)

estuaries <- readRDS("Data/Shapefiles/estuaries.rds")
estuaries <- st_transform(estuaries, crs = st_crs("EPSG:4326"))
#estuary.subset <- readRDS("Data/Shapefiles/estuary_subset.rds")
#estuary.subset <- st_transform(estuary.subset, crs = st_crs("EPSG:4326"))

#set up data that will result from each forloop iteration

ssp126.2011.2040 <- list()
ssp370.2011.2040 <- list()
ssp585.2011.2040 <- list()
ssp126.2041.2070 <- list()
ssp370.2041.2070 <- list()
ssp585.2041.2070 <- list()
ssp126.2071.2100 <- list()
ssp370.2071.2100 <- list()
ssp585.2071.2100 <- list()

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
tas.results <- matrix.futures.temp(hist = tas.estuary, sd = bio4.estuary, proj = tas.proj.estuary)

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

pct.change <- pr.results[,1]
pct.change$EST_ID <- estuaries$EST_ID[i] #need this for full forloop process

possibleError <- tryCatch(
pct.change.estuary <- percent.change(r1 = prmax.extremes, r2 = tasmax.extremes, r3 = tasmin.extremes, r4 = prmin.extremes),
error=function(e) e)

if(inherits(possibleError, "error")) next

#separate mega df into smaller dfs per climate scenario (1 row per estuary)

ssp126.2011.2040[[i]] <- pct.change.estuary[1,]
ssp370.2011.2040[[i]] <- pct.change.estuary[2,]
ssp585.2011.2040[[i]] <- pct.change.estuary[3,]
ssp126.2041.2070[[i]] <- pct.change.estuary[4,]
ssp370.2041.2070[[i]] <- pct.change.estuary[5,]
ssp585.2041.2070[[i]] <- pct.change.estuary[6,]
ssp126.2071.2100[[i]] <- pct.change.estuary[7,]
ssp370.2071.2100[[i]] <- pct.change.estuary[8,]
ssp585.2071.2100[[i]] <- pct.change.estuary[9,]

print (i)

}

ssp126.2011.2040 <- do.call(rbind, ssp126.2011.2040)
ssp370.2011.2040 <- do.call(rbind, ssp370.2011.2040)
ssp585.2011.2040 <- do.call(rbind, ssp585.2011.2040)
ssp126.2041.2070 <- do.call(rbind, ssp126.2041.2070)
ssp370.2041.2070 <- do.call(rbind, ssp370.2041.2070)
ssp585.2041.2070 <- do.call(rbind, ssp585.2041.2070)
ssp126.2071.2100 <- do.call(rbind, ssp126.2071.2100)
ssp370.2071.2100 <- do.call(rbind, ssp370.2071.2100)
ssp585.2071.2100 <- do.call(rbind, ssp585.2071.2100)

saveRDS(ssp126.2011.2040, "Data/Results/ssp126.2011.2040.rds")
saveRDS(ssp370.2011.2040, "Data/Results/ssp370.2011.2040.rds")
saveRDS(ssp585.2011.2040, "Data/Results/ssp585.2011.2040.rds")
saveRDS(ssp126.2041.2070, "Data/Results/ssp126.2041.2070.rds")
saveRDS(ssp370.2041.2070, "Data/Results/ssp370.2041.2070.rds")
saveRDS(ssp585.2041.2070, "Data/Results/ssp585.2041.2070.rds")
saveRDS(ssp126.2071.2100, "Data/Results/ssp126.2071.2100.rds")
saveRDS(ssp370.2071.2100, "Data/Results/ssp370.2071.2100.rds")
saveRDS(ssp585.2071.2100, "Data/Results/ssp585.2071.2100.rds")

 