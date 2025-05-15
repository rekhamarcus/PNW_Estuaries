#testing data

library(dplyr)
library(terra)
library(sf)
library(rgee)

setwd('C:/Users/rekhamarcus/OneDrive - University of Victoria/Wetlands/')

#read in estuaries shapefile

pnw <- readRDS('Data/PNW_shapefile.rds')

#reproject to be able to crop data
pnw <- st_transform(pnw, crs = st_crs("EPSG:4326"))

#CEDA data----------------------------------------------------------------
rast <- rast('Downloads/Global_pr_Downscaled_ACCESS-CM2_1981-2014_compressed.nc')

#subset bc this dataset is huge
rast.1 <- rast[[1]]
rast.1 <- rotate(rast.1) #rotate latlong projection
rast.1.pnw <- crop(rast.1, pnw)
rast.1.pnw <- mask(rast.1.pnw, pnw)

#data is very low resolution

#earth engine data-------------------------------------------------------

#required steps for rgee setup

# Get the username
HOME <- Sys.getenv("HOME")

# 1. Install miniconda
reticulate::install_miniconda()

# 2. Install Google Cloud SDK
system("curl -sSL https://sdk.cloud.google.com | bash")

# 3 Set global parameters
Sys.setenv("RETICULATE_PYTHON" = sprintf("%s/.local/share/r-miniconda/bin/python3", HOME))
Sys.setenv("EARTHENGINE_GCLOUD" = sprintf("%s/google-cloud-sdk/bin/", HOME))

# 4 Install rgee Python dependencies
ee_install() 

Y# 5. Authenticate and initialize your Earth Engine session
# Replace "my-project-id" with the ID of the Cloud project you created above 
ee_Initialize(project = "my-project-id") 

# Set your Python ENV
Sys.setenv("RETICULATE_PYTHON" = "Downloads/python-3.13.3-amd64")

# Set Google Cloud SDK. Only need it the first time you log in. 
Sys.setenv("EARTHENGINE_GCLOUD" = "Dow")
ee_Authenticate()

# Initialize your Earth Engine Session 
ee_Initialize(project = "my-project-id")

# move geometry from local to earth engine
ee_pnw <- sf_as_ee(pnw)

# 2. Load your ImageCollection
data <- ee$ImageCollection("ECMWF/ERA5/MONTHLY")$
  filterDate("1980-01-01","1981-01-01")$
  map(function(img) img$select("mean_2m_air_temperature"))


#CHELSA data ------------------------------------------------------------

#read in files
world.precip <- list.files(path = 'Data/Precipitation/CHELSA/',
                      pattern = '.tif', full.names = TRUE)

pnw.precip <- list()

#use a for loop to rasterize, crop, and make a data frame from each file
for(i in 1:length(world.precip)){
  r <- rast(world.precip[i])
  r <- project(r, "EPSG:4326")
  c <- crop(r, pnw)
  c <- mask(c, pnw)
  
  names(c) <- "pr"
  
  dat <- as.data.frame(c, xy = TRUE)
  
    dat$mm <- substr(world.precip[i],
                               start = nchar(world.precip[i]) - nchar('mm_yyyy_V.2.1.tif') + 1,
                               stop = nchar(world.precip[i]) - nchar('_yyyy_V.2.1.tif'))
    
    dat$yyyy <- substr(world.precip[i],
                     start = nchar(world.precip[i]) - nchar('yyyy_V.2.1.tif') + 1,
                     stop = nchar(world.precip[i]) - nchar('_V.2.1.tif'))
    
    pnw.precip[[i]] <- dat
    
    print(i) #indicate when each layer has been completed
    
  }

pnw.pr <- do.call(rbind, pnw.precip)


