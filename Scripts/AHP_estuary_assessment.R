#AHP process

library("dplyr")

setwd("C:/Users/rekha/OneDrive - University of Victoria/Wetlands")
options(scipen = 999) #remove scientific notation

#load in matrices created from survey

clim <- read.csv("Ranking/AHP/climate_preference_matrix.csv")
clim.layers <- clim$X
clim <- as.matrix(clim[,c(2:7)])

data <- read.csv("Ranking/AHP/data_preference_matrix.csv")
data.layers <- data$X
data <- as.matrix(data[,c(2:5)])

#calculate principal eigenvector (priority vector) for matrix 

clim.e <- eigen(clim)
data.e <- eigen(data)

#normalize principal eigenvectors

clim.w <- as.data.frame(Re(clim.e$vectors[,1])) #principal eigenvector/priority vector is the first eigenvector, held in column 1
clim.w$norm <- clim.w[,1]/sum(clim.w[,1]) #normalize result to obtain weights that sum to 1

data.w <- as.data.frame(Re(data.e$vectors[,1])) 
data.w$norm <- data.w[,1]/sum(data.w[,1]) 

clim.w$layer <- clim.layers
clim.w <- clim.w[,c(3,1,2)]
names(clim.w)[2] <- "prin.eigenvector"

data.w$layer <- data.layers
data.w <- data.w[,c(3,1,2)]
names(data.w)[2] <- "prin.eigenvector"

#save matrices of weights

write.csv(clim.w, "Ranking/AHP/climate_data_weights.csv")
write.csv(data.w, "Ranking/AHP/data_weights.csv")





