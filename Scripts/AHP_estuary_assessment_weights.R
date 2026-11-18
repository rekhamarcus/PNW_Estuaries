##'File name: AHP_estuary_assessment_weights
##'Author: Rekha Marcus
##'Thesis: Chapter 2
##'
##'This script takes the input priority matrices created via the Analytical Hierarchy
##'Process and calculates the normalized principal eigenvectors of each matrix. 
##'The normalized principal eigenvectors are then used as weights for the multi-criteria
##'decision models. This script is written to generate the weights for one model - 
##'for each model, this script must be re-run each time with the corresponding preference
##'matrices for each model.

#load packages ----------------------------------------------------------------

library("dplyr")

options(scipen = 999) #remove scientific notation

#create matrices --------------------------------------------------------------

#load in matrices created from survey

clim <- read.csv("AHP/waterfowl/climate_preference_matrix.csv")
clim.layers <- clim$X
clim <- as.matrix(clim[,c(2:7)])

data <- read.csv("AHP/waterfowl/data_preference_matrix.csv")
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

#save matrices of weights - these can also be directly input into the model spreadsheets

#write.csv(clim.w, "Ranking/AHP/climate_data_weights.csv")
#write.csv(data.w, "Ranking/AHP/data_weights.csv")
