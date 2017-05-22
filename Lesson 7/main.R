## Author: Marta Blanco
## Date:  May 21st, 2017
## Lesson 7 Exercise

library(raster)

## Set working directory to data folder of Lesson 7
setwd("C://Users//MARTA//Desktop//Geoscripting//GitHub//Lesson 7")

load("data/GewataB1.rda")
load("data/GewataB2.rda")
load("data/GewataB3.rda")
load("data/GewataB4.rda")
load("data/GewataB5.rda")
load("data/GewataB7.rda")
load('data/vcfGewata.rda')

## Build a brick containing all data
alldata <- brick(GewataB1, GewataB2, GewataB3, GewataB4, GewataB5, GewataB7, vcfGewata)
names(alldata) <- c("band1", "band2", "band3", "band4", "band5", "band7", "VCF")

par(mfrow = c(1,1))  ## reset plotting window

## Extract all data to a data.frame
df <- as.data.frame(getValues(alldata))

## Calculate NDVI from rasterBrick
ndvi <- overlay(GewataB4, GewataB3, fun=function(x,y){(x-y)/(x+y)})
plot(ndvi)

# pairs(alldata)

head(df)

## Run unsupervised k-means method for one cluster (forest)
km <- kmeans(na.omit(df), centers = 1, iter.max = 100, nstart = 10)

head(km$cluster)
unique(km$cluster)  ## only 1 seems unique

# ## Create a blank raster with default values of 0
# rNA <- setValues(raster(covs), 0)
# 
# ## Loop through layers of covs
# ## Assign a 1 to rNA wherever an NA is enountered in covs
# for(i in 1:nlayers(covs)){
#   rNA[is.na(covs[[i]])] <- 1
# }
# ## Convert rNA to an integer vector
# rNA <- getValues(rNA)
# 
# 
# ## Convert valuetable to a data.frame
# valuetable <- as.data.frame(valuetable)
# 
# ## If rNA is a 0, assign the cluster value at that position
# valuetable$class[rNA==0] <- km$cluster
# 
# ## If rNA is a 1, assign an NA at that position
# valuetable$class[rNA==1] <- NA

km
plot(km$cluster)

# model <- lm()
