## Libraries
library(raster)

## Set working directory to data folder of Lesson 7
setwd("C://Users//MARTA//Desktop//Geoscripting//GitHub//Lesson 7")

## Load data
load("data/GewataB2.rda")
load("data/GewataB3.rda")
load("data/GewataB4.rda")

## Check out the attributes
GewataB2

## Some basic statistics using cellStats()
cellStats(GewataB2, stat=max)
cellStats(GewataB2, stat=mean)

# This is equivalent to:
maxValue(GewataB2)

## What is the maximum value of all three bands?
max(c(maxValue(GewataB2), maxValue(GewataB3), maxValue(GewataB4)))

## summary() is useful function for a quick overview
summary(GewataB2)

## Put the 3 bands into a RasterBrick object to summarize together
gewata <- brick(GewataB2, GewataB3, GewataB4)

# 3 histograms in one window (automatic, if a RasterBrick is supplied)
hist(gewata)

par(mfrow = c(1, 1)) # reset plotting window
hist(gewata, xlim = c(0, 5000), ylim = c(0, 750000), breaks = seq(0, 5000, by = 100))

## Raster algebra can be performed with the calc() method

## plot histograms and scatterplots based on random sample of raster pixels
pairs(gewata) 

## Calculate NDVI from rasterBrick
ndvi <- overlay(GewataB4, GewataB3, fun=function(x,y){(x-y)/(x+y)})
plot(ndvi)

## Load the additional VCF Landsat data and check it out
load("data/vcfGewata.rda")
vcfGewata
plot(vcfGewata)
summary(vcfGewata)
hist(vcfGewata)

## Assign NA to all values above 100 (the max), since those are not vegetation
vcfGewata[vcfGewata > 100] <- NA
plot(vcfGewata)
summary(vcfGewata)
hist(vcfGewata)

## Rescale reflectance values to original scales (from 0 to 1)
gewata <- calc(gewata, fun=function(x) x / 10000)

## Make a new RasterBrick of covariates by adding NDVI and VCF layers
covs <- addLayer(gewata, ndvi, vcfGewata)
plot(covs)

## Name the layers within the final combination layer
names(covs) <- c("band2", "band3", "band4", "NDVI", "VCF")
plot(covs)

## Load the training polygons
load("data/trainingPoly.rda")

## Superimpose training polygons onto NDVI plot
plot(ndvi)
plot(trainingPoly, add = TRUE)

## Inspect the data slot of the trainingPoly object
trainingPoly@data

# The 'Class' column is actually an ordered factor type
trainingPoly@data$Class
str(trainingPoly@data$Class)

# We can convert to integer by using the as.numeric() function, 
# which takes the factor levels
trainingPoly@data$Code <- as.numeric(trainingPoly@data$Class)
trainingPoly@data

## Assign 'Code' values to raster cells (where they overlap)
classes <- rasterize(trainingPoly, ndvi, field='Code')
plot(classes)

## Plotting
# Define a color scale for the classes (from above)
# corresponding to: cropland, forest, wetland
cols <- c("orange", "dark green", "light blue")

## Plot without a legend
plot(classes, col=cols, legend=FALSE)

## Add a customized legend
legend("topright", legend=c("cropland", "forest", "wetland"), 
       fill=cols, bg="white")

## Mask all layers/bands to the classes based on class codes (numbers)
covmasked <- mask(covs, classes)
plot(covmasked)

## Combine the new brick with the classes layer to make a training dataset
names(classes) <- "class"
trainingbrick <- addLayer(covmasked, classes)
plot(trainingbrick)

## Extract all values into a matrix
valuetable <- getValues(trainingbrick)

## Omit NA values
valuetable <- na.omit(valuetable)

## Convert these values to a data.frame, and inspect first and last 10 rows
valuetable <- as.data.frame(valuetable)
head(valuetable, n = 10)  ## See first 10 rows
tail(valuetable, n = 10)  ## See last 10 rows

## Convert the training dataset class column into a factor
valuetable$class <- factor(valuetable$class, levels = c(1:3))

## break the three defined classes into subsets, for visualization purposes
val_crop <- subset(valuetable, class == 1)
val_forest <- subset(valuetable, class == 2)
val_wetland <- subset(valuetable, class == 3)

## 1. Plot histograms based on NDVI values for each class
par(mfrow = c(3, 1))
hist(val_crop$NDVI, main = "cropland", xlab = "NDVI", xlim = c(0, 1), 
     ylim = c(0, 4000), col = "orange")
hist(val_forest$NDVI, main = "forest", xlab = "NDVI", xlim = c(0, 1), 
     ylim = c(0, 4000), col = "dark green")
hist(val_wetland$NDVI, main = "wetland", xlab = "NDVI", xlim = c(0, 1), 
     ylim = c(0, 4000), col = "light blue")

## 2. Plot histograms based on VCF values for each class
hist(val_crop$VCF, main = "cropland", xlab = "% tree cover", xlim = c(0, 100),
     ylim = c(0, 7500), col = "orange")
hist(val_forest$VCF, main = "forest", xlab = "% tree cover", xlim = c(0, 100), 
     ylim = c(0, 7500), col = "dark green")
hist(val_wetland$VCF, main = "wetland", xlab = "% tree cover", xlim = c(0, 100),
     ylim = c(0, 7500), col = "light blue")

## 3. Plot scatterplots based on Band 3 & Band 4 values for each class
plot(band4 ~ band3, data = val_crop, pch = ".", col = "orange", 
     xlim = c(0, 0.2), ylim = c(0, 0.5))
points(band4 ~ band3, data = val_forest, pch = ".", col = "dark green")
points(band4 ~ band3, data = val_wetland, pch = ".", col = "light blue")
legend("topright", legend=c("cropland", "forest", "wetland"), 
       fill=c("orange", "dark green", "light blue"), bg="white")

par(mfrow = c(1, 1)) # reset plotting window

## Construct a random forest model
# Covariates (x, or the actual cell values) are found in columns 1 to 5 of 
# valuetable. Training classes (y) are found in the 'class' column of valuetable
## Caution: this step takes fairly long!
# but can be shortened by setting importance=FALSE
library(randomForest)
## Save random forest model to variable, using all rows but only columns 1-5
modelRF <- randomForest(x=valuetable[ ,c(1:5)], y=valuetable$class,
                        importance = TRUE)

save(modelRF, file = "output/modelRF.rda")
# modelRF <- load(file = "output/modelRF.rda")

## Inspect the structure and element names of the resulting model
modelRF
class(modelRF)
str(modelRF)
names(modelRF)

## Inspect the confusion matrix of the OOB error assessment
modelRF$confusion

# to make the confusion matrix more readable
colnames(modelRF$confusion) <- c("cropland", "forest", "wetland", "class.error")
rownames(modelRF$confusion) <- c("cropland", "forest", "wetland")
modelRF$confusion

## Visualize statistical importance of covariates
varImpPlot(modelRF)

## Double-check layer and column names to make sure they match
names(covs)
names(valuetable)

## Predict land cover using the RF model,and save output to a new file
predLC <- predict(covs, model=modelRF, filename = 'output/predLC.rda', 
                  overwrite = T, na.rm=TRUE)
# writeRaster(predLC, filename = "output/predLC.rda")

## Plot the results
# recall: 1 = cropland, 2 = forest, 3 = wetland
par(mfrow = c(1, 1))
cols <- c("orange", "dark green", "light blue")
plot(predLC, col=cols, legend=FALSE)
legend("bottomright", 
       legend=c("cropland", "forest", "wetland"), 
       fill=cols, bg="white")



## UNSUPERVISED CLASSIFICATION - USING K-MEANS
valuetable <- getValues(covs)
head(valuetable)

## Construct a k-means object, setting clusters number to 3 (crop, wet, forest)
## Also omit NA values that don't contain useful information
km <- kmeans(na.omit(valuetable), centers = 3, iter.max = 100, nstart = 10)

# km contains the clusters (classes) assigned to the cells
head(km$cluster)

## Create a blank raster with default values of 0
rNA <- setValues(raster(covs), 0)

## Loop through layers of covs
## Assign a 1 to rNA wherever an NA is enountered in covs
for(i in 1:nlayers(covs)){
  rNA[is.na(covs[[i]])] <- 1
}

## Convert rNA to an integer vector
rNA <- getValues(rNA)
unique(km$cluster) # displays unique values

## Convert valuetable to a data.frame
valuetable <- as.data.frame(valuetable)

## If rNA is a 0, assign the cluster value at that position
valuetable$class[rNA==0] <- km$cluster

## If rNA is a 1, assign an NA at that position
valuetable$class[rNA==1] <- NA

## Create a blank raster
classes <- raster(covs)

## Assign values from the 'class' column of valuetable
classes <- setValues(classes, valuetable$class)
plot(classes, legend=FALSE, col=c("dark green", "orange", "light blue"))


## APPLYING A RASTER SIEVE BY CLUMPING

## Make an NA-value raster based on the LC raster attributes
formask <- setValues(raster(predLC), NA)

## Assign 1 to formask to all cells corresponding to the forest class
formask[predLC==2] <- 1

plot(formask, col="dark green", legend = FALSE)

## Group raster cells into clumps based on the Queen's Case
if(!file.exists(fn <- "data/clumformask.grd")) {
  forestclumps <- clump(formask, directions=8, filename=fn)
} else {
  forestclumps <- raster(fn)
}
plot(forestclumps)

## Assign freqency table to a matrix
clumpFreq <- freq(forestclumps)
head(clumpFreq)
tail(clumpFreq)

## Coerce freq table to data.frame
clumpFreq <- as.data.frame(clumpFreq)

## which rows of the data.frame are only represented by one cell?
str(which(clumpFreq$count==1))

## which values do these correspond to?
str(clumpFreq$value[which(clumpFreq$count==1)])

## Put these into a vector of clump ID's to be removed
excludeID <- clumpFreq$value[which(clumpFreq$count==1)]

## Make a new forest mask to be sieved
formaskSieve <- formask

## Assign NA to all clumps whose IDs are found in excludeID
formaskSieve[forestclumps %in% excludeID] <- NA

## Zoom in to a small extent to check the results
# Note: you can define your own zoom by using e <- drawExtent()
e <- extent(c(811744.8, 812764.3, 849997.8, 850920.3))
opar <- par(mfrow=c(1, 2)) # allow 2 plots side-by-side
plot(formask, ext=e, col="dark green", legend=FALSE)
plot(formaskSieve, ext=e, col="dark green", legend=FALSE)

par(opar)  ## reset plotting window


## WORKING WITH THEMATIC RASTERS
load("data/lulcGewata.rda")

## Check out the distribution of the values
freq(lulcGewata)
hist(lulcGewata)

## Load the data.frame file containing defining classes
load("data/LUTGewata.rda")
LUTGewata

lulc <- as.factor(lulcGewata)

# assign a raster attribute table (RAT)
levels(lulc) <- LUTGewata
lulc

## Layerize allows to visualize one raster class at a time
classes <- layerize(lulc)
# Layer names follow the order of classes in the LUT
names(classes) <- LUTGewata$Class
plot(classes, legend=FALSE)

## Construct a forest only layer to single it out
forest <- raster(classes, 5)
# is equivalent to:
forest <- classes[[5]]
# or (since the layers are named):
forest <- classes$forest

## Replace 0's (non-forest) with NA's
forest[forest==0] <- NA
plot(forest, col="dark green", legend=FALSE)
