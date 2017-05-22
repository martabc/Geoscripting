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


