## Practice examples - Lesson 6 :

## crop(r1, r1) - crops r1 to the extent of r2
## drawExtent() - works to define an extent interactively (self-draw)

library(raster)
## Download, unzip and load the data
download.file(url = 'https://raw.githubusercontent.com/GeoScripting-WUR/VectorRaster/gh-pages/data/landsat8.zip', destfile = 'data/landsat8.zip', method = 'auto')

unzip('data/landsat8.zip')
## Identify the right file
landsatPath <- list.files(pattern = glob2rx('LC8*.grd'), full.names = TRUE)

wagLandsat <- brick(landsatPath)

wagLandsat[wagLandsat < 0] <- NA
plotRGB(wagLandsat, 5, 4, 3)

## Download municipality boundaries
nlCity <- raster::getData('GADM',country='NLD', level=2)
class(nlCity)

head(nlCity@data)

nlCity@data <- nlCity@data[!is.na(nlCity$NAME_2),] # Remove rows with NA
wagContour <- nlCity[nlCity$NAME_2 == 'Wageningen',]

## Load rgdal library (needed to reproject data)
library(rgdal)
wagContourUTM <- spTransform(wagContour, CRS(proj4string(wagLandsat)))

wagLandsatCrop <- crop(wagLandsat, wagContourUTM)
wagLandsatSub <- mask(wagLandsat, wagContourUTM)

## Set graphical parameters (one row and two columns)
opar <- par(mfrow=c(1,2))
plotRGB(wagLandsatCrop, 5, 4, 3, main = 'Crop()')
plotRGB(wagLandsatSub, 5, 4, 3, main = 'Mask()')
plot(wagContourUTM, add = TRUE, border = "green", lwd = 3)

## Reset graphical parameters
par(mfrow = c(1,1))

download.file(url = 'https://raw.githubusercontent.com/GeoScripting-WUR/VectorRaster/gh-pages/data/wageningenWater.zip', destfile = 'data/wageningenWater.zip', method = 'auto')
unzip('data/wageningenWater.zip')

## Check the names of the layers for input in readOGR()
ogrListLayers('Water.shp')
water <- readOGR('Water.shp', layer = 'Water')
waterUTM <- spTransform(water, CRS(proj4string(wagLandsat)))

wagLandsatSubW <- mask(wagLandsatSub, mask = waterUTM, inverse = TRUE)
plotRGB(wagLandsatSubW, 5, 4, 3)
plot(waterUTM, col = 'blue', add = TRUE, border = 'blue', lwd = 2)

samples <- readOGR('data/calibPts.kml', layer = 'calibPts')

## Re-project SpatialPointsDataFrame
samplesUTM <- spTransform(samples, CRS(proj4string(wagLandsatCrop)))

# The extract function does not understand why the object would have 3 coord columns, so we need to edit this field
samplesUTM@coords <- coordinates(samplesUTM)[,-3]

## Extract the surface reflectance 
calib <- extract(wagLandsatCrop, samplesUTM, df=TRUE) ## df=TRUE i.e. return as a data.frame

## Combine the newly created dataframe to the description column of the calibration dataset
calib2 <- cbind(samplesUTM$description, calib)

## Change the name of the first column, for convienience
colnames(calib)[1] <- 'lc'

## Inspect the structure of the dataframe
str(calib)

if(!require(randomForest)) {
  install.packages("randomForest")
}

library(randomForest)

## Calibrate model
model <- randomForest(lc ~ band1 + band2 + band3 + band4 + band5 + band6 + band7, data = calib)

## Use the model to predict land cover
lcMap <- predict(wagLandsatCrop, model = model)

library(rasterVis)

levelplot(lcMap, col.regions = c('green', 'brown', 'darkgreen', 'lightgreen', 'grey', 'blue'))


########

## Download data
bel <- getData('alt', country='BEL', mask=TRUE)

## Display metadata
bel

plot(bel)

line <- drawLine()

alt <- extract(bel, line, along = TRUE)

plot(alt[[1]], type = 'l', ylab = "Altitude (m)")

if(!require(geosphere)) {
  install.packages("geosphere")
}

library(geosphere)

## Calculate great circle distance between the two ends of the line
dist <- distHaversine(coordinates(line)[[1]][[1]][1,], coordinates(line)[[1]][[1]][2,])

## Format an array for use as x axis index with the same length as the alt[[1]] array
distanceVector <- seq(0, dist, along.with = alt[[1]])

## Visualize the output
plot(bel, main = 'Altitude (m)')
plot(line, add = TRUE)
plot(distanceVector/1000, alt[[1]], type = 'l',
     main = 'Altitude transect Belgium',
     xlab = 'Distance (Km)',
     ylab = 'Altitude (m)',
     las = 1)


##################

# You can choose your own country here
bel <- getData('alt', country='BEL', mask=TRUE) ## SRTM 90m height data
belshp <- getData('GADM', country='BEL', level=2) ## administrative boundaries

## Sample the raster randomly with 40 points
sRandomBel <- sampleRandom(bel, na.rm=TRUE, sp=TRUE, size = 40)

## Create a data.frame containing relevant info
sRandomData <- data.frame(altitude = sRandomBel@data[[1]],
                          latitude = sRandomBel@coords[,'y'],
                          longitude = sRandomBel@coords[,'x'])

## Plot
plot(bel)
plot(belshp, add=TRUE)
plot(sRandomBel, add = TRUE, col = "red")

## Plot altitude versus latitude
plot(sRandomData$latitude,sRandomData$altitude, ylab = "Altitude (m)", xlab = "Latitude (degrees)")

