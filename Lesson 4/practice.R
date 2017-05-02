library(raster)

## Generate a RasterLayer object
r <- raster(ncol=40, nrow=20)
class(r) 

# Using the previously generated RasterLayer object
# Let's first put some values in the cells of the layer
r[] <- rnorm(n=ncell(r))
# Create a RasterStack object with 3 layers
s <- stack(x=c(r, r*2, r))
# The exact same procedure works for creating a RasterBrick
b <- brick(x=c(r, r*2, r))
# Let's look at the properties of of one of these two objects
b

getwd()  ## Might need to set the path to the 'data' subfolder
download.file(url = 'https://raw.githubusercontent.com/GeoScripting-WUR/IntroToRaster/gh-pages/data/gewata.zip', destfile = 'gewata.zip', method = 'auto')
# In case the download code doesn't work, use method = 'wget'
## Unpack the archive
unzip('gewata.zip')


# ## HOW TO COPY FILES to subfolder so they can be accessed
# from.dir <- "/Users/RLearner/Desktop/RDMS"
# to.dir   <- "/Users/RLearner/Desktop/Test"
# files    <- list.files(path = from.dir, full.names = TRUE, recursive = TRUE)
# for (f in files) file.copy(from = f, to = to.dir)



# When passed without arguments, list.files() returns a character vector, listing the content of the working directory
list.files()
# To get only the files with .tif extension
list.files(pattern = glob2rx('*.tif'))
# Or if you are familiar with regular expressions
list.files(pattern = '^.*\\.tif$')

gewata <- brick('LE71700552001036SGS00_SR_Gewata_INT1U.tif')
gewata   ## 3 layers

gewataB1 <- raster('LE71700552001036SGS00_SR_Gewata_INT1U.tif')
gewataB1   ## 1 layer

plot(gewata, 1)  ## plot the 1st layer of brick

e <- drawExtent(show=TRUE)

## Crop gewata using e
gewataSub <- crop(gewata, e)
## Now visualize the new cropped object
plot(gewataSub, 1)

######################## NDVI part
# Again, make sure that your working directory is properly set
getwd()
## Download the data
download.file(url='https://raw.githubusercontent.com/GeoScripting-WUR/IntroToRaster/gh-pages/data/tura.zip', destfile='tura.zip', method='auto')
unzip(zipfile='tura.zip')
## Retrieve the content of the tura sub-directory
list <- list.files(path='tura/', full.names=TRUE)

plot(raster(list[1]))  ## Plots the very first raster layer (index = 1)

turaStack <- stack(list)
turaStack

# Write this file at the root of the working directory
writeRaster(x=turaStack, filename='turaStack.grd', datatype='INT2S')

ndvi <- (gewata[[4]] - gewata[[3]]) / (gewata[[4]] + gewata[[3]])

plot(ndvi)

## Define the function to calculate NDVI from 
ndvCalc <- function(x) {
  ndvi <- (x[[4]] - x[[3]]) / (x[[4]] + x[[3]])
  return(ndvi)
}
## Method using the "calc" function
ndvi2 <- calc(x=gewata, fun=ndvCalc)

ndvOver <- function(x, y) {
  ndvi <- (y - x) / (x + y)
  return(ndvi)
}
## Method using the "overlay" function
ndvi3 <- overlay(x=gewata[[3]], y=gewata[[4]], fun=ndvOver)

all.equal(ndvi, ndvi2) ## they all return the same results

all.equal(ndvi, ndvi3) ## same results


######################################
## One single line is sufficient to project any raster to any projection
ndviLL <- projectRaster(ndvi, crs='+proj=longlat')

# Since this function will write a file to your working directory
# you want to make sure that it is set where you want the file to be written
# It can be changed using setwd()
getwd()
# Note that we are using the filename argument, contained in the ellipsis (...) of 
# the function, since we want to write the output directly to file.
KML(x=ndviLL, filename='gewataNDVI.kml')


######################################################
## Download the data
download.file(url='https://raw.githubusercontent.com/GeoScripting-WUR/IntroToRaster/gh-pages/data/tahiti.zip', destfile='tahiti.zip', method='auto')
unzip(zipfile='tahiti.zip')

## Load the data as a RasterBrick object and investigate its content
tahiti <- brick('LE70530722000126_sub.grd')
tahiti

## Display names of each individual layer
names(tahiti)

plotRGB(tahiti, 3,4,5)  ## plotRGB outputs the entire raster file
plot(tahiti, 7)  ## this command only shows us layer/band 7 (cloud mask)

## Extract cloud layer from the brick
cloud <- tahiti[[7]]

## Replace 'clear land' with 'NA'
cloud[cloud == 0] <- NA

## Plot the stack and the cloud mask on top of each other
plotRGB(tahiti, 3,4,5)
plot(cloud, add = TRUE, legend = FALSE)

## Extract cloud mask RasterLayer
fmask <- tahiti[[7]]

## Remove fmask layer from the Landsat stack
tahiti6 <- dropLayer(tahiti, 7)

## Perform value replacement
tahiti6[fmask != 0] <- NA  ## we focus on clear land pixels (visible)

## First define a value replacement function
cloud2NA <- function(x, y){
  x[y != 0] <- NA
  return(x)
}

# Let's create a new 6 layers object since tahiti6 has been masked already
tahiti6_2 <- dropLayer(tahiti, 7)

## Apply the function on the two raster objects using overlay
tahitiCloudFree <- overlay(x = tahiti6_2, y = fmask, fun = cloud2NA)

## Visualize the output
plotRGB(tahitiCloudFree, 3,4,5)

## Visualize the data
plotRGB(tahiti, 3,4,5)
