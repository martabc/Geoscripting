## AUTHOR: Marta Blanco
## DATE: May 1, 2017
## Project: Lesson 5

## This exercise takes two input tar files and processes them so that
## an output image and KML files are obtained. It aims at comparing
## the two images from Landsat (for different years), to compare basic
## changes in vegetation. This is attainable by implementing the famous
## NDVI function using NIR and red bands (NIR - Red/ NIR + Red):


## Load raster library
library(raster)

## Set working environment (where to save/retrieve data)
setwd("C:/Users/MARTA/Desktop/Geoscripting/GitHub/Lesson 4/data/")

# list.files() returns the content of the working directory
list.files()

## Unzip landsat 8 files into its own folder, called "land-8"
untar("LC81970242014109-SC20141230042441.tar.gz", exdir = 'land-8')

## Same with landsat 5 files - unzip into "land-5" folder
untar("LT51980241990098-SC20150107121947.tar.gz", exdir = "land-5")

# To get only the files with .tif extension from both folders
list8 <- list.files("land-8/", pattern = glob2rx('*.tif'), full.names = T)
list5 <- list.files("land-5/", pattern = glob2rx('*.tif'), full.names = T)

## Test that the lists have been processed properly, by plotting one band each
plot(raster(list8[1]))  ## Plots the first raster layer (index = 1) for land8
plot(raster(list5[1])) ## repeat test plotting with band 1 of land8

## Turn both sets of files into raster stacks for processing
landStack8 <- stack(list8)
landStack5 <- stack(list5)

# Write both files into .grd formats
writeRaster(x=landStack5, filename='landstack-5.grd', datatype='INT2S')
writeRaster(x=landStack8, filename='landstack-8.grd', datatype='INT2S')

## Define simple function to calculate NDVI on a given file
## This one works for Landsat 5 imagery because NIR = band 4 and Red = band 3
ndvCalc5 <- function(x) {
  ndvi <- (x[[4]] - x[[3]]) / (x[[4]] + x[[3]])
  return(ndvi)
}

## This one is for Landsat 8, due to slight band differences (NIR = 5, Red = 4)
ndvCalc8 <- function(x) {
  ndvi <- (x[[5]] - x[[4]]) / (x[[5]] + x[[4]])
  return(ndvi)
}

## Apply for functions to the stack datasets to save results onto variables
ndvi5 <- calc(x=landStack5, fun=ndvCalc5)
ndvi8 <- calc(x=landStack8, fun=ndvCalc8)

## view ndvi results for both imagery variables
par(mfcol = c(1, 2)) ## prepare the plotting env. for 1 row, 2 columns
plot(ndvi5)
title(main = "Landsat 5, 1990", col="black", cex = 1.0)
plot(ndvi8)
title(main = "Landsat 8, 2014", col="black", cex = 1.0)

## Project to Lat/Long so we can export to kml format and view on Google Earth
nvdi5Proj <- projectRaster(ndvi5, crs='+proj=longlat')
nvdi8Proj <- projectRaster(ndvi8, crs='+proj=longlat')

## Export projected datasets to KML format (also outputs png versions)
KML(x=nvdi5Proj, filename='../output/land5NDVI.kml')
KML(x=nvdi8Proj, filename='../output/land8NDVI.kml')
