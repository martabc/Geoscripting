## AUTHOR: Marta Blanco
## DATE: May 15, 2017
## Project: Lesson 7

## In this exercise I find which municipality in the Netherlands is the
## greenest. To do so I use MODIS NDVI data, and check from greenest
## between January, August, and a yearly average for all municipalities.
## I finish by plotting a map of January to see the results from then:

library(raster)

setwd("C:\\Users\\MARTA\\Desktop\\Geoscripting\\GitHub\\Lesson 6")

## Download, unzip and load the data
download.file(url = 'https://raw.githubusercontent.com/GeoScripting-WUR/VectorRaster/gh-pages/data/MODIS.zip', destfile = 'data/MODIS.zip', method = 'auto')

unzip('data/MODIS.zip')

## Identify the right file
nedPath <- list.files(pattern = glob2rx('MOD*.grd'), full.names = TRUE)

nedMOD <- brick(nedPath)

nedMOD[nedMOD < 0] <- NA
plotRGB(nedMOD, 1, 4, 3)  ## this should be true RGB for MODIS, I think?

## Download municipality boundaries
nlMunicipality <- getData('GADM',country='NLD', level=2)

plot(nlMunicipality)

# nlCity@data <- nlCity@data[!is.na(nlCity$NAME_2),] # Remove rows with NA
# nedJan <- nlMunicipality[nlMunicipality$names == 'January',]

## Load rgdal library (needed to reproject data)
library(rgdal)
nedUTM <- spTransform(nlMunicipality, CRS(proj4string(nedMOD)))

nedMODCrop <- crop(nedMOD, nedUTM)
nedSub <- mask(nedMODCrop, nedUTM)

plotRGB(nedSub, 1, 4, 3)  ## check that masking worked by plotting true color
plot(nedUTM, add = T) ## with municipalities shapefile on top (they match)


## JANUARY--------------------------------
nedJan <- nedSub$January  ## extract January data to a variable

par(mfrow = c(1, 2))

# plot(nedJan)
plot(nedUTM) ## with municipalities shapefile on top (they match)

greenJan <- nedSub$January > 9000
plot(greenJan)
# output <- extract(greenJan, nedUTM)  ## DOES NOT WORK
# mask <- mask(green, nedUTM) ## NOT WORKING EITHER - NOT SURE HOW TO NARROW 
## DOWN TO GREENEST MUNICIPALITY


## AUGUST------------------------------
nedAug <- nedSub$August  ## extract August data to a variable

par(mfrow = c(1, 2))

# plot(nedAug)
plot(nedUTM) ## with municipalities shapefile on top (they match)

greenAug <- nedSub$August > 9000
plot(greenAug)
# mask <- mask(green, nedUTM) ## NOT WORKING - NOT SURE HOW TO NARROW DOWN
## TO GREENEST MUNICIPALITY


## AVERAGE OVER THE YEAR------------------------------
## Can probably do with extract(), but need to figure out how...