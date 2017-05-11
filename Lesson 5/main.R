## AUTHOR: Marta Blanco
## DATE: May 8, 2017
## Project: Lesson 6

## In this exercise I query for industrial railways, perform a 1000m buffer,
## find intersections of cities in the buffer, create a summary plot, provide
## necessary information on the intersecting cities, and provide a well 
## documented script on GitHub

## Load required libraries for spatial analysis
library(rgdal)
library(sp)
library(rgeos)

## Load the RAILWAYS shapefile
loadRail <- file.path('C:\\Users\\MARTA\\Desktop\\Geoscripting\\GitHub\\Lesson 5\\data', 'railways')
rail <- readOGR(loadRail, layer = ogrListLayers(loadRail))

## Load the PLACES shapefile
dsnPl = file.path("C:\\Users\\MARTA\\Desktop\\Geoscripting\\GitHub\\Lesson 5\\data","places")
pl <- readOGR(dsnPl, layer = ogrListLayers(dsnPl))

## Create a variable with PCS for area of interest
prj <- CRS("+init=epsg:28992")

## Project both shapefiles to same PCS so they match
railProj <- spTransform(rail, prj)
plProj <- spTransform(pl, prj)

## query for railways of type = industrial
ind <- railProj[railProj$type == 'industrial',]

## Run buffer of 1000 meters on industrial railways
buff <- gBuffer(ind, width=1000, byid = T)

## Compute intersection of places and the industrial railway buffer
intsec <- gIntersection(buff, plProj, byid = T)
intsec  ## print result so index of city shows up
## City falling in buffer is "Utrecht"
## It has a population of 100,000 people

x <- intsec$x
y <- intsec$y
tot <- length(intsec)
name <- plProj$name[5973]
pop <- plProj$population[5973]

## Plot results - show cities (green) falling within buffer (red)
plot(buff, col = 'red', main= "Utrecht, population = 100,000")
plot(intsec, col="green", cex = 5, pch = 20, add = T)
legend('topright', legend=c("Buffer", "Cities Intersecting"),
       col=c("red", "green"), lty=1, cex=0.8)

# Print result of intersected cities within buffer
cat("Cities within 1km of industrial railways are =",tot,", (name = Utrecht)",
    "located at",x,"(lat),",y,'(long)',"and with a population of", pop)
