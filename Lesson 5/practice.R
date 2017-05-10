## Load the sp and rgdal packages
library(sp)
library(rgdal)

## Coordinates of two points identified in Google Earth, for example:
pnt1_xy <- cbind(5.6660, 51.9872)   # enter your own coordinates
pnt2_xy <- cbind(5.6643, 51.9668)   # enter your own coordinates

## Combine coordinates in single matrix
coords <- rbind(pnt1_xy, pnt2_xy)

## Make spatial points object
prj_string_WGS <- CRS("+proj=longlat +datum=WGS84")
mypoints <- SpatialPoints(coords, proj4string=prj_string_WGS)

## Inspect object
class(mypoints)
str(mypoints)

## Create and display some attribute data and store in a data frame
mydata <- data.frame(cbind(id = c(1,2), 
                           Name = c("my first point", 
                                    "my second point")))

## Make spatial points data frame
mypointsdf <- SpatialPointsDataFrame(
  coords, data = mydata, 
  proj4string=prj_string_WGS)


class(mypointsdf) # Inspect and plot object
names(mypointsdf)
str(mypointsdf)

spplot(mypointsdf, zcol="Name", col.regions = c("red", "blue"), 
       xlim = bbox(mypointsdf)[1, ]+c(-0.01,0.01), 
       ylim = bbox(mypointsdf)[2, ]+c(-0.01,0.01),
       scales= list(draw = TRUE))

## Play with the spplot function!
## What is needed to make the following work?
# spplot(mypointsdf, col.regions = c(1,2))


## LINES EXAMPLES:
## Consult help on SpatialLines class
(simple_line <- Line(coords))

(lines_obj <- Lines(list(simple_line), "1"))

(spatlines <- SpatialLines(list(lines_obj), proj4string=prj_string_WGS))

(line_data <- data.frame(Name = "straight line", row.names="1"))

(mylinesdf <- SpatialLinesDataFrame(spatlines, line_data))

class(mylinesdf)
str(mylinesdf)

spplot(mylinesdf, col.regions = "blue", 
       xlim = bbox(mypointsdf)[1, ]+c(-0.01,0.01), 
       ylim = bbox(mypointsdf)[2, ]+c(-0.01,0.01),
       scales= list(draw = TRUE))



## READING AND WRITING VECTOR DATA WITH OGR

library(rgdal)

## Write to KML; below we assume a subdirectory data within the current 
#  working directory.
# dir.create("data", showWarnings = FALSE)
writeOGR(mypointsdf, file.path("data","mypointsGE.kml"), 
         "mypointsGE", driver="KML", overwrite_layer=TRUE)
writeOGR(mylinesdf, file.path("data","mylinesGE.kml"), 
         "mylinesGE", driver="KML", overwrite_layer=TRUE)

dsn = file.path("data","route.kml")
ogrListLayers(dsn) # To find out what the layers are
myroute <- readOGR(dsn, layer = ogrListLayers(dsn))

## Put both in single data frame
proj4string(myroute) <- prj_string_WGS

names(myroute)

myroute$Description <- NULL # delete Description
# mylinesdf <- rbind(mylinesdf, myroute)
# Note: some problems were reported with this step
mylinesdf <-  rbind.SpatialLines(mylinesdf, myroute) ## combine previously 
## created line from code with manually digitized line/path based on blocks
plot(mylinesdf)

## Define CRS object for RD projection
prj_string_RD <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.2369,50.0087,465.658,-0.406857330322398,0.350732676542563,-1.8703473836068,4.0812 +units=m +no_defs")

## Perform the coordinate transformation from WGS84 to RD
mylinesRD <- spTransform(mylinesdf, prj_string_RD)

plot(mylinesRD, col = c("blue", "red", 'red')) ## mine needs three inputs;
## there seems to be a repeated value
box()


## Use rgeos for computing the length of lines 
library(rgeos)

(mylinesdf$length <- gLength(mylinesRD, byid=T)) ## using double parenthesis 
## allows the statement to print to the console (rather than jst run w/o output)

mylinesdf@data
# or
data.frame(mylinesdf)


## POLYGON EXAMPLES:
## Perform the coordinate transformation from WGS84 (i.e. not a projection) to RD (projected)"
#  This step is necessary to be able to measure objectives in 2D (e.g. meters)
(mypointsRD <- spTransform(mypointsdf, prj_string_RD))

pnt1_rd <- coordinates(mypointsRD)[1,]
pnt2_rd <- coordinates(mypointsRD)[2,]

## Make circles around points, with radius equal to distance between points
## Define a series of angles going from 0 to 2pi
ang <- pi*0:200/100
circle1x <- pnt1_rd[1] + cos(ang) * mylinesdf$length[1]
circle1y <- pnt1_rd[2] + sin(ang) * mylinesdf$length[1]
circle2x <- pnt2_rd[1] + cos(ang) * mylinesdf$length[1]
circle2y <- pnt2_rd[2] + sin(ang) * mylinesdf$length[1] 
c1 <- cbind(circle1x, circle1y)
c2 <- cbind(circle2x, circle2y)

plot(c2, pch = 19, cex = 0.2, col = "red", ylim = range(circle1y, circle2y))
points(c1, pch = 19, cex = 0.2, col = "blue")  ## using points vs. plot ensures
## output is included in same plot, rather than a new one
points(mypointsRD, pch = 3, col= "darkgreen")

## Iterate through some steps to create SpatialPolygonsDataFrame object
## with both circles
circle1 <- Polygons(list(Polygon(cbind(circle1x, circle1y))),"1")
circle2 <- Polygons(list(Polygon(cbind(circle2x, circle2y))),"2")
spcircles <- SpatialPolygons(list(circle1, circle2), proj4string=prj_string_RD)
circledat <- data.frame(mypointsRD@data, row.names=c("1", "2"))
circlesdf <- SpatialPolygonsDataFrame(spcircles, circledat)

plot(circlesdf, col = c("gray60", "gray30"))
plot(mypointsRD, add = TRUE, col="red", pch=19, cex=1)
plot(mylinesRD, add = TRUE, col = c("green", "yellow", "yellow"), lwd=1.5)
box() ## add a box frame around the plot output

spplot(circlesdf, zcol="Name", col.regions=c("gray60", "gray30"), 
       sp.layout=list(list("sp.points", mypointsRD, col= c("red", 'green'), pch=19, cex=2), 
                      list("sp.lines", mylinesRD, lwd=1.5)))

## Expand the given geometry to include the area within the specified width 
buffpoint <- gBuffer(mypointsRD[1,], width=mylinesdf$length[1], quadsegs=2)
## quadseg = Number of line segments to use to approximate a quarter circle
## best to use whole numbers, as decimals will be ignored
mydiff <- gDifference(circlesdf[1,], buffpoint)

plot(circlesdf[1,], col = "red")
plot(buffpoint, add = TRUE, lty = 3, lwd = 2, col = "blue")

gArea(mydiff) ## what is the area of the difference?

plot(mydiff, col = "red")  ## plot area difference in red

myintersection <- gIntersection(circlesdf[1,], buffpoint)

plot(myintersection, col="blue") ## plot intersection blue

gArea(myintersection)

print(paste("The difference in area =", round(100 * gArea(mydiff) / 
                                                gArea(myintersection),2), "%"))
