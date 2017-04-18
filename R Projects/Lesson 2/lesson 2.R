# Author: Marta Blanco Castano

## Libraries needed
library(raster)
library(spatstat)
library(rgeos)
## You can choose your own country here
bel <- raster::getData('alt', country='BEL', mask=TRUE) ## SRTM 90m height data
belshp <- raster::getData('GADM', country='BEL', level=2) ## administrative boundaries
## Create random points
dran <- runifpoint(500, win = as.vector(extent(bel)))
## Make the random point spatial points
S <- SpatialPoints(data.frame(x = dran$x, y = dran$y), 
                   proj4string = CRS(proj4string(belshp)))
## Select only the ones within belgium
Sint <- gIntersection(S, belshp)
## Create a map
plot(bel)
plot(belshp, add=TRUE)
plot(Sint, add = TRUE, col = "red", pch = 19, cex = 0.2)

out <- extract(bel, Sint, df = TRUE)
colnames(out) <- c("id", "height")
head(out)

####
plot(out, type = "p", pch = 19)

download.file(url = 'https://raw.githubusercontent.com/loicdtx/bfastSpatial/master/data/tura.rda', destfile = 'tura.rda', method = 'auto')

load('tura.rda')

library(zoo)
## Define the function to extract and plot the time series
click2ts <- function(x) {
  val <- click(x, n = 1)
  z <- getZ(x)
  plot(zoo(t(val), z), type = 'p', pch = 20, xlab = 'Time', ylab = 'NDVI (-)')
}

plot(tura, 1)
click2ts(tura)

#####
library(zoo)
library(lubridate)
library(raster)

## Download data (if it doesn't work, try with method='wget')
download.file(url = 'https://raw.githubusercontent.com/GeoScripting-WUR/Scripting4Geo/gh-pages/data/MODIS_VCF_2000-2010_NL.rds', destfile = 'MODIS.rds', method = 'wget')
## Read the data
modis <- readRDS('data/MODIS.rds')
## Clean data (values > 100 correspond to water)
modis[modis > 100] <- NA
## Visualize
plot(modis, 1)

e <- extent(340101, 370323, 5756221, 5787772)
plot(e, add=TRUE)
modis_sub <- crop(modis, e)
plot(modis_sub, 1)

# Define function to calculate temporal trends
fun <- function(x) {
  ts <- zoo(x, time)
  df <- data.frame(t = decimal_date(index(ts)), vcf = c(ts))
  out <- try(lm(vcf ~ t, data = df)$coefficients[2], silent = T)
  if(class(out) == 'try-error')
    out <- NA
  return(out)
}

# Run the function spatially (this may take a few minutes, time for a break?)
time <- getZ(modis)
out <- calc(x = modis_sub, fun = fun)

# Visualize output
plot(out)
hist(out, main = 'Tree cover change at\n 250 m resolution (2000-2010)', xlab = 'Percentage change')


#####
library(leaflet)
m <- leaflet()
m <- addTiles(m)
m <- addMarkers(m, lng=5.665349, lat=51.987870, popup="Wageningen University")
m


library(googleVis)
library(XML)
library(httr)
# set googleVis options to change the behaviour of plot.gvis, 
# so that only the chart component of the HTML file is written into the # output file.
op <- options(gvis.plot.tag='chart')

# Read table from html
url <- "http://en.wikipedia.org/wiki/List_of_national_parks"
#x <- readHTMLTable(readLines(url), which=3, stringsAsFactors = FALSE)
page <- GET(url, user_agent("httr"))
x <- readHTMLTable(text_content(page), which=3, stringsAsFactors = FALSE)

# Clean up df 
colnames (x) <- c('country', 'oldest', 'number', 'area_tot', 'country_percentage')
x$oldest <- as.numeric(x$oldest)
x$number <- as.numeric(gsub("\\*", "", x$number))
x$area_tot <- as.numeric(gsub("(,)|(\\[.*\\])", "", x$area_tot))
x$country_percentage <- as.numeric(gsub("(%)|(\\[.*\\])", "", x$country_percentage))

nationalParks <- x

g <- gvisGeoChart(nationalParks, locationvar="country", colorvar = "oldest", sizevar = "number",
                  options=list(region="150", displayMode="markers", colorAxis="{colors: ['green', 'blue']}"))

plot(g)
