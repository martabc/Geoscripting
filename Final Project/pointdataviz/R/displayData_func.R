## Author: Marta Blanco Castano
#' First function - Basic Point Data Visualization
#'
#' This function allows you to visualize an input point data file (as a CSV)
#' on top of a basemap. You may additionally select a field to use
#' for popups on the points, so the map becomes interactive. NOTE: the fields
#' for latitude and longitude (to plot the points) must be labeled as "lat" and
#' "lon" in your input CSV file (without the quotation marks)
#' @param dataset Path to CSV file used as input
#' @param field Optional field that will enable informational popups upon hover
#' (note that if no field is desired as input, an empty set of quotations must
#' be specified (e.g., " "))
#' @keywords point data visualization
#' @export
#' @examples
#' displayData()
#'
#' ## FYI: if an error message comes up saying there is no current rMaps version,
## one may still install the library via the commands:
## require(devtools)
## install_github('ramnathv/rCharts@dev')
## install_github('ramnathv/rMaps')
## devtools::install_github('rstudio/leaflet')
## devtools::install_github('bhaskarvk/leaflet.extras')


library(rMaps)
library(leaflet.extras)
library(sp)


displayData <- function(dataset, field){

  # ct <- "Final Project/heatmapViz/data/heliports.csv"

  ct <- read.csv(dataset, sep = ",")

  map <- leaflet(ct) %>% addTiles('http://korona.geog.uni-heidelberg.de/tiles/roads/x={x}&y={y}&z={z}', attribution = 'OpenStreetMap')

  meanX <- mean(ct$lat)
  meanY <- mean(ct$lon)

  map %>% setView(meanX, meanY, zoom = 10)

  # test <- noquote(field)

  map %>% addCircles(~lon, ~lat, popup=ct$type,  label = ~as.character(STORE_NAME), weight = 5, radius=80,
      color="#EF06A8", fillColor = 'black', stroke = TRUE, fillOpacity = 1,
      opacity = 0.5) %>% addLegend("bottomright", colors= "#EF06A8", labels="Points")
}


# displayData(dataset = "Final Project/heatmapViz/data/food.csv", field = )   ## not working with field parameter yet...

