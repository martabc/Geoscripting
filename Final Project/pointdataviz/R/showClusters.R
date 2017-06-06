## Author: Marta Blanco Castano
#' Second function - Clustering of Point Data
#'
#' This function allows you to visualize an input point data file (as a CSV)
#' on top of a basemap, as point clusters. As one zooms in, specific data
#' points are revealed with their particular locations.
#' You may additionally select a field to use
#' for popups on the points, so the map becomes interactive. NOTE: the fields
#' for latitude and longitude (to plot the points) must be labeled as "lat" and
#' "lon" in your input CSV file (without the quotation marks)
#' @param dataset Path to CSV file used as input
#' @param field Optional field that will enable informational popups upon hover
#' (note that if no field is desired as input, an empty set of quotations must
#' be specified (e.g., " "))
#' @keywords point clustering, data visualization
#' @export
#' @examples
#' clusters()


library(rMaps)
library(leaflet)
library(leaflet.extras)
library(magrittr)
library(sp)


clusters <- function(dataset, field)
{
  ct <- read.csv(dataset, sep = ",")

  map <- leaflet(ct) %>% addTiles('http://korona.geog.uni-heidelberg.de/tiles/roads/x={x}&y={y}&z={z}', attribution = 'OpenStreetMap')

  meanX <- mean(ct$lat)
  meanY <- mean(ct$lon)

  map %>% setView(meanX, meanY, zoom = 10)

  leaflet(ct) %>% addTiles('http://korona.geog.uni-heidelberg.de/tiles/roads/x={x}&y={y}&z={z}', attribution = 'OpenStreetMap') %>%
    addMarkers(clusterOptions = markerClusterOptions(), label = ~as.character(STORE_NAME))
}


clusters(dataset = "Final Project/heatmapViz/data/food.csv", field = )  ## not working with field parameter yet...
