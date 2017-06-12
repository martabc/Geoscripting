#' Basic Point Data Visualization
#'
#' This function allows you to visualize an input point data file (as a CSV) on top of a basemap. You may additionally select a field to use for popups on the points, so the map becomes interactive. NOTE: the fields for latitude and longitude (to plot the points) must be labeled as "lat" and "lon" in your input CSV file (without the quotation marks)
#' @param dataset Path to CSV file used as input
#' @param field Optional field that will enable informational popups upon hover (note that if no field is desired as input, an empty set of quotations must be specified (e.g., " "))
#' @keywords point data visualization
#' @examples
#' displayData(dataset = "../data/food.csv", field = STORE_NAME)
#' @return A basic map showing the points based on the coordinates from the CSV file.
#' @export
displayData <- function(dataset, field)
{
  ## Load necessary libraries (just in case!)
  library(leaflet)
  library(leaflet.extras)
  library(rMaps)
  library(sp)
  library(magrittr)
  library(ggmap)
  library(dplyr)

  ## Load file from function call (path, separated by a comma because it should be a CSV file)
  ct <- read.csv(dataset, sep = ",")

  ## Add leaflet tiles (basemap)
  map <- leaflet(ct) %>% addTiles('http://korona.geog.uni-heidelberg.de/tiles/roads/x={x}&y={y}&z={z}', attribution = 'OpenStreetMap')

  ## Calculate mean for x and y, so we know where to center the map in the next step
  meanX <- mean(ct$lat)
  meanY <- mean(ct$lon)

  ## Center map and set zoom based on lat/long center means (from previous step)
  map %>% setView(meanX, meanY, zoom = 10)

  ## Load point data from file based on lat/long columns, set specific labels for popups, and customize colors, opacity, etc.
  ## Add a legend as well at the bottom-right
  map %>% addCircles(~lon, ~lat, popup=ct$type,  label = ~as.character(STORE_NAME), weight = 5, radius=80,
                     color="#EF06A8", fillColor = 'black', stroke = TRUE, fillOpacity = 1,
                     opacity = 0.5) %>% addLegend("bottomright", colors= "#EF06A8", labels="Points")
}


# displayData(dataset = "data/food.csv", field = )   ## not working with field parameter yet...But will be fixed in the future! :)
