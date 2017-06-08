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
  # ct <- read.csv('data/food.csv', sep = ",")

  ct <- read.csv(dataset, sep = ",")

  map <- leaflet(ct) %>% addTiles('http://korona.geog.uni-heidelberg.de/tiles/roads/x={x}&y={y}&z={z}', attribution = 'OpenStreetMap')

  meanX <- mean(ct$lat)
  meanY <- mean(ct$lon)

  map %>% setView(meanX, meanY, zoom = 10)

  map %>% addCircles(~lon, ~lat, popup=ct$type,  label = ~as.character(STORE_NAME), weight = 5, radius=80,
                     color="#EF06A8", fillColor = 'black', stroke = TRUE, fillOpacity = 1,
                     opacity = 0.5) %>% addLegend("bottomright", colors= "#EF06A8", labels="Points")
}


# displayData(dataset = "data/food.csv", field = )   ## not working with field parameter yet...
