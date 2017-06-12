#' Heatmap for Basic Trend Visualization
#'
#' This function allows you to visualize an input point data file (as a CSV) on top of a basemap, in the form of a basic heatmap
#' @param dataset Path to CSV file used as input
#' @param location Location, in quotations, of the geographical area in question
#' @keywords heatmap, data visualization
#' @examples
#' heatmapTrend(dataset = "../data/food.csv", field = STORE_NAME)
#' @return A heatmap showing the trend in point data distribution
#' @export
heatmapTrend <- function(dataset, location)
{
  library(leaflet)
  library(leaflet.extras)
  library(rMaps)
  library(sp)
  library(magrittr)
  library(ggmap)
  library(dplyr)

  # load the data (read from csv)
  ct <- read.csv(dataset, sep = ",")

  # Package source URL: http://cran.r-project.org/web/packages/ggmap/ggmap.pdf
  # Data source URL: http://www.geo.ut.ee/aasa/LOOM02331/heatmap_in_R.html

  # Download the basemap
  map <- get_map(location = location, zoom = 11)
  
  # Draw the heat map, with all customizeable parameters (size of heatmap point radius, etc.)
  ggmap(map, extent = "device") + geom_density2d(data = ct, aes(x = lon, y = lat), size = 0.3) +
    stat_density2d(data = ct,
                   aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), size = 0.9,
                   bins = 8, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") +
    scale_alpha(range = c(0, 0.9), guide = FALSE)
}


# heatmapTrend(dataset = "data/food.csv", location = 'Denver')
