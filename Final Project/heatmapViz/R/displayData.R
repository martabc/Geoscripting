## First function -

## Note: if an error message comes up saying there is no current rMaps version,
## one may still install the library via the commands:
## require(devtools)
## install_github('ramnathv/rCharts@dev')
## install_github('ramnathv/rMaps')
# devtools::install_github('rstudio/leaflet')
# devtools::install_github('bhaskarvk/leaflet.extras')

library(rMaps)
library(leaflet.extras)
library(magrittr)
library(sp)
library(V8)
library(geojsonio)

# L2 <- Leaflet$new()
# L2$setView(c(29.7632836,  -95.3632715), 10)
# L2$tileLayer(provider = "CartoDB")
# L2

# leaflet(quakes) %>% addProviderTiles(providers$CartoDB.DarkMatter) %>%
#   setView( 178, -20, 4) %>%
#   addHeatmap(lng = ~long, lat = ~lat, intensity = ~mag,
#              blur = 20, max = 0.05, radius = 15)

library(sp)
URL <- 'https://github.com/martabc/Geoscripting/blob/master/Final%20Project/heatmapViz/Data/foodStores_geojson.json'
v8 <- V8::v8()
v8$source(URL)
geoJson <- geojsonio::as.json(v8$get('pubsGeoJSON'))
spdf <- geojsonio::geojson_sp(geoJson)




