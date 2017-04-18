# Author: Marta Blanco Castano
# Week 1 exercise

raster::getData("ISO3")
library(raster)
library(sp)

countAndCap <- function(inCountry, admLevel){
  datdir <- 'data'
  dir.create(datdir, showWarnings = F)
  adm <- raster::getData("GADM", country = inCountry, 
                         level = admLevel, path = datdir)   ## level 2 indicates that we want the counties 
  ## to show, not just state
  ## level 0 = entire country, lvl 1 = states
  
  mainc <- adm[adm$NAME_0 == inCountry,]
  
  plot(mainc, bg = "bisque", axes=T, cex.axis= 0.5, cex.main = 0.7, cex.axis= 0.5)
  plot(mainc, lwd = 5, border = "darkorange", add=T)
  plot(mainc, col = "peru", add = T)
  grid()
  box()
  mtext(side = 1, "Longitude", line = 2.5, cex=0.5)
  mtext(side = 2, "Latitude", line = 2.5, cex=0.5)
  
  title(sprintf("Map of %s with Labels", inCountry), outer = F, cex = 2)
  mtext(side = 1, "Longitude", line = 2.5, cex=0.5)
  mtext(side = 2, "Latitude", line = 2.5, cex=0.5)
  mtext("\n\n\n\nDatum: WGS 1984", cex = .5, side = 4, col = "grey20")
  
  
  invisible(text(getSpPPolygonsLabptSlots(mainc),
                 labels = as.character(mainc$NAME_1), cex = 0.5,
                 col = "royalblue4", font = 1))
  
}


countAndCap('Italy', 1)  ## ensure input country is within parenthesis
