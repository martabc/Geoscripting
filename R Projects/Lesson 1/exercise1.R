# rm(list = ls()) # Clear the workspace!
# ls() # No objects left in the workspace

# raster::getData("ISO3")

countAndCap <- function(inCountry, admLevel){
  datdir <- 'data'
  dir.create(datdir, showWarnings = F)
  adm <- raster::getData("GADM", country = inCountry, 
                         level = admLevel, path = datdir)   ## level 2 indicates that we want the counties 
  ## to show, not just state
  ## level 0 = entire country, lvl 1 = states
  
  checkCap <- adm$TYPE_1
  resultCap <- names(table(checkCap))[table(checkCap)<2] 
  
  par(mfrow = c(1, 2))
  
  mainc <- adm[adm$NAME_0 == inCountry,]
  cap <- adm[adm$TYPE_1 == resultCap,]
  
  plot(mainc, bg = "bisque", axes=T, main = "Entire Country", cex.axis= 0.5, cex.main = 0.7, cex.axis= 0.5)
  plot(mainc, lwd = 5, border = "darkorange", add=T)
  plot(mainc, col = "peru", add = T)
  grid()
  box()
  mtext(side = 1, "Longitude", line = 2.5, cex=0.5)
  mtext(side = 2, "Latitude", line = 2.5, cex=0.5)
  
  plot(cap, bg = 'bisque', axes = T, main = "Location of Capital", cex.axis= 0.5, cex.main = 0.7)
  plot(cap, lwd = 5, border = "darkorange", add=T)
  plot(cap, col = "peru", add = T)
  grid()
  box()
  
  invisible(text(getSpPPolygonsLabptSlots(cap),
                 labels = as.character(cap$TYPE_1), cex = 1,
                 col = "royalblue4", font = 1))
  
  title(cat("Map of", inCountry, "and\n Area where Capital is Located", outer = T, cex = 2))
  mtext(side = 1, "Longitude", line = 2.5, cex=0.5)
  mtext(side = 2, "Latitude", line = 2.5, cex=0.5)
  text("Projection: Geographic\n
       Coordinate System: WGS 1984\n
       Data Source: GADM.org", adj = c(0, 0), cex = 0.2, col = "grey20")
}


countAndCap('Mexico', 1)
