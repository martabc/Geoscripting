# rm(list = ls()) # Clear the workspace!
# ls() # No objects left in the workspace

# raster::getData("ISO3")
datdir <- 'data'
dir.create(datdir, showWarnings = F)
adm <- raster::getData("GADM", country = "USA", 
                       level = 2, path = datdir)   ## level 2 indicates that we want the counties 
                                                  ## to show, not just state
                                                  ## level 0 = entire country, lvl 1 = states
plot(adm[adm$NAME_1 == "Alaska",])

col <- adm[adm$NAME_1 == "Bethel",]

plot(col, bg = "dodgerblue", axes=T)
plot(col, lwd = 10, border = "skyblue", add=T)
plot(col, col = "green4", add = T)
grid()
box()
invisible(text(getSpPPolygonsLabptSlots(col),
               labels = as.character(col$NAME_2), cex = 1.1, 
               col = "white", font = 2))

mtext(side = 3, line = 1, "Map of CO", cex = 2)
mtext(side = 1, "Longitude", line = 2.5, cex=1.1)
mtext(side = 2, "Latitude", line = 2.5, cex=1.1)
text(122.08, 13.22, "Projection: Geographic\n
     Coordinate System: WGS 1984\n
     Data Source: GADM.org", adj = c(0, 0), cex = 0.7, col = "grey20")
