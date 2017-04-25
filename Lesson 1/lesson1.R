rm(list = ls()) # Clear the workspace!
ls() # No objects left in the workspace

add <- function(x = 5){
  z <- x + 1
  return (z)
}

newfunc <- function(x, y) {
  z <- 2*x + y
  return(c(z,x,y))
}

raster::getData("ISO3")
datdir <- 'data'
dir.create(datdir, showWarnings = F)
adm <- raster::getData("GADM", country = "PHL", 
                       level = 2, path = datdir)
plot(adm[adm$NAME_1 == "Tarlac",])

mar <- adm[adm$NAME_1 == "Marinduque",]

plot(mar, bg = "dodgerblue", axes=T)
plot(mar, lwd = 10, border = "skyblue", add=T)
plot(mar, col = "green4", add = T)
grid()
box()
invisible(text(getSpPPolygonsLabptSlots(mar),
               labels = as.character(mar$NAME_2), cex = 1.1, 
               col = "white", font = 2))

mtext(side = 3, line = 1, "Provincial Map of Marinduque", cex = 2)
mtext(side = 1, "Longitude", line = 2.5, cex=1.1)
mtext(side = 2, "Latitude", line = 2.5, cex=1.1)
text(122.08, 13.22, "Projection: Geographic\n
     Coordinate System: WGS 1984\n
     Data Source: GADM.org", adj = c(0, 0), cex = 0.7, col = "grey20")
