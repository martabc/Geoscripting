## AUTHOR: Marta Blanco
## DATE: May 8, 2017
## Project: Lesson 6

## In this exercise I query for industrial railways, perform buffers,
## find intersections of cities in the buffer, create a summary plot, and
## ensure that a well documented script with comments is uploaded onto GitHub

# Create a clear and documented script that:
#   
#   Selects the "industrial" (type == "industrial") railways
# Buffers the "industrial" railways with a buffer of 1000m (hint: gBuffer with byid=TRUE)
# Find the place (i.e. a city) that intersects with this buffer.
# Create a plot that shows the buffer, the points, and the name of the city
# write down the name of the city and the population of that city as one comment at the end of the script.

library(rgdal)
library(sp)

dsn = file.path("data","railways")
rail <- readOGR(dsn, layer = ogrListLayers(dsn))

plot(rail, col = "blue")
summary(rail) ## gives a snapshot of the fields and first rows of data
