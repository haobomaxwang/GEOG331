# load in the terra package 
library(terra)

setwd("Z:/students/hwang/homework")

p <- rast("Z:/data/rs_data/20190706_002918_101b_3B_AnalyticMS_SR.tif")

# take a look at p metadata
p
# it's showing coordinate system, cell size, extent 


plot(p)

# plot an rgb rendering of the data

plotRGB(p, r=3, g=2, b=1)

plotRGB(p, r=3, g=2, b=1,
        scale = 65535,
        stretch = "hist")
help("plotRGB")

## readin csv
tree<- read.csv("Z:/students/hwang/DATA/siberia_stand_data.csv", 
                header = T)

## convert to vector 
gtree<- vect(tree, geom = c("Long", "Lat"), "epsg:4326")

## project the projected coordinate system to match rastor data

gtree2<- project(gtree, p)

## plot the tree points
plot(gtree2, add=T, col= "red")

# create a polygon from the extent of the points
b<- as.lines(ext(gtree), "epsg:4326")

# reproject the polygons to the same projection 
b2<- project(b, crs(p))
plot(b2, add=T)

# buffer the extent by 200 m 
b3<- buffer(b2, width=200)
plot(b3, add=T)
