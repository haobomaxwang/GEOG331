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

# use this to crop the raster layer so we can see just
# the area of interest
p2<- crop(p,b3, overwrite=T,
          filename= "20190706_SR_crop.tif")

## plot the newly cropped raster
plotRGB(p2,r=3, g=2, b=1,
        scale = 65535,
        stretch = "lin")

# show the points again
points(gtree2, col= "blue", cex=gtree2$cc.pct/50)


# create a plot of the ndvi map with sample points on top
# calculate ndvi
ndvi<- (p2[[4]]-p2[[3]])/(p2[[4]]+p2[[3]])
names(ndvi)<- "ndvi"
ndvi


## make a png file 
help(png)

png(filename = "ndvi_map.png",
    width = 6, height = 4, units = "in", 
    res = 300)

plot(ndvi)
points(gtree2, col= "blue", cex=gtree2$cc.pct/50)

dev.off()

# extract NDVI values for each point
help(extract)
nt<- terra::extract(ndvi, gtree2, fun= mean, 
                    method= "bilinear") #four nearest cells

nt

plot(nt$ndvi, gtree2$cc.pct, 
     pch=16, col= "blue",
     xlim= c(0,1))
