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
