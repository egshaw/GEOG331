#In class work with Remote Sensing date 4/6
#Elizabeth Shaw

#load the terra package
library(terra)

#set working directory
setwd("Z:/students/egshaw/GitHub/GEOG331/")

#read a raster from file
p <- rast("Z:/data/rs_data/20190706_002918_101b_3B_AnalyticMS_SR.tif")

#plot raster
plot(p)

#plot an rgb rendering of the data
plotRGB(p, r = 3, g = 2, b = 1,
        scale = 65535,
        stretch = "hist")
