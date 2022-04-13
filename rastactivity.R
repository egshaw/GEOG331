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

#read file with field observations of canopy cover
tree <- read.csv("Z:/data/rs_data/siberia_stand_data.csv", header = T)

#convert to vector object using terra
gtree <- vect(tree, geom = c("Long", "Lat"), crs = "espg:4326")

#project the data to match the coordinate stem of the raster layer
gtree2 <- project(gtree, p)

#create a polygon from the extent of the points
#must respecify crs
b <- as.lines(ext(gtree), "espg:4326")

#reproject the polygons to the same projection as our raster
b2 <- project(b,crs(p))

#buffer the extent by 200m
b3 <- buffer(b2, width = 200)

#use this to crop the raster layer so we can see just our study area
p2 <- crop(p, b3,
           filename = "20190706_SR_crop.tif")
