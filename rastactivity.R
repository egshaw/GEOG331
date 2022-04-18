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
p2 <- crop(p, b3,overwrite = T,
           filename = "20190706_SR_crop.tif")

#make a plot to see how it looks
plotRGB(p2, r = 3, g = 2, b = 1,
        scale = 65535,
        stretch = "lin")

#calculate NDVI
#double brackets represent layer within spat raster/spat vector (i.e. this calculates
#ndvi pixel by pixel)
ndvi <- (p2[[4]] - p2[[3]])/(p2[[4]] + p2[[3]])

#set the name layer to ndvi to avoid confusion
names(ndvi) <- "ndvi"

#create a plot of the ndvi map with sample points on top
png(filename = "ndvi_map.png",
    width = 6, height = 4, units = "in", res = 300)

plot(ndvi)
points(gtree2, cex = gtree$cc.pct/50, col = "blue")

#stop writing to the file "ndvi_map.png"
dev.off()

#extract NDVI values for each point (careful with extract functions! dplyr also
#has an exract function)
nt <- terra:: extract(ndvi, gtree2, fun = mean, method = 'bilinear')

#plot ndvi vs. canopy cover
plot(nt$ndvi, gtree2$cc.pct,
     pch = 16, col = "blue")

#plot again but fix the y axis
plot(nt$ndvi, gtree$cc.pct,
     pch = 16, col = "blue",
     xlim = c(0,1))





