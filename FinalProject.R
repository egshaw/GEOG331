#Final Project
#install.packages("terra")
#install.packages("rnoaa")
library("terra")
library("tidyverse")
library("ggplot2")
library ("rnoaa")
tree.pecan <- read.csv("Z:/students/egshaw/Data/Final Project Data/individual_phenometrics_data.csv",
                       header = T)
#trying to split Phenophase Description column into three columns so it will be 
#easy to distinguish leaf, bud, and fruit phenophases and compare them to each other.
#tree.pecan <- tree.pecan %>% separate(col = Phenophase_Description, 
  #                                    into = c("Phenophase_Leaves", "Phenophase_Flowers", "Phenophase_Fruits"))
#this is the test which works
station_data <- ghcnd_stations()
lat_lon_df <- data.frame(id = c("sydney", "brisbane"),
                         latitude = c(-33.8675, -27.4710),
                         longitude = c(151.2070, 153.0234))
nearby_stations <- meteo_nearby_stations(lat_lon_df = lat_lon_df,
                                         station_data = station_data, radius = 10)

#this is my attempt, which does not work 
nearby_station_ids <- meteo_nearby_stations(tree.pecan, station_data = station_data,
                                            lat_colname = "Latitude", lon_colname = "Longitude", 
                                            var = c("TMIN", "TMAX", "TAVG", "PSUN"), 
                                            year_min = 2012, year_max = 2015, radius = 10)


# color = State[Phenophase_Description == "Breaking leaf buds"]
fig1 <- ggplot(subset(tree.pecan, Phenophase_Description == "Breaking leaf buds"), 
               aes(x = First_Yes_DOY, y = Longitude, color = State)) +
        geom_point() +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        labs(title = "Variation in Breaking Leaf Buds") +
        xlab("First Sighting of Breaking Leaf Buds (Day of Year)") +
        ylab("Longitude")

