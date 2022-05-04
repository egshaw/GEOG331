#Final Project
#install.packages("terra")
#install.packages("rnoaa")
library("terra")
library("tidyverse")
library("ggplot2")
library ("rnoaa")
tree.pecan <- read.csv("Z:/students/egshaw/Data/Final Project Data/individual_phenometrics_data.csv",
                       header = T)
#creating a new dataframe with unique locations, save unique site ids
site_ids <- unique(tree.pecan$Site_ID)
lat_lon_df <- tibble(.rows = 221)
lat_lon_df$id <- tree.pecan$Site_ID
lat_lon_df$latitude <- tree.pecan$Latitude
lat_lon_df$longitude <- tree.pecan$Longitude
lat_lon_df$elevation <- tree.pecan$Elevation_in_Meters
lat_lon_df <- lat_lon_df %>% distinct()


#Now that I have a tidier data frame, I can (hopefully) get nearby station data
station_data <- ghcnd_stations()
nearby_station_ids <- meteo_nearby_stations(lat_lon_df = lat_lon_df, station_data = station_data,
                                            lat_colname = "latitude", lon_colname = "longitude", 
                                            var = c("TMIN", "TMAX", "TAVG", "PSUN"), limit = 1, 
                                            year_min = 2012, year_max = 2015, radius = 10)

nearby_station_ids <- transpose(nearby_station_ids)

datW <- tibble(.rows = 4)
for (i in 1:22){
  if_else (is.na(nearby_station_ids[[1]][[i]]), datW$i <- NA, next)
  datW$i <- ghcnd_search(nearby_station_ids[[1]][[i]], date_min = "2012-01-01", date_max = "2015-12-31", 
                         var = c("TMIN", "TMAX", "TAVG", "PSUN"))
}

#Need to figure out a way to have all my data in one place, going back to the drawing
#board a bit to figure out a structure for this data

station_13891 <- ghcnd_search("USW00013891", date_min = "2012-01-01", date_max = "2015-12-31", 
                     var = c("TMIN", "TMAX", "TAVG", "PSUN"))
station_13891 <- do.call(rbind, station_13891)

#station_13891 <- bind_rows(station_13891)



# color = State[Phenophase_Description == "Breaking leaf buds"]
fig1 <- ggplot(subset(tree.pecan, Phenophase_Description == "Breaking leaf buds"), 
               aes(x = First_Yes_DOY, y = Longitude, color = State)) +
        geom_point() +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        labs(title = "Variation in Breaking Leaf Buds") +
        xlab("First Sighting of Breaking Leaf Buds (Day of Year)") +
        ylab("Longitude")

