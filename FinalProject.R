#Final Project
#install.packages("terra")
#install.packages("rnoaa")
library("terra")
library("tidyverse")
library("ggplot2")
library ("rnoaa")
library("lubridate")
tree.pecan <- read.csv("Z:/students/egshaw/Data/Final Project Data/individual_phenometrics_data.csv",
                       header = T)

#see if there are spatial variations in fruiting based on given information
long_fruit <- ggplot(subset(tree.pecan, Phenophase_Description == "Fruits"), 
               aes(x = First_Yes_DOY, y = Longitude, color = State)) +
               geom_jitter() +
               theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
               labs(title = "Variation in Fruiting") +
               xlab("First Sighting of Fruits (Day of Year)") +
               ylab("Longitude")
lat_fruit <- ggplot(subset(tree.pecan, Phenophase_Description == "Fruits"), 
               aes(x = First_Yes_DOY, y = Latitude, color = State)) +
               geom_jitter() +
               theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
               labs(title = "Variation in Fruiting") +
               xlab("First Sighting of Fruits (Day of Year)") +
               ylab("Latitude")
elev_fruit <- ggplot(subset(tree.pecan, Phenophase_Description == "Fruits"), 
               aes(x = First_Yes_DOY, y = Elevation_in_Meters, color = State)) +
               geom_jitter() +
               theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
               labs(title = "Variation in Fruiting") +
               xlab("First Sighting of Fruits (Day of Year)") +
               ylab("Elevation in Meters")
#there may be a relationship with longitude and elevation, but it's tricky to tell with such little data
#lets add the other phenophase descriptions that have to do with fruiting and look again
  
tree.pecan_fruit <- subset(tree.pecan, Phenophase_Description == "Fruits")
for (i in c("Recent fruit or seed drop", "Ripe fruits")){
  temp.df <- tree.pecan %>% subset(Phenophase_Description == i)
  tree.pecan_fruit <- full_join(tree.pecan_fruit, temp.df)
}

long_fruit_all <- ggplot(tree.pecan_fruit, 
                     aes(x = First_Yes_DOY, y = Longitude, color = State, shape = Phenophase_Description)) +
                     geom_jitter() +
                     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
                     labs(title = "Variation in Fruiting") +
                     xlab("First Sighting of Fruit Phenophase (Day of Year)") +
                     ylab("Longitude")
lat_fruit_all <- ggplot(tree.pecan_fruit, 
                     aes(x = First_Yes_DOY, y = Longitude, color = State, shape = Phenophase_Description)) +
                     geom_jitter() +
                     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
                     labs(title = "Variation in Fruiting") +
                     xlab("First Sighting of Fruit Phenophase (Day of Year)") +
                     ylab("Latitude")
elev_fruit_all <- ggplot(tree.pecan_fruit, 
                      aes(x = First_Yes_DOY, y = Elevation_in_Meters, color = State, shape = Phenophase_Description)) +
                      geom_point() +
                      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
                      labs(title = "Variation in Fruiting") +
                      xlab("First Sighting of Fruit Phenophase (Day of Year)") +
                      ylab("Elevation in Meters")
#with more information spatial determiners do not seem to play a large role in fruit
#related phenophase. There does visually appear to be a positive correlation between elevation and
#fruit development, but the graph could be misleading here.

#literature suggests temperature data may play a role in fruit development

#creating a new dataframe with unique locations, save unique site ids
site_ids <- unique(tree.pecan_fruit$Site_ID)
lat_lon_df <- tibble(.rows = 29)
lat_lon_df$id <- tree.pecan_fruit$Site_ID
lat_lon_df$Latitude <- tree.pecan_fruit$Latitude
lat_lon_df$Longitude <- tree.pecan_fruit$Longitude
lat_lon_df$elevation <- tree.pecan_fruit$Elevation_in_Meters
lat_lon_df <- lat_lon_df %>% distinct()


#Now that I have a tidier data frame, I can call nearby station data
station_data <- ghcnd_stations()
nearby_station_ids <- meteo_nearby_stations(lat_lon_df = lat_lon_df, station_data = station_data,
                                            lat_colname = "Latitude", lon_colname = "Longitude", 
                                            var = c("TMIN", "TMAX", "TAVG", "PSUN"), limit = 1, 
                                            year_min = 2012, year_max = 2015, radius = 50)
#we now have the locations of the nearest westher station for each pecan tree, but the
#data is a list of lists. We can fix that by transposing so the "column" headings are variables
nearby_station_ids <- transpose(nearby_station_ids)
#lists are tricky so lets turn this into a data frame
nearby_station_ids <- as_tibble(nearby_station_ids)
#Latitude and longitude should act like our original lat/lon columns (as well as the rest)
nearby_station_ids$latitude <- as.double(nearby_station_ids$latitude)
nearby_station_ids$longitude <- as.double(nearby_station_ids$longitude)
nearby_station_ids$id <- as.character(nearby_station_ids$id)
nearby_station_ids$name <- as.character(nearby_station_ids$name)
nearby_station_ids$distance <- as.numeric(nearby_station_ids$distance)
#we add the pecan id as a key variable
nearby_station_ids$pecanid <- lat_lon_df$id
#finally we can join the two together
tree.pecan_fruit <- left_join(tree.pecan_fruit, nearby_station_ids, by = c("Site_ID" = "pecanid"))
#it's confusing to have two latitude and longitude columns so let's fix that
colnames(tree.pecan_fruit)[28] <- c("latitude_wid")
colnames(tree.pecan_fruit)[29] <- c("longitude_wid")

#now that we have inscribed this information in tree.pecan_fruit, we can use the 
#nearby station ids dataframe to retrieve temperature data
nearby_station_ids <- nearby_station_ids %>% distinct(id, .keep_all = T)

datW <- ghcnd_search(nearby_station_ids$id, date_min = "2012-01-01", date_max = "2015-12-31", 
                       var = c("TMIN", "TMAX"))

#Turns out there is no percent sun data available with the given time frame and
#station parameters, so I took it out of the data collection
#also there is only average daily temperature data available from 2013, unfortunately
#I will try to focus more on ranges than average to accommodate this shortcoming

#ghcnd_search returns a list of two dataframes which we can separate by variable
#and join to a single tibble
datWtmin <- as_tibble(datW$tmin)
datWtmax <- as_tibble(datW$tmax)
datW <- full_join(datWtmin, datWtmax, by = c("id", "date"))

#ghcnd provides temperature data in tenths of degrees celsius, which is bizarre
datW$tmin <- datW$tmin / 10
datW$tmax <- datW$tmax / 10
datfull <- full_join(datW, tree.pecan_fruit, 
                     by = c("date", "id"))
#removing temperature values that did not pass QC
datfull$tmin[datfull$qflag.x != " "] <- NA
datfull$tmax[datfull$qflag.y != " "] <- NA

#Let's graph temperature data for the Knoxville station because it is closest to 
#the most phenophase measurements
knox_plot <- ggplot(subset(datW, id == "USW00013891"), aes(x = date, y = tmax)) +
              geom_ribbon(aes(ymin = tmin, ymax = tmax), alpha=0.3,       #transparency
                          linetype=1,      #solid, dashed or other line types
                          colour="grey70", #border line color
                          size=.2,          #border line size
                          fill="orange") +    #fill color) 
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
              labs(title = "Temperature variation in Knoxville") +
              xlab("Time") +
              ylab("Daily Minimum & Maximum Temperature (in Celsius)")

datW$year <- year(datW$date)
tree.pecan_fruit$First_Yes_Julian_Date <- as_date(tree.pecan_fruit$First_Yes_Julian_Date,
                                                  origin = structure(-2440588, class = "Date"))
colnames(tree.pecan_fruit)[18] <- c("date")
#it's clear these data are very noisy! there is a lot of variation
#let's look just at 2012 and overlay phenophase because all the phenophase data is in 2012
datknox <- subset(datfull, id == "USW00013891")
datknox$isfruit <- ifelse(is.na(datknox$Phenophase_Description), 0, 1)
maxyk <- max(datknox$tmax) + 5
knox_2012 <- ggplot(subset(datknox, year == 2012), aes(x = date, y = tmax)) +
              geom_ribbon(aes(ymin = tmin, ymax = tmax), alpha=0.3,       #transparency
                linetype=1,      #solid, dashed or other line types
                colour="grey70", #border line color
                size=.2,          #border line size
                fill="orange") +    #fill color) 
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
              labs(title = "Temperature Variation and Pecan Phenophase in Knoxville") +
              xlab("Day of Year") +
              ylab("Daily Minimum & Maximum Temperature (in Celsius)") +
              coord_cartesian(ylim = c(-10, 45)) +
              geom_ribbon(aes(ymin = -20, ymax = ((maxyk + 25) * isfruit)-20), 
                fill = "red", alpha = 0.2)
#it's pretty hard to glean any patterns from this, let's look at another site
#setting up a chart for Louisville
datlvil <- subset(datfull, id == "USW00093821" & year != 2012)
datlvil$isfruit <- ifelse(is.na(datlvil$Phenophase_Description), 0, 1)
maxyl <- max(datlvil$tmax) + 5
Lvilplot <- ggplot(datlvil, aes(x = date, y = tmax)) +
              geom_ribbon(aes(ymin = tmin, ymax = tmax), alpha=0.3,       #transparency
              linetype=1,      #solid, dashed or other line types
              colour="grey70", #border line color
              size=.2,          #border line size
              fill="orange") +    #fill color) 
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
              labs(title = "Temperature & Phenophase Variation in Louisville") +
              xlab("Time") +
              ylab("Daily Minimum & Maximum Temperature (in Celsius)") +
              coord_cartesian(ylim = c(-20, 40)) +
              geom_ribbon(aes(ymin = -31, ymax = ((maxyl + 25) * isfruit)-20), 
              fill = "red", alpha = 0.2)
