######################################
##          Homework 3              ##
######################################
#Environmental Data Science - GEOG331
#Colgate University Spring 2022
#-------------------------------------

#use install.packages to install lubridate
#install.packages(c("lubridate"))
#commented out for convenience of script.
library(lubridate)

#create a function. The names of the arguments for your function will be in parentheses. 
#Everything in curly brackets will be run each time the function is run.
assert <- function(statement,err.message){
  #if evaluates if a statement is true or false for a single item
  if(statement == FALSE){
    print(err.message)
  }
  
}

######################################
##          Question 1              ##
######################################

######################################
##          Question 2              ##
######################################


#read in the data file
#skip the first 3 rows since there is additional column info
#specify the the NA is designated differently
datW <- read.csv("Z:/students/egshaw/Data/bewkes/bewkes_weather.csv", 
                 na.strings=c("#N/A"), skip=3, header=FALSE)

#get sensor info from file
# this data table will contain all relevant units
sensorInfo <-   read.csv("Z:/students/egshaw/Data/bewkes/bewkes_weather.csv",
                         na.strings=c("#N/A"), nrows=2)

print(sensorInfo)

#get column names from sensorInfo table
# and set weather station colnames  to be the same
colnames(datW) <-   colnames(sensorInfo)
#preview data
print(datW[1,])

######################################
##          Question 3              ##
######################################

#convert to standardized format
#date format is m/d/y
dates <- mdy_hm(datW$timestamp, tz= "America/New_York")
#calculate day of year
datW$doy <- yday(dates)
#calculate hour in the day
datW$hour <- hour(dates) + (minute(dates)/60)
#calculate decimal day of year
datW$DD <- datW$doy + (datW$hour/24)

#see how many values have missing data for each sensor observation
#air temperature
length(which(is.na(datW$air.temperature)))
#wind speed
length(which(is.na(datW$wind.speed)))
#precipitation
length(which(is.na(datW$precipitation)))
#soil temperature
length(which(is.na(datW$soil.moisture)))
#soil moisture
length(which(is.na(datW$soil.temp)))

#make a plot of soil moisture with filled in points (using pch)
plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil moisture (cm3 water per cm3 soil)")

#make a plot of air temperature with filled in points (using pch)
plot(datW$DD, datW$air.temperature, pch=19, type="b", xlab = "Day of Year",
     ylab="Air temperature (degrees C)")


#I'm going to make a new column to work with that indicates that I am conducting QAQC
#because overwriting values should be done cautiously and can lead to confusing issues.
datW$air.tempQ1 <- ifelse(datW$air.temperature < 0, NA, datW$air.temperature)

#check the values at the extreme range of the data
#and throughout the percentiles
quantile(datW$air.tempQ1)

#look at days with really low air temperature
datW[datW$air.tempQ1 < 8,] 

#look at days with really high air temperature
datW[datW$air.tempQ1 > 33,]

######################################
##          Question 4              ##
######################################

#plot precipitation and lightning strikes on the same plot
#normalize lighting strikes to match precipitation
lightscale <- (max(datW$precipitation)/max(datW$lightning.acvitivy)) * datW$lightning.acvitivy
#make the plot with precipitation and lightning activity marked
#make it empty to start and add in features
plot(datW$DD , datW$precipitation, xlab = "Day of Year", ylab = "Precipitation & lightning",
     type="n")
#plot precipitation points only when there is precipitation 
#make the points semi-transparent
points(datW$DD[datW$precipitation > 0], datW$precipitation[datW$precipitation > 0],
       col= rgb(95/255,158/255,160/255,.5), pch=15)        

#plot lightning points only when there is lightning     
points(datW$DD[lightscale > 0], lightscale[lightscale > 0],
       col= "tomato3", pch=19)

######################################
##          Question 5              ##
######################################
assert(identical((datW$DD[lightscale > 0]), datW$DD[datW$lightning.acvitivy > 0]), 
       "indexing by lightscale is not equivalent to lightning.acvitivy")


######################################
##          Question 6              ##
######################################

#filter out storms air temperature measurements
#filter all values with lightning that coincides with rainfall greater than 2mm or only rainfall over 5 mm.    
#create a new air temp column
datW$air.tempQ2 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$air.tempQ1))

#filter storms out of wind measurements and create a new column
#filter all values with lightning that coincides with rainfall greater than 2mm or only rainfall over 5 mm. 
datW$wind.speedQ1 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                            ifelse(datW$precipitation > 5, NA, datW$wind.speed))

#add a column that indicates whether or not a thunderstorm is happening
datW$thunderstorm <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, TRUE,
                            ifelse(datW$precipitation > 5, TRUE, FALSE))

#assert that when a thunderstorm is happening, wind speed is NA
assert(identical(is.na(datW$wind.speedQ1), datW$thunderstorm), "QC Ineffective")

#plot new wind speed values
plot(datW$DD, datW$wind.speedQ1, xlab = "Day of Year", ylab = "Quality Controlled Wind Speed in m/s",
     type="b", col = "lightsteelblue", pch = 20)

######################################
##          Question 7              ##
######################################
#determine the period of the data logger error
which(is.na(datW$soil.moisture))
which(is.na(datW$soil.temp))
#there are NA values from the 1412th item till the end of our dataset for both soil
#moisture and soil temperature.
datW$timestamp[1412]
#this correlates to July 11.
#plot soil temp and moisture on the same plot.
#make moisture relative to temperature to see both on the same plot
moisturescale <- (max(datW$soil.temp, na.rm = TRUE)/max(datW$soil.moisture, na.rm = TRUE))*datW$soil.moisture

plot(datW$DD[0:1412], datW$soil.temp[0:1412], xlab = "Day of Year", ylab = "Soil Temp Degrees C & Relative Moisture",
     type="n")

points(datW$DD[0:1412], datW$soil.temp[0:1412], col = rgb(129/255, 49/255, 36/255, .5), pch = 20)
points(datW$DD[0:1412], moisturescale[0:1412], col = "skyblue3", pch = 18)

#nothing looks too out of the ordinary so far.
#plot soil temp and air temp to identify possible patterns.

plot(datW$DD[0:1412], datW$soil.temp[0:1412],main = "Soil & Air Temperature Data",
     xlab = "Day of Year", ylab = "Temperature Degrees C", type="n")

points(datW$DD[0:1412], datW$soil.temp[0:1412], col = rgb(129/255, 49/255, 36/255, .5), pch = 20)
points(datW$DD[0:1412], datW$air.tempQ2[0:1412], col = rgb(0/255, 0/255, 0/255, .5), pch = 20)

#from this plot, there does not seem to be anything out of the ordinary in the measurements
#leading up to the blackout.

######################################
##          Question 8              ##
######################################
#install.packages("reactable")
library(reactable)

#start accruing necessary values
#make a list of important averages
col.order <- c(18, 19, 12, 13, 3)
obs.count <- numeric(0)
aves <- numeric(0)
#determine and store the number of non NA values in each column of interest
for (i in col.order){
  obs.count <- append(obs.count, length(which(!is.na(datW[,i]))))
  ifelse(i != 3 , aves <- append(aves, mean(datW[,i], na.rm = T)), aves <- append(aves, NA))
}

#define start and ending observation numbers to eliminate redundancy
final = 2118
logger.error = 1412
#make a list of end observations to indicate time period in the final table
end.obs <- c(datW$timestamp[final], datW$timestamp[final], datW$timestamp[logger.error],
             datW$timestamp[logger.error], datW$timestamp[final])


#find the value of total precip in mm
total.precip <- sum(datW$precipitation)
totals <- c(NA, NA, NA, NA, total.precip)

#create a data frame with these values
presen.table <- data.frame(obs.count, end.obs, aves, totals)
#round the averages to the correct decimal place
presen.table[1,3] <- round(presen.table[1,3], digits = 1)
presen.table[2,3] <- round(presen.table[2,3], digits = 2)
presen.table[3,3] <- round(presen.table[3,3], digits = 4)
presen.table[4,3] <- round(presen.table[4,3], digits = 1)

colnames(presen.table) <- c("Number of Observations", "Final Observation Date", "Average", "Total")
rownames(presen.table) <- c("Air Temperature Degrees C", "Wind Speed m/s", "Soil Moisture m^3/m^3", "Soil Temperature Degrees C", "Precipitation (mm)")
#return the table in a nicer and more editable form
reactable(presen.table)

######################################
##          Question 9              ##
######################################

#make plots of soil moisture, air temperature, soil temperature, and precipitation
par(mfrow = c(2,2))

plot(datW$DD, datW$soil.moisture, main = "Summer 2018 Soil Moisture Levels", 
     xlab = "Decimal Day", ylab = "Soil Moisture (m^3/m^3)", type="b", pch = 20, 
     col = "cornflowerblue")
plot(datW$DD, datW$air.temperature, main = "Summer 2018 Air Temperature Levels",
     xlab = "Decimal Day", ylab = "Air Temperature Degrees C", type="b", pch = 20, 
     col = "cornflowerblue")
plot(datW$DD, datW$soil.temp, main = "Summer 2018 Soil Temperature Levels",
     xlab = "Decimal Day", ylab = "Soil Temperature Degrees C", type="b", pch = 20, 
     col = "cornflowerblue")
plot(datW$DD, datW$precipitation, main = "Summer 2018 Precipitation Levels",
     xlab = "Decimal Day", ylab = "Precipitation (mm)", type="b", pch = 20, 
     col = "cornflowerblue")


plot(datW$DD[0:1412], datW$soil.temp[0:1412],main = "Soil & Air Temperature Data",
     xlab = "Day of Year", ylab = "Temperature Degrees C", type="n")

points(datW$DD[0:1412], datW$soil.temp[0:1412], col = rgb(129/255, 49/255, 36/255, .5), pch = 20)
