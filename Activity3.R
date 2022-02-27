#create a function. The names of the arguments for your function will be in parentheses. Everything in curly brackets will be run each time the function is run.
assert <- function(statement,err.message){
  #if evaluates if a statement is true or false for a single item
  if(statement == FALSE){
    print(err.message)
  }
  
}

######################################
##          Question 1              ##
######################################
#The RH values are most accurate for humidity in the 25 to 65 percent range. The 
# temperature range with the highest RH accuracy ranges from about 20 degrees C
# to 60 degrees C. RH values are notably inaccurate at low temperatures (O degrees C)
# and low humidity (0). Depending on the location of the sensor, this could give
# a lot of NA values (e.g. this sensor would preform poorly in Hamilton, New York).

######################################
##          Question 2              ##
######################################
#Section 3.3 states "If an instantaneous measurement is more than eight times the running
#average, the instantaneous measurement is rejected. It is not reported as the maximum gust
#or included in the data that are averaged over the output interval." This is designed 
# to prevent signal errors in wind speed calculation, and relies on the assumption that 
# a factor of 8 is too large a change within a 100 second time span. Although this is 
# likely included because the sensor would return faulty measurements otherwise, it seems
# likely that in gusty weather, it would be possible to exceed this measurement legitimately.

#use install.packages to install lubridate
#install.packages(c("lubridate"))
#commented out for convenience of script.
library(lubridate)

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
# The argument "skip" ignores the first number of indicated lines. This is useful 
#when the first few lines may contain headers and unit information that would otherwise
#indicate to r to read the rest of the data as character data. nrows indicates the 
#number of rows to read in. Thus, skip is exclusive and nrows is inclusive. The arg
# header = FALSE indicates that the first row that will be read in contains data 
#and does not contain row headers.

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
#It can be particularly confusing when you are just learning R.
#Here I'm using the ifelse function
#the first argument is a logical statement to be evaluated as true or false on a vector
#the second argument is the value that my air.tempQ1 column will be given if the statement
#is true. The last value is the value that will be given to air.tempQ1 if the statement is false.
#In this case it is just given the air temperature value
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
#Based on the wunderground data, our data seem pretty accurate. The measurements 
#recorded in Rome for 6/22, 6/26, 7/1 and 7/2 are within a degree Celsius of our measurements
# and seem to follow the same daily trends. The accuracy of the air temperature sensor does
#not vary with humidity or solar radiation. So although relative humidity is higher for the 
#days with low temperature, this does not indicate a higher chance of error. Overall, 
# it seems like we can interprete these data as reliable.

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
assert(identical(length(datW$DD),length(lightscale)), "lengths are not equal")
assert(identical((datW$DD[lightscale > 0]), datW$DD(lightning.acvitivy > 0)), "there are no days with lightning activity")
# Lightscale is not held in the dataframe. It is derived, however, from the values in
#the dataframe that directly correspond to Decimal Dates. Lightscale alters the scale
#of lightning activity values using the max precipitation value so that we can see both 
#on the same plot. This does not alter the order in any way, so that 
#end work 2/27 4:00pm