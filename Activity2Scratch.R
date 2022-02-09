# GEOG331 Activity2 Scratch work
# ES 1/31/22

#set working directory to noaa data folder
setwd( "Z:/students/egshaw/Data/noaa_weather/ " )

# make a vector of tree heights in meters
heights <- c(30, 41, 20, 22)

#convert to cm
heights_cm <- heights*100
heights_cm

#set up a matrix with 2 columns and fill in by rows
#first argument is the vector of numbers to fill in the matrix
Mat<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=TRUE)
Mat
Mat.bycol <- matrix(c(1,2,3,4,5,6), ncol=2, byrow = FALSE)
Mat.bycol[,2]


#read in weather station file from your data folder
datW <- read.csv("Z:/students/egshaw/Data/noaa_weather/2011124.csv")

#Question 1: the data frame has 9 columns and 157,849 variables
#Question 2: Differences between data types
chr <- (c("a", "b", "c", "hello", "world", "1"))
nmr <- (c(1, 2, 3, 4, 5, 6))
int <- c(1L, 2L, 3L, 4L, 5L)
fct <- factor(c("male", "female", "non-binary", "female", "female"))

#find out unique site names
unique(datW$NAME)
#look at mean teperature for Aberdeen
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm = TRUE)

#average daily temperature is midway between min and max
datW$TAVE <- datW$TMIN + ((datW$TMAX-datW$TMIN)/2)

#get the mean across all sites using aggregate(x = an r object, by = list of one
  #or more variables to index over, FUN = intended function, ... , na.rm = TRUE to ignore NA)
#the by function is a list of one or more variables to index over.

averageTemp <- aggregate(datW$TAVE, by=list(datW$NAME), FUN="mean",na.rm=TRUE)
averageTemp

#convert columns "STATION" and "NAME" to factors to match activity
  #columns commented out until they become relevant
#datW$STATION <- as.factor (datW$STATION)
datW$NAME <- as.factor (datW$NAME)
#datW$DATE <- as.factor (datW$DATE)

#siteN converted to factor and then to numeric data
datW$siteN <- as.numeric(datW$NAME)

#change the automatic output of column names to be more meaningful
#note that MAAT is a common abbreviation for Mean Annual Air Temperature
colnames(averageTemp) <- c("NAME","MAAT")
averageTemp

#make a histogram for the first site in our levels
#hist(x = vector, 
  # freq = FALSE, hist represents densities not frequencies
  # main = title (printed so use factor)
  # xlab, ylab = axis labels
  # col = color for fill bars
  # border = border color)

# hist(datW$TAVE[datW$siteN == 1],
#     freq=FALSE, 
#     main = paste(levels(datW$NAME)[1]),
#     xlab = "Average daily temperature (degrees C)", 
#     ylab="Relative frequency",
#     col="grey50",
#     border="white")
  #commented out with addition of same histogram but with standard deviation

#Question 3 - first argument sets the vector to be plotted as the averages from
  #site. freq = FALSE indicates that the data represents densities not frequencies
  #this means that the resulting histogram will return the component densities of 
  # a pdf, (i.e. area sums to 1). main, xlab, and ylab are the title and axis labels
  # respectively. col and border indicate the color of the bars (col) and border.
  #In the main arg, you use paste() to return a string instead of a factor value. 
  # paste() concatenates converted character strings of its argument.

#make histograms for our data using a for loop
lvl <- c(1,2,3,4,5)
for (val in lvl){
  hist(datW$TAVE[datW$siteN == val],
       freq=FALSE, 
       main = paste(levels(datW$NAME)[val]),
       xlab = "Average daily temperature (degrees C)", 
       ylab="Relative frequency",
       col="mintcream",
       border="snow4")
  #add mean line with violetred4
  #and thickness of 3
  abline(v = mean(datW$TAVE[datW$siteN == val],na.rm=TRUE), 
         col = "violetred4",
         lwd = 3)
  #add standard deviation line below the mean with violetred4 color
  #and thickness of 3
  abline(v = mean(datW$TAVE[datW$siteN == val],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == val],na.rm=TRUE), 
         col = "violetred4", 
         lty = 3,
         lwd = 3)
  #add standard deviation line above the mean with vioeltred4 color
  #and thickness of 3
  abline(v = mean(datW$TAVE[datW$siteN == val],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == val],na.rm=TRUE), 
         col = "violetred4", 
         lty = 3,
         lwd = 3)
}
#am i logged in
