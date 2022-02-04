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
