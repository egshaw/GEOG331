######################################
###          Activity 5            ###
###        Elizabeth Shaw          ###
######################################
#install.packages("tidyverse")
#load in lubridate
library(lubridate)
library (dplyr)

#read in streamflow data and precipitation data (in mm)
datH <- read.csv("Z://students/egshaw/Data/streamflow/stream_flow_data.csv", 
                 na.strings = c("Eqp"))
datP <- read.csv("Z://students/egshaw/Data/streamflow/2049867.csv")

#only use measurements approved for publication
datD <- datH[datH$discharge.flag == "A",]

#### define time for streamflow #####
#convert date and time
datesD <- as.Date(datD$date, "%m/%d/%Y")
#get day of year
datD$doy <- yday(datesD)
#calculate year
datD$year <- year(datesD)
#define time
timesD <- hm(datD$time)

#### define time for precipitation #####    
dateP <- ymd_hm(datP$DATE)
#get day of year
datP$doy <- yday(dateP)
#get year 
datP$year <- year(dateP)

#### get decimal formats #####
#convert time from a string to a more usable format
#with a decimal hour
datD$hour <- hour(timesD ) + (minute(timesD )/60)
#get full decimal time
datD$decDay <- datD$doy + (datD$hour/24)
#calculate a decimal year, but account for leap year
datD$decYear <- ifelse(leap_year(datD$year),datD$year + (datD$decDay/366),
                       datD$year + (datD$decDay/365))
#calculate times for datP                       
datP$hour <- hour(dateP ) + (minute(dateP )/60)
#get full decimal time
datP$decDay <- datP$doy + (datP$hour/24)
#calculate a decimal year, but account for leap year
datP$decYear <- ifelse(leap_year(datP$year),datP$year + (datP$decDay/366),
                       datP$year + (datP$decDay/365))

#plot discharge
plot(datD$decYear, datD$discharge, type="l", xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))

######################################
###          Question 5            ###
######################################
#formatting for plot
aveF <- aggregate(datD$discharge, by=list(datD$doy), FUN="mean")
colnames(aveF) <- c("doy","dailyAve")
sdF <- aggregate(datD$discharge, by=list(datD$doy), FUN="sd")
colnames(sdF) <- c("doy","dailySD")

#start new plot
dev.new(width=8,height=8)

#bigger margins
par(mai=c(1,1,1,1))
#make plot
#show standard deviation around the mean

plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",
     axes = FALSE)#remove gaps from axes 
lines(datD$discharge[datD$year == 2017],
      col = "coral")

polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col= rgb(0.392, 0.584, 0.929,.2),
        border=NA#no border
)

Months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
            "Nov", "Dec", "")

axis(1, seq(0,366, by = 30), #tick intervals
     lab= paste(Months)) #tick labels
axis(2, seq(0,100, by=20),
     seq(0,100, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation", "2017 discharge data"), #legend items
       lwd=c(2,NA, 2),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2), "coral"),#colors
       pch=c(NA,15, NA),#symbols
       bty="n")#no legend border

######################################
###          Question 7            ###
######################################
#aggregate Precipitation to see which days have 24 associated measurements
dat.comp <- aggregate(datP, by=list(datP$doy, datP$year), FUN = length)

#this will be the final data frame
days.comp <- tibble()
#this is rewritten with each loop
day.comp <- tibble()
#this vector keeps track of the day
days <- c(0)
#this value keeps track of the year
yr <- 2007
#The for loop is looping over days of the year that have 24 hours of data. The if
#statement makes sure those days are associated with the correct year. We then add
#the i to days to keep track of it, filter for the current day, and add that data to
#a larger data frame.
for (i in dat.comp$Group.1[dat.comp$HPCP == 24]){
  if (i < days[length(days)]) yr <- yr + 1
  days[length(days)+ 1] <- i
  day.comp <- datP %>%
    filter(doy == i, year == yr)
  days.comp <- rbind(days.comp, day.comp)
}
#The plot first plots all discharge values, and then adds points in a different 
#color along the x-axis to showcase days with full precipitation measurements.
ggplot (datD, aes(x = decYear, y = discharge)) +
        geom_point(aes(color = "Discharge")) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              legend.title = element_text(paste("Legend"))) +
        geom_point(data = days.comp, aes(x=decYear, y=0, 
                                         color="Days with Precipitation Data")) +
        labs(title="Distribution of Daily Discharge Data & Hourly Precipitation",
       x ="Year", y = expression(paste("Discharge ft"^"3 ","sec"^"-1")))      

######################################
###          Question 8            ###
######################################
#subsest discharge and precipitation within range of interest
hydroD <- datD[datD$doy >= 248 & datD$doy < 250 & datD$year == 2011,]
hydroP <- datP[datP$doy >= 248 & datP$doy < 250 & datP$year == 2011,]
min(hydroD$discharge)#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydroD$discharge))-1
#ceiling rounds up to the integer
yh <- ceiling(max(hydroD$discharge))+1
#minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydroP$HPCP))+.5
#scale precipitation to fit on the 
hydroP$pscale <- (((yh-yl)/(pm-pl)) * hydroP$HPCP) + yl

par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD$decDay,
     hydroD$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     xlab="Day of year, 2011", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP)){
  polygon(c(hydroP$decDay[i]-0.017,hydroP$decDay[i]-0.017,
            hydroP$decDay[i]+0.017,hydroP$decDay[i]+0.017),
          c(yl,hydroP$pscale[i],hydroP$pscale[i],yl),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}
#Making another hydrograph with data from winter day with all 24 hours of precipitation data
#subsest discharge and precipitation within range of interest
hydroD.w <- datD[datD$doy >= 12 & datD$doy < 14 & datD$year == 2011,]
hydroP.w <- datP[datP$doy >= 12 & datP$doy < 14 & datP$year == 2011,]

#floor rounds down the integer
yl <- floor(min(hydroD.w$discharge))-1
#ceiling rounds up to the integer
yh <- ceiling(max(hydroD.w$discharge))+1
#minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydroP.w$HPCP))+.5
#scale precipitation to fit on the 
hydroP.w$pscale <- (((yh-yl)/(pm-pl)) * hydroP$HPCP) + yl

par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD.w$decDay,
     hydroD.w$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     xlab="Day of year, 2011", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP)){
  polygon(c(hydroP.w$decDay[i]-0.017,hydroP.w$decDay[i]-0.017,
            hydroP.w$decDay[i]+0.017,hydroP.w$decDay[i]+0.017),
          c(yl,hydroP.w$pscale[i],hydroP.w$pscale[i],yl),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}

######################################
###          Question 9            ###
######################################
library(ggplot2)
#specify year as a factor
datD$yearPlot <- as.factor(datD$year)
#make a boxplot
ggplot(data= datD, aes(yearPlot,discharge)) + 
  geom_boxplot()
#make a violin plot
ggplot(data = datD, aes(yearPlot,discharge)) + 
  geom_violin()

#Sort data by season
#The data is from North of the Equator in a four season climate so this analysis
#should prove useful
#subset data for 2016
datD.2016 <- datD[datD$year == 2016,]
#Calculate season breaks in day of year and record them for leap years and non-leap years
leap.szn <- c(61, 153, 245, 336)
nleap.szn <- c(60, 152, 244, 335)
#Create a factor vector of season names (this would prove useful if you wanted
# to translate the labels, for instance). Defining these as factors allows us to 
#order the graph chronologically instead of alphabetically (although the winter data
#bookends instead of truly coming first).
szn.name <- factor(c("Spring", "Summer", "Fall", "Winter"),
                   levels = "Winter", "Spring", "Summer", "Fall")

#Mutate creates a new column in datD.2016 based on other columns, and case_when 
#allows for a tidier combination of if statements. Together, these allow us to add
#a column that lists season based on day of year and our previously defined bounds.
datD.2016 <- datD.2016 %>% 
  mutate(
    szn = case_when(
      doy >= leap.szn[1] & doy < leap.szn[2] ~ szn.name[1],
      doy >= leap.szn[2] & doy < leap.szn[3] ~ szn.name[2],
      doy >= leap.szn[3] & doy < leap.szn[4] ~ szn.name[3],
      doy >= leap.szn[4] | doy < leap.szn[1] ~ szn.name[4],
    )
  )
#It is now very simple to create a violin plot using this new column.
ggplot(data = datD.2016, aes(szn,discharge, color = szn)) + 
  geom_violin() +
  labs(title="Distribution of Daily Discharge Rates in 2016",
       x ="Season", y = expression(paste("Discharge ft"^"3 ","sec"^"-1"))) +
  theme(legend.position = "none")

#We can now do the same thing with 2017 data.
datD.2017 <- datD[datD$year == 2017,]
datD.2017 <- datD.2017 %>% 
  mutate(
    szn = case_when(
      doy >= nleap.szn[1] & doy < nleap.szn[2] ~ szn.name[1],
      doy >= nleap.szn[2] & doy < nleap.szn[3] ~ szn.name[2],
      doy >= nleap.szn[3] & doy < nleap.szn[4] ~ szn.name[3],
      doy >= nleap.szn[4] | doy < nleap.szn[1] ~ szn.name[4],
    )
  )
ggplot(data = datD.2017, aes(szn,discharge, color = szn)) + 
  geom_violin() +
  labs(title="Distribution of Daily Discharge Rates in 2017",
       x ="Season", y = expression(paste("Discharge ft"^"3 ","sec"^"-1"))) +
  theme(legend.position = "none")