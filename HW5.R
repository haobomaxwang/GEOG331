# read in csv

# na values were recorded as eqp so we need to specify that
datH<- read.csv("Z:\\students\\hwang\\DATA\\streamflow\\stream_flow_data.csv",
                na.strings = c("Eqp"))

head(datH)

# read in precipitation data
datP <- read.csv("Z:\\students\\hwang\\DATA\\streamflow\\2049867.csv")                            
head(datP)
# HPCP stands for hourly precipitation in mm



#load in lubridate
library(lubridate)

# subset the dataset by only including the entries when 
# discharge.flag is "A"
# this will be the datasheet of our interest
datD<- datH[datH$discharge.flag=="A", ]

#### define time for streamflow #####
#convert date and time from the datasheet, using as.Date
# function

# because the date format in the dataset is month day year
# we specify "%m/%d/%Y" after the comma

datesD <- as.Date(datD$date, "%m/%d/%Y")
#get day of year
datD$doy <- yday(datesD)
#calculate year
datD$year <- year(datesD)
# define time
timesD<- hm(datD$time)

# create a new column in datD data for hours by adding
# hour and minute 
datD$hour <- hour(timesD) + minute(timesD)/60

# decimal day equals day plus hour
datD$decday <- datD$doy + datD$hour/24 

# decimal year equals year plus day 
# account for leap year

datD$decyear <- ifelse(leap_year(datD$year), datD$year + 
                         datD$decday/366, datD$year +
                          datD$decday/365)

# then repeat the whole procedure for precipitation data

dateP <- ymd_hm(datP$DATE)
#get day of year
datP$doy <- yday(dateP)
#get year 
datP$year <- year(dateP)

# decimal day and decimal year for precipitation
#calculate times for datP                       
datP$hour <- hour(dateP ) + (minute(dateP )/60)
#get full decimal time
datP$decDay <- datP$doy + (datP$hour/24)
#calculate a decimal year, but account for leap year
datP$decYear <- ifelse(leap_year(datP$year),datP$year + (datP$decDay/366),
                       datP$year + (datP$decDay/365)) 

#plot discharge by decimal year
plot(datD$decyear, datD$discharge, type="l", xlab="Year", 
         ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))

###### to plot the daily average discharge vs day of year
aveF<- aggregate(datD$discharge, by=list(datD$doy), FUN="mean")

# rename the colomn name to doy and discharge
colnames(aveF)<- c("doy", "dailyAve")

# standard deviation
sdF <- aggregate(datD$discharge, by=list(datD$doy), FUN="sd")
colnames(sdF)<- c("doy","dailySD")


### question 5###
###

### use the line function to make the line for 2017
lines(datD$doy[datD$year=="2017"], 
      datD$discharge[datD$year=="2017"], type = "l",
      lty=1, col="red")

### to get the range of the 2017 discharge

disc2017<- datD$discharge[datD$year=="2017"]
range(disc2017)
## 1.22 160.00
## thus the upper limit of the y axis should be 
## at least 160


### remake the plot by changing the y limit to 180

par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="month", main = "Time of the year vs Discharge",
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,180),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)# dont show axis, save it for later
#show standard deviation around the mean
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)
### readjust the axis
### change the x axis label from doy to month
axis(1, seq(0,360, by=31), #tick intervals
     labels=c(1:12)) #tick labels
axis(2, seq(0,180, by=30),
     seq(0,180, by=30),
     las = 2)#show ticks at

legend("topright", c("mean","2017", "1 standard deviation"), #legend items
       lwd=c(1,1,NA),#lines
       col=c("black","red", rgb(0.392, 0.584, 0.929,.2)),#colors
       pch=c(NA,NA,15),#symbols
       bty="n")#no legend border


### make the line for 2017
lines(datD$doy[datD$year=="2017"], 
      datD$discharge[datD$year=="2017"], type = "l",
      lty=1, col="red", lwd=1)



############### question 7################
# to know that which day has the full 24 hour record,
# to need to find 24 consecutive numbers in the doy
# column. 
# first we need to create a data frame to save the 
# days with full 24h measurment 

rainyday <- data.frame()

# then we need to ask r to check the roy field one 
# entry at a time and check the whole dataset
# we know that if doy of an entry has the same value 
# as the doy 23 entries later, then there must be 24 
# entries for that day.

# we also know that doy is the fifth column in the 
# dataset, then we can just ask r to compare the entry 
# number of the fifth column. 
for (i in 1:nrow(datP)) { #asking r to go through the whole dataset
  if (datP[i,5]== datP[i+23,5]) #comparing number of doy 23 rows away
  {d<- (datP[i, ]) # save that entry 
  rainyday=rbind(rainyday,d)} # combine them to the newly created dataset
}

### the result is a dataframe with all the days that
# have 24 measurments



#### question 8####

#### for the 7th day in 2009, which has 24 measurements

#subsest discharge and precipitation within range of interest
hydroD <- datD[datD$doy >= 7 & datD$doy < 8 & datD$year == 2009,]
hydroP <- datP[datP$doy >= 7& datP$doy < 8 & datP$year == 2009,]


min(hydroD$discharge)

#get minimum and maximum range of discharge to plot
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
plot(hydroD$decday,
     hydroD$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP)){
  polygon(c(hydroP$decDay[i]-0.017,hydroP$decDay[i]-0.017,
            hydroP$decDay[i]+0.017,hydroP$decDay[i]+0.017),
          c(yl,hydroP$pscale[i],hydroP$pscale[i],yl),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}



### question 9###


### subset month 
datD$month <- month(datesD)

## create a new dataset for 2016 
discharge2016 <- datD[datD$year==2016, ]

## add a new column that indicates the season
for (i in 1:nrow(discharge2016)){

if(discharge2016[i,15]== 3){
   discharge2016[i,16]<- "spring"
} else if (discharge2016[i,15]==4) {
  discharge2016[i,16]<- "spring"
}else if (discharge2016[i,15]==5){
  discharge2016[i,16] <- "spring"
}

}

# do the same thing for summer 

for (i in 1:nrow(discharge2016)){
  
  if(discharge2016[i,15]== 6){
    discharge2016[i,16]<- "summer"
  } else if (discharge2016[i,15]==7) {
    discharge2016[i,16]<- "summer"
  }else if (discharge2016[i,15]==8){
    discharge2016[i,16] <- "summer"
  }
  
}

## do the same thing to fall September, October, November 
for (i in 1:nrow(discharge2016)){
  
  if(discharge2016[i,15]== 9){
    discharge2016[i,16]<- "fall"
  } else if (discharge2016[i,15]==10) {
    discharge2016[i,16]<- "fall"
  }else if (discharge2016[i,15]==11){
    discharge2016[i,16] <- "fall"
  }
  
}

## winter 2016 Dec Jan Feb
for (i in 1:nrow(discharge2016)){
  
  if(discharge2016[i,15]== 12){
    discharge2016[i,16]<- "winter"
  } else if (discharge2016[i,15]==1) {
    discharge2016[i,16]<- "winter"
  }else if (discharge2016[i,15]==2){
    discharge2016[i,16] <- "winter"
  }
  
}

# rename the column
names(discharge2016)[names(discharge2016) == "V16"] <- "season"


## make the violin plot for 2016 
#make a violin plot
library(ggplot2)
plot2016<- ggplot(data= discharge2016, aes(season,discharge)) + 
  geom_violin()

plot2016 + ggtitle("Discharge in 2016 by season")



## repeat the whole process for 2017

## create a new dataset for 2017 
discharge2017 <- datD[datD$year==2017, ]

## add a new column that indicates the season
for (i in 1:nrow(discharge2017)){
  
  if(discharge2017[i,15]== 3){
    discharge2017[i,16]<- "spring"
  } else if (discharge2017[i,15]==4) {
    discharge2017[i,16]<- "spring"
  }else if (discharge2017[i,15]==5){
    discharge2017[i,16] <- "spring"
  }
  
}

# do the same thing for summer 

for (i in 1:nrow(discharge2017)){
  
  if(discharge2017[i,15]== 6){
    discharge2017[i,16]<- "summer"
  } else if (discharge2017[i,15]==7) {
    discharge2017[i,16]<- "summer"
  }else if (discharge2017[i,15]==8){
    discharge2017[i,16] <- "summer"
  }
  
}

## do the same thing to fall September, October, November 
for (i in 1:nrow(discharge2017)){
  
  if(discharge2017[i,15]== 9){
    discharge2017[i,16]<- "fall"
  } else if (discharge2017[i,15]==10) {
    discharge2017[i,16]<- "fall"
  }else if (discharge2017[i,15]==11){
    discharge2017[i,16] <- "fall"
  }
  
}

## winter 2017 Dec Jan Feb
for (i in 1:nrow(discharge2017)){
  
  if(discharge2017[i,15]== 12){
    discharge2017[i,16]<- "winter"
  } else if (discharge2017[i,15]==1) {
    discharge2017[i,16]<- "winter"
  }else if (discharge2017[i,15]==2){
    discharge2017[i,16] <- "winter"
  }
  
}

# rename the column
names(discharge2017)[names(discharge2017) == "V16"] <- "season"


## make the violin plot for 2017 
#make a violin plot
library(ggplot2)
plot2017<- ggplot(data= discharge2017, aes(season,discharge)) + 
  geom_violin()

plot2017 + ggtitle("Discharge in 2017 by season")

