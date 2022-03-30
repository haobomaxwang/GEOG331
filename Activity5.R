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

#You can improve the plot by using a few 
# plotting arguments. 
#The dev.new function will start a new plot window 
# with a standard size.

#start new plot
dev.new(width=8,height=8)


### reset the y limit to include the standard deviation 
### and use polygon to show the standard deviation
#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="DOY", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)# dont show axis, save it for later
#show standard deviation around the mean
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)
### readjust the axis
axis(1, seq(0,360, by=40), #tick intervals
     lab=seq(0,360, by=40)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
### legend
#legend("topright", c("mean","1 standard deviation"), #legend items
#       lwd=c(2,NA),#lines
#       fill=c(NA,rgb(0.392, 0.584, 0.929,.2)),#fill boxes
#       border=NA,#no border for both fill boxes (don't need a vector here since both are the same)
#       bty="n")#no legend border

### or you can use this legend notation

legend("topright", c("mean", "1 standard deviation"), #legend items
       lwd=c(2,NA),#lines
       col=c("black", rgb(0.392, 0.584, 0.929,.2)),#colors
       pch=c(NA,15),#symbols
       bty="n")#no legend border
  

### make the line for 2017
lines(datD$doy[datD$year=="2017"], 
     datD$discharge[datD$year=="2017"], type = "l",
     lty=1, col="red")
### to get the range of the 2017 discharge

disc2017<- datD$discharge[datD$year=="2017"]
range(disc2017)
## 1.22 160.00
## thus the upper limit of the y axis should be 
## at least 160







### remake the plot

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
### to get the range of the 2017 discharge

disc2017<- datD$discharge[datD$year=="2017"]
range(disc2017)
## 1.22 160.00
## thus the upper limit of the y axis should be 
## at least 160




### to know which day has consecutive 
## 24 h measurement 

#rainydays2007<- data.frame()

#for (i in 1:366) {
#  d<- datP[datP$doy==i,] #[datP$year=="2007"]
# if (nrow(d)==24){
#   rainydays2007=rbind(rainydays,d)
# }  
#}

###############
rainyday <- data.frame()

for (i in 1:nrow(datP)) {
  if (datP[i,5]== datP[i+23,5])
  {d<- (datP[i, ])
  rainyday=rbind(rainyday,d)} 
}






#### 2007 296 day
#subsest discharge and precipitation within range of interest
hydroD <- datD[datD$doy >= 296 & datD$doy < 297 & datD$year == 2007,]
hydroP <- datP[datP$doy >= 296 & datP$doy < 297 & datP$year == 2007,]


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



### box plot and violin plot
library(ggplot2)
#specify year as a factor
datD$yearPlot <- as.factor(datD$year)
#make a boxplot
ggplot(data= datD, aes(yearPlot,discharge)) + 
  geom_boxplot()

#make a violin plot
ggplot(data= datD, aes(yearPlot,discharge)) + 
  geom_violin()