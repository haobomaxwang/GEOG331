
# read in csv
PM2.5_2017<- read.csv("Z:/students/hwang/github/GEOG331/research project/data/ad_viz_plotval_data_PM_2.5_NYC_2017.csv")
PM2.5_2018<- read.csv("Z:/students/hwang/github/GEOG331/research project/data/ad_viz_plotval_data_PM_2.5_NYC_2018.csv")
PM2.5_2019<- read.csv("Z:/students/hwang/github/GEOG331/research project/data/ad_viz_plotval_data_PM_2.5_NYC_2019.csv")
PM2.5_2020<- read.csv("Z:/students/hwang/github/GEOG331/research project/data/ad_viz_plotval_data_PM_2.5_NYC_2020.csv")
PM2.5_2021<- read.csv("Z:/students/hwang/github/GEOG331/research project/data/ad_viz_plotval_data_PM_2.5_NYC_2021.csv")

## each entry has multiple measurements on each day 
## from different sites. We want to calculate the mean
## for each day
PM2.5_2017dailyave<- aggregate(PM2.5_2017$Daily.Mean.PM2.5.Concentration,
                               by=list(PM2.5_2017$Date), FUN= "mean",
                               na.rm=TRUE)
colnames(PM2.5_2017dailyave)<- c("date","avePM2.5")

## do the same thing for 2018
PM2.5_2018dailyave<- aggregate(PM2.5_2018$Daily.Mean.PM2.5.Concentration,
                               by=list(PM2.5_2018$Date), FUN= "mean",
                               na.rm=TRUE)
colnames(PM2.5_2018dailyave)<- c("date","avePM2.5")

## 2019
PM2.5_2019dailyave<- aggregate(PM2.5_2019$Daily.Mean.PM2.5.Concentration,
                               by=list(PM2.5_2019$Date), FUN= "mean",
                               na.rm=TRUE)
colnames(PM2.5_2019dailyave)<- c("date","avePM2.5")

## 2020
PM2.5_2020dailyave<- aggregate(PM2.5_2020$Daily.Mean.PM2.5.Concentration,
                               by=list(PM2.5_2020$Date), FUN= "mean",
                               na.rm=TRUE)
colnames(PM2.5_2020dailyave)<- c("date","avePM2.5")

## 2021
PM2.5_2021dailyave<- aggregate(PM2.5_2021$Daily.Mean.PM2.5.Concentration,
                               by=list(PM2.5_2021$Date), FUN= "mean",
                               na.rm=TRUE)
colnames(PM2.5_2021dailyave)<- c("date","avePM2.5")




## plan: plot pm2.5 vs day of year? date? 
# for all the years and look if 2020 differs 

## to plot the pm2.5 vs date,
# we need to get the day of year first
library(lubridate)

date2017<- as.Date(PM2.5_2017dailyave$date, "%m/%d/%Y")
PM2.5_2017dailyave$doy<- yday(date2017)

date2018<- as.Date(PM2.5_2018dailyave$date, "%m/%d/%Y")
PM2.5_2018dailyave$doy<- yday(date2018)

date2019<- as.Date(PM2.5_2019dailyave$date, "%m/%d/%Y")
PM2.5_2019dailyave$doy<- yday(date2019)

date2020<- as.Date(PM2.5_2020dailyave$date, "%m/%d/%Y")
PM2.5_2020dailyave$doy<- yday(date2020)

date2021<- as.Date(PM2.5_2021dailyave$date, "%m/%d/%Y")
PM2.5_2021dailyave$doy<- yday(date2021)

## plot the graph for 2017 first 
dev.new(width=8, height=8)
par(mai=c(1,1,1,1))

plot(PM2.5_2017dailyave$doy, PM2.5_2017dailyave$avePM2.5,
     main = "Date vs mean pm2.5", 
     xlab = "Date as day of year", 
     ylab = "PM2.5 µg/m³",
     type= "l", lty= 3, 
     col= "magenta4",
    ylim= c(0,40)
     )

lines(PM2.5_2018dailyave$doy, 
      PM2.5_2018dailyave$avePM2.5, type = "l",
      lty=4, col="red")
lines(PM2.5_2019dailyave$doy, 
      PM2.5_2019dailyave$avePM2.5, type = "l",
      lty=2, col="blue")
lines(PM2.5_2020dailyave$doy, 
      PM2.5_2020dailyave$avePM2.5, type = "l",
      lty=1, col="green", lwd=2)

legend("topright", c("2017","2018", "2019", "2020"), #legend items
       lty=c(3,4,2,1), #line types
       lwd=c(1,1,1,2),#lines width
       col=c("magenta4","red", "blue", "green"), #colors
       pch=c(NA,NA,NA,NA),#symbols
       bty="n")#no legend border

## Most studies indicate PM2.5 at or below 12 μg/m3 is 
# considered healthy with little to no risk from 
# exposure. If the level goes to or above 35 μg/m3 
# during a 24-hour period, the air is considered 
# unhealthy and can cause issues for people with 
# existing breathing issues such as asthma. 
# Prolonged exposure to levels above 50 μg/m3 can lead 
# to serious health issues and premature mortality.


## or, plot the average pm2.5 across multiple years 
## and then plot 2020 on top

