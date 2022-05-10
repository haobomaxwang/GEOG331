
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

abline(h=12, col="yellow")
abline(h=35, col="red")
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

avePM2.5 <- merge(PM2.5_2017dailyave,
                  PM2.5_2018dailyave,
                  by = "doy")
avePM2.5<- merge(avePM2.5,
                 PM2.5_2019dailyave,
                 by="doy")
avePM2.5<- merge(avePM2.5,
                 PM2.5_2021dailyave,
                 by="doy")
# now we have a dataframe that has 2017,
# 2018, 2019, 2021 data
# change column names
colnames(avePM2.5)<- c("doy", "date2017",
                       "ave2017", "date2018",
                       "ave2018", "date2019",
                       "ave2019", "date2021", 
                       "ave2021")

## calculate the average
avePM2.5$ave4years <- (avePM2.5$ave2017+
                         avePM2.5$ave2018+
                         avePM2.5$ave2019+
                         avePM2.5$ave2021)/4

## plot the average and then 2020
dev.new(width=8, height=8)
par(mai=c(1,1,1,1))

plot(avePM2.5$doy, avePM2.5$ave4years,
     main = "Date vs mean pm2.5", 
     xlab = "Date as day of year", 
     ylab = "PM2.5 µg/m³",
     type= "l", lty= 3, 
     col= "blue",
     ylim= c(0,40)
)

lines(PM2.5_2020dailyave$doy, 
      PM2.5_2020dailyave$avePM2.5, type = "l",
      lty=1, col="green")

legend("topright", c("Four-year average", "2020"), #legend items
       lty=c(3,1), #line types
       lwd=c(1,1),#lines width
       col=c("blue", "green"), #colors
       pch=c(NA,NA),#symbols
       bty="n")#no legend border

abline(h=12, col="yellow")
abline(h=35, col="red")

## interesting, it seems like most days in
# 2020 have lower PM 2.5 values than 4 year 
# average 


pm2.52020<- PM2.5_2020dailyave$avePM2.5
pm2.54yearave<- avePM2.5$ave4years
sd(pm2.52020)
# 3.45
sd(pm2.54yearave)
# 2.16

# shapiro test 
shapiro.test(pm2.52020)
# p-value= 1.7e-15

shapiro.test(pm2.54yearave)
# p-value= 1.142e-10


# plot a histogram 
dev.new(width=8, height=8)
par(mai=c(1,1,1,1))

hist(pm2.52020, breaks=20,
     col=rgb(0,0,1,0.2), 
     xlim = c(0,30), ylim = c(0,100),
     xlab= "PM2.5 µg/m³",
     main = "2020 vs Four year average (2017-19, 2021)")

hist(pm2.54yearave, breaks=20,
     col=rgb(1,0,0,0.2), add=TRUE)

legend("topright", c("2020", "Four year average"), #legend items
       col=c(rgb(0,0,1,0.2), rgb(1,0,0,0.2)), #colors
       pch=c(15,15),#symbols
       bty="n")#no legend border

# note that both histograms are not 
# normally distributed
t.test(pm2.52020, pm2.54yearave)

# we have data:  pm2.52020 and pm2.54yearave
# t = -3.8898, df = 613.71, p-value =
#  0.0001113
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -1.2469091 -0.4102558
# sample estimates:mean of x mean of y 
# 6.505896  7.334479 




###
# plotting traffic count data with PM2.5 pollution
Traffic<- read.csv("Z:/students/hwang/github/GEOG331/research project/data/Traffic_Volume_Counts__2014-2020_NYC.csv")

## sum the hourly count into 24 hour count for each site
Traffic$sum<- rowSums(Traffic[, c(8:31)])

## aggregate the average traffic sum by day.
Traffic_dailyave<- aggregate(Traffic$sum,
                             by= list(Traffic$Date),
                             FUN= "mean",
                             na.rm= TRUE
)
library(lubridate)
colnames(Traffic_dailyave)<- c("Date", "Traffic_count")
traffic_date<- as.Date(Traffic_dailyave$Date, "%m/%d/%Y")
Traffic_dailyave$year<- year(traffic_date)
Traffic_dailyave$doy<- yday(traffic_date)
## subset 2020
Traffic2020<- Traffic_dailyave[Traffic_dailyave$year==2020, ]

## merge pm2.5 to traffic data
merged2020<- merge(Traffic2020, PM2.5_2020dailyave,
      by= "doy")

# plot the graph
plot(merged2020$Traffic_count, merged2020$avePM2.5,
     xlab = "Traffic count", ylab = "PM2.5 µg/m³")

# linear model
LMtraffic_pm2.5<- lm(merged2020$avePM2.5~merged2020$Traffic_count)

summary(LMtraffic_pm2.5)

# significant but negatively correlated