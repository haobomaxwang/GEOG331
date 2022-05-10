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

## subset 2020
Traffic2020<- Traffic_dailyave[Traffic_dailyave$year==2020, ]

## merge pm2.5 to traffic data
