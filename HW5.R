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