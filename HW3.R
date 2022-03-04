# read in csv for bewkes data
# we want to skip the first three lines because they are 
# descriptions 

# also we need to tell r that #N/A stands for NA

# we also don't want a header. If header = false 
# then r will not treat the first enterie as column names 

datW <- read.csv("Z:/students/hwang/DATA/bewkes/bewkes_weather.csv",
                 na.strings = c("#N/A"), skip = 3, header = FALSE)


# get sensor info from file
# this data table will contain all relevant units
# only read in the first two rows 

sensorInfo <- read.csv("Z:/students/hwang/DATA/bewkes/bewkes_weather.csv",
                       na.strings=c("#N/A"), nrows=2)

#get column names from sensorInfo table
# and set weather station colnames  to be the same
colnames(datW) <-   colnames(sensorInfo)

# now we have the column names of the original dataset 
# and get rid off irrelevant enteries




# then
# we need to create a assert function that would output an 
# error message if the condition is false 

# if the statement is true then nothing will happen
assert <- function(statement, err.message)
{if(statement==FALSE){print(err.message)}}



# then we need to transform the date into something 
# that could easily be plotted on the x axis 

###### to clean up the date format
#use install.packages to install lubridate
  install.packages(c("lubridate"))
# only need to download once for each computer 

# load the package to the working environment
library(lubridate)

#convert to standardized format
#date format is m/d/y /hour/ minute 
dates <- mdy_hm(datW$timestamp, tz= "America/New_York")
  
#the function yday calculates day of year
datW$doy <- yday(dates)

# hour () calculate hour in the day and minute() takes apart minute 
datW$hour <- hour(dates) + (minute(dates)/60)

#calculate decimal day of year
datW$DD <- datW$doy + (datW$hour/24)



#QA/QC
# we know that the summer temperature data should not
# contain any value below 0

# using ifelse to change anything below 0 to NA
datW$air.tempQ1 <- ifelse(datW$air.temperature < 0, NA, 
                          datW$air.temperature)


# next, look at the days when it's experiencing thunderstorm
# thunderstorms can potentially mess up the wind speed data

#normalize lightning with precipitation
lightscale <- (max(datW$precipitation)/max(datW$lightning.acvitivy)) * datW$lightning.acvitivy

# plot preticipation 
plot(datW$DD, datW$precipitation,
     xlab = "Day of Year", 
     ylab = "Precipitation & lightning",
     type="n")


# only plot when there are precipitation 
points(datW$DD[datW$precipitation > 0], datW$precipitation[datW$precipitation > 0],
       col= rgb(95/255,158/255,160/255,.5), pch=15)    

#plot lightning points only when there is lightning     
points(datW$DD[lightscale > 0], lightscale[lightscale > 0],
       col= "tomato3", pch=19)


assert(length(datW$precipitation) == length(lightscale),
       "error: unequal length")
assert(length(datW$DD)== length(lightscale), 
       "error: unequal length")

#filter out storms in wind and air temperature measurements
# filter all values with lightning that coincides with rainfall greater than 2mm or only rainfall over 5 mm.    
#create a new air temp column
datW$air.tempQ2 <- ifelse(datW$precipitation  >= 2 
                          & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, 
                                 datW$air.tempQ1))