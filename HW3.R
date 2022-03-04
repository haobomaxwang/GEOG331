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


#Homework question 5

assert(length(datW$precipitation) == length(lightscale),
       "error: unequal length")
assert(length(datW$DD)== length(lightscale), 
       "error: unequal length")

# no error messages showed up, which indicates that
# the lightscale vector has the same length as the data sheet


#HW question 6: 
#filter out storms in wind and air temperature measurements
# filter all values with lightning that coincides with rainfall greater than 2mm or only rainfall over 5 mm.    
#create a new air temp column
datW$air.tempQ2 <- ifelse(datW$precipitation  >= 2 
                          & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, 
                                 datW$air.tempQ1))


# remove the same entries from wind speed 
datW$wind.speedQ1 <- ifelse(datW$precipitation  >= 2 
                            & datW$lightning.acvitivy >0, NA,
                            ifelse(datW$precipitation > 5, NA, 
                                   datW$wind.speed))

# to see whether we've removed the same number of rows
# use the assert function on the length of NA values
# from the air.tempQ2 and wind.speedQ1

assert(length(which(is.na(datW$air.tempQ2)))==length(which(is.na(datW$wind.speedQ1)))
       , "error: unequal length")

## no error massage showed up, which means we're good


plot(datW$DD, datW$wind.speedQ1, pch=19, type="b", xlab = "Day of Year",
     ylab="wind speed m/s")




#question 7:

# to see whether the soil temperature make sense, we 
# can check the values against the air temperature 

# it would be great if we can plot soil temperature and
# air temperature on the same graph


plot(datW$DD, datW$air.tempQ1,
     xlab = "Day of Year", 
     ylab = "air and soil temperature in C",
     type="n")


# plot air temperature  
points(datW$DD, datW$air.tempQ1,
       col= rgb(95/255,158/255,160/255,.5), pch=15)    

#plot soil temperature     
points(datW$DD, datW$soil.temp,
       col= "tomato3", pch=19)


# question 8
# calculate the average air temperature (after removing skeptical data)
mean(datW$air.tempQ2, na.rm = TRUE)
# the answer is 20.00936 which is around 20.0 C

# to know how many values in air. tempQ2 is NA
length(which(is.na(datW$air.tempQ2)))
# the answer is 13 . Therefore the number of entries calculated 
# is 2118-13=2105

# calculate the average wind speed (after removing skeptical data)
mean(datW$wind.speedQ1, na.rm = TRUE)
# the answer is 0.445263 which is around 0.45 M/s
# because we removed the same rows from wind speed and
# air temperature, the number of rows for those two 
# columns should be the same. which is 2105

# calculate the average for soil moisture and temperature 
mean(datW$soil.moisture, na.rm = TRUE)
# we have 0.1445495 which is around 0.145 m^3/m^3

mean(datW$soil.temp, na.rm = TRUE)
# we have 17.41777 which is around 17.4 C

# we knew that there are 707 NAs in the soil measurements 
# therefore the sample number should be 2118-707=1411


# question 8
# calculate the average air temperature (after removing skeptical data)
mean(datW$air.tempQ2, na.rm = TRUE)
# the answer is 20.00936 which is around 20.0 C

# to know how many values in air. tempQ2 is NA
length(which(is.na(datW$air.tempQ2)))
# the answer is 13 . Therefore the number of entries calculated 
# is 2118-13=2105

# calculate the average wind speed (after removing skeptical data)
mean(datW$wind.speedQ1, na.rm = TRUE)
# the answer is 0.445263 which is around 0.45 M/s
# because we removed the same rows from wind speed and
# air temperature, the number of rows for those two 
# columns should be the same. which is 2105

# calculate the average for soil moisture and temperature 
mean(datW$soil.moisture, na.rm = TRUE)
# we have 0.1445495 which is around 0.145 m^3/m^3

mean(datW$soil.temp, na.rm = TRUE)
# we have 17.41777 which is around 17.4 C

# we knew that there are 707 NAs in the soil measurements 
# therefore the sample number should be 2118-707=1411


# for the air temperature and wind speed data, 
# we know that the measurement is reliable throughout the 
# measuring period.There are only 13 skeptical values 
# we changed in to NA
which(is.na(datW$air.tempQ2))
# from this code we know that the 13 values are 
[1]   67  300  974  975  976  979  980 1658 1659 1660 1670 1671
[13] 2039

# meaning that they are not clustered after some specific date
# so the active measuring period for the air T and wind Speed
# data is just from 2018.6.12 to 2018.7.26

# from the soil data graph we know that starting from some date
# all the soil data were NAs
# there are 707 NAs and in total 2118 observations 
# so the 1412th row in the data sheet should be the first NA

datW[1412, ]

# we have 

timestamp solar.radiation precipitation
1412 7/11/2018 21:00               2             0
lightning.acvitivy lightning.distance wind.dir wind.speed
1412                  0                  0      272       0.21
gust.speed air.temperature relative.humid
1412       0.41            16.9            0.6
atmospheric.pressure soil.moisture soil.temp doy hour
1412                 96.4            NA        NA 192   21
DD air.tempQ1 wind.speedQ1 air.tempQ2
1412 192.875       16.9         0.21       16.9

# so, until 2018.7.11 21:00 the soil data should be good

# for total precipitation: 
sum(datW$precipitation)
# we get 177.828 mm
