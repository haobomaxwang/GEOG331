


#create a function. The names of the arguments for your function will be in parentheses. 
#Everything in curly brackets will be run each time the function is run.
assert <- function(statement,err.message){
  #if evaluates if a statement is true or false for a single item
  if(statement == FALSE){
    print(err.message)
  }
  
}

#check how the statement works
#evaluate a false statement
assert(1 == 2, "error: unequal values")

#evaluate a true statement
assert(2 == 2, "error: unequal values")

#set up assert to check if two vectors are the same length
a <- c(1,2,3,4)
b <- c(8,4,5)
assert(length(a) == length(b), "error: unequal length")


#read in the data file
#skip the first 3 rows since there is additional column info
#specify the the NA is designated differently
datW <- read.csv("Z:/students/hwang/DATA/bewkes/bewkes_weather.csv",
                 na.strings = c("#N/A"), skip = 3, header = FALSE)

#preview data
print(datW[1,])


#get sensor info from file
# this data table will contain all relevant units
# only read in the first two rows 

sensorInfo <- read.csv("Z:/students/hwang/DATA/bewkes/bewkes_weather.csv",
                         na.strings=c("#N/A"), nrows=2)

print(sensorInfo)

#now that the column names for sensorInfo data is good
# let's use it on the main data

#get column names from sensorInfo table
# and set weather station colnames  to be the same
colnames(datW) <-   colnames(sensorInfo)
#preview data
print(datW[1,])









###### to clean up the date format
#use install.packages to install lubridate
#    install.packages(c("lubridate"))
# only need to download once for each computer 


# load the package to the working environment
# library(lubridate)

#convert to standardized format
#date format is m/d/y
dates <- mdy_hm(datW$timestamp, tz= "America/New_York")

#calculate day of year
datW$doy <- yday(dates)
#calculate hour in the day
datW$hour <- hour(dates) + (minute(dates)/60)
#calculate decimal day of year
datW$DD <- datW$doy + (datW$hour/24)
#quick preview of new date calculations
datW[1,]








# checking missing data
####
# The is.na function is meant to return a TRUE/FALSE answer
# where TRUE indicates a NA is present. 
# The which function indicates all elements in the vector 
# where the items in the function return a TRUE value. 
# If you remember from last class length returns the 
# length of the vector. 

#see how many values have missing data for each sensor observation
#air temperature
length(which(is.na(datW$air.temperature)))
# the answer is 0

#wind speed
length(which(is.na(datW$wind.speed)))
# 0

#precipitation
length(which(is.na(datW$precipitation)))
# 0
# so these three are all good


#soil temperature
length(which(is.na(datW$soil.moisture)))
#soil moisture
length(which(is.na(datW$soil.temp)))

## the two soil measurement both have 707 NA

#make a plot with filled in points (using pch)
#line lines
plot(datW$DD, datW$soil.moisture, pch=10, 
     type="b", xlab = "Day of Year",
     ylab="Soil moisture (cm3 water per cm3 soil)")

# we'll see that the data for the second half of the year 
# is missing 



# lets plot something for air temperature
#make a plot with filled in points (using pch)
#line lines
plot(datW$DD, datW$air.temperature, pch=19, type="b", xlab = "Day of Year",
     ylab="Air temperature (degrees C)")
# the plot looks good 
# we know that the summer temperature data should not
# contain any value below 0

# using ifelse to change anything below 0 to NA
datW$air.tempQ1 <- ifelse(datW$air.temperature < 0, NA, 
                          datW$air.temperature)





#check the values at the extreme range of the data
#and throughout the percentiles
quantile(datW$air.tempQ1)

# we got 
# 0%  25%  50%  75% 100% 
# 7.0 16.2 20.1 23.3 34.4 


#look at days with really low air temperature
datW[datW$air.tempQ1 < 8,]  
length(which(datW$air.tempQ1<8))
# there are 6 enteries where air temp is below 8

#look at days with really high air temperature
datW[datW$air.tempQ1 > 33,]  
length(which(datW$air.tempQ1 > 33))

# there are 8 enteries where air temp is above 33




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
