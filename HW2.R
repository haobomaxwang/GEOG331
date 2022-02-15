#HW2

#QUESTION 1
# There are 157849 enteries and 9 variables in this data sheet, so it's 157849 rows and 9 columns. 
#readin the data
datW <- read.csv("Z:\\students\\hwang\\DATA\\noaa_weather\\2011124.csv",
                 stringsAsFactors = T)

#see the structure of the dataset
str(datW)



#QUESTION 2
#what's the difference between factor data, numeric data, character data and interger data?
#factor: factors are used to represent categorical data. Factor data are
# stored as intergers and have labels (charactors and words or numbers)
# associated with these unique intergers. 

#example, if we're trying to categorize food calories level, R world 
# recognize that there are three levels but the ranking of the levels
# will go alphabetically 
food <- factor(c("low", "high", "medium", "high", "low", "medium", "high"))
levels(food)

# so commanding levels(food) would yield the result high>low>medium

# if we're putting in some numbers as factors, r will treat the numbers
# as characters and assign a number indicating their ranking
# from the smallest to the largest, starting
# from 1. 

f<-factor(c(3.4, 1.2, 5))
as.numeric(f)

# in this case, 3.4 will be assigned 2 because it's the second smallest
# so when you are trying to save the three numbers as number, 
# instead of 3.4, 1.2, and 5
# the newly saved vector set will contain 2, 1, 3




#numeric: Numeric data were treated as numbers and can include all real 
#numbers
samplenumbers <- as.numeric(c(1.1,1.2,1.3,1.4,1.5))

#Integers: a sebset of numeric data where only integers were used
sampleintegers_prime <- as.integer(c(2,3,5,7,11))


#Character: Data were treated as names inside the citation marks
# Can contain numbers, signs, and letters
samplecharacter<- c("My", "name", "is", "bla", "bla...")









#reformatting the date 
datW$dateF <-as.Date(datW$DATE, "%Y-%m-%d")

#saving the year to a separate column 
datW$year <-as.numeric(format(datW$dateF, "%Y"))



#find out all unique site names
unique(datW$NAME)


#look at the mean maximum temperature for Aberdeen
mean(datW$TMAX[datW$NAME== "ABERDEEN, WA US"], na.rm = TRUE)

# calculate the average temperature for all enteries 

datW$TAVE <- (datW$TMIN + datW$TMAX)/2

#convert site name to numbers 
#convert level to number for factor data type
#you will have to reference the level output or look at the row of data to see the character designation.
datW$siteN <- as.numeric(datW$NAME)




#qustion 3: 

hist(datW$TAVE[datW$siteN=="1"], freq = FALSE, 
     main = paste(levels(datW$NAME)[1]), xlab = "Average daily temperature",
     ylab = "Relative frequency", col="grey50",
     border="white")


# help(hist) 
# frequency if true then it shows real frequency (or number of occurance)
# if false then it describes relative frequency.

# main is the main title of the graph. xlab is the x label and ylab
# is y label.


# col describes the color used for the histogram bars and border is 
# the color of the border around the bars. 

# help(paste) this function Takes multiple elements from the multiple vectors 
# and concatenates them into a single element. 
# in this case, it will paste out the characters refered by the code:
# levels()datW$NAME)[1] which is aberdeen

#add mean line with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
# standard deviation line below the mean with red (tomato3) color
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) - 
         sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#add standard deviation line above the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) + 
         sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)










## to plot other graphs into a single plot
par(mfrow=c(2,2))

### plot siteN ==2 
h2<- hist(datW$TAVE[datW$siteN=="2"], freq = FALSE, 
     main = paste(levels(datW$NAME)[2]), xlab = "Average daily temperature",
     ylab = "Relative frequency", col="grey50",
     border="white")
abline(v= mean(datW$TAVE[datW$siteN==2], na.rm = TRUE), 
       col= "tomato3", lwd=3)
abline(v= mean(datW$TAVE[datW$siteN==2], na.rm = TRUE)- 
             sd(datW$TAVE[datW$siteN==2], na.rm = TRUE),
       col= "tomato3",
       lty = 3, 
       lwd=3)
abline(v= mean(datW$TAVE[datW$siteN==2], na.rm = TRUE)+ 
         sd(datW$TAVE[datW$siteN==2], na.rm = TRUE),
       col= "tomato3",
       lty = 3, 
       lwd=3)

x.plot2 <- seq(0,40, length.out = 100)

y.plot2 <-  dnorm(seq(0,40, length.out = 100),
                  mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE),
                  sd(datW$TAVE[datW$siteN == 2],na.rm=TRUE))

y.scaled2 <- (max(h2$density)/max(y.plot2)) * y.plot2

points(x.plot2,
       y.scaled2, 
       type = "l", 
       col = "royalblue3",
       lwd = 4, 
       lty = 2)















## plot siteN ==3
h3<- hist(datW$TAVE[datW$siteN=="3"], freq = FALSE, 
     main = paste(levels(datW$NAME)[3]), xlab = "Average daily temperature",
     ylab = "Relative frequency", col="black",
     border="white")
abline(v= mean(datW$TAVE[datW$siteN==3], na.rm = TRUE), 
       col= "tomato3", lwd=3)
abline(v= mean(datW$TAVE[datW$siteN==3], na.rm = TRUE)- 
         sd(datW$TAVE[datW$siteN==3], na.rm = TRUE),
       col= "tomato3",
       lty = 3, 
       lwd=3)
abline(v= mean(datW$TAVE[datW$siteN==3], na.rm = TRUE)+ 
         sd(datW$TAVE[datW$siteN==3], na.rm = TRUE),
       col= "tomato3",
       lty = 3, 
       lwd=3)






x.plot3 <- seq(-40,40, length.out = 100)

y.plot3 <-  dnorm(seq(-40,40, length.out = 100),
                  mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE),
                  sd(datW$TAVE[datW$siteN == 3],na.rm=TRUE))

y.scaled3 <- (max(h3$density)/max(y.plot3)) * y.plot3

points(x.plot3,
       y.scaled3, 
       type = "l", 
       col = "royalblue3",
       lwd = 4, 
       lty = 2)






## plot siteN ==4
h4<- hist(datW$TAVE[datW$siteN=="4"], freq = FALSE, 
     main = paste(levels(datW$NAME)[4]), xlab = "Average daily temperature",
     ylab = "Relative frequency", col="aquamarine",
     border="white")
abline(v= mean(datW$TAVE[datW$siteN==4], na.rm = TRUE), 
       col= "tomato3", lwd=3)
abline(v= mean(datW$TAVE[datW$siteN==4], na.rm = TRUE)- 
         sd(datW$TAVE[datW$siteN==4], na.rm = TRUE),
       col= "tomato3",
       lty = 3, 
       lwd=3)
abline(v= mean(datW$TAVE[datW$siteN==4], na.rm = TRUE)+ 
         sd(datW$TAVE[datW$siteN==4], na.rm = TRUE),
       col= "tomato3",
       lty = 3, 
       lwd=3)



x.plot4 <- seq(0,40, length.out = 100)

y.plot4 <-  dnorm(seq(0,40, length.out = 100),
                  mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE),
                  sd(datW$TAVE[datW$siteN == 4],na.rm=TRUE))

y.scaled4 <- (max(h4$density)/max(y.plot4)) * y.plot4

points(x.plot4,
       y.scaled4, 
       type = "l", 
       col = "royalblue3",
       lwd = 4, 
       lty = 2)





## plot siteN ==5
h5<- hist(datW$TAVE[datW$siteN=="5"], freq = FALSE, 
     main = paste(levels(datW$NAME)[5]), xlab = "Average daily temperature",
     ylab = "Relative frequency", col="cyan2",
     border="white")
abline(v= mean(datW$TAVE[datW$siteN==5], na.rm = TRUE), 
       col= "tomato3", lwd=3)
abline(v= mean(datW$TAVE[datW$siteN==5], na.rm = TRUE)- 
         sd(datW$TAVE[datW$siteN==5], na.rm = TRUE),
       col= "tomato3",
       lty = 3, 
       lwd=3)
abline(v= mean(datW$TAVE[datW$siteN==5], na.rm = TRUE)+ 
         sd(datW$TAVE[datW$siteN==5], na.rm = TRUE),
       col= "tomato3",
       lty = 3, 
       lwd=3)





x.plot5 <- seq(-30,30, length.out = 100)

y.plot5 <-  dnorm(seq(-30,30, length.out = 100),
                  mean(datW$TAVE[datW$siteN == 5],na.rm=TRUE),
                  sd(datW$TAVE[datW$siteN == 5],na.rm=TRUE))

y.scaled5 <- (max(h5$density)/max(y.plot5)) * y.plot5

points(x.plot5,
       y.scaled5, 
       type = "l", 
       col = "royalblue3",
       lwd = 4, 
       lty = 2)






#question 6 

qnorm(0.95,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE)+4,
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

# answer is 22.51 meaning that the 0.95 level is now 22.51 degree


## calculating the pnorm for old threshold 
1-pnorm(18.51026, mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE)+4,
        sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

# answer is 0.203
# meaning that there will be 20% chance that the average temperature of a given day will be greater 
# than the previous threshold. 





# question 7 precipitation of alberdeen 
precalb<- hist(datW$PRCP[datW$siteN==1], na.rm=TRUE, freq=FALSE, col = "grey50", 
               main = paste(levels(datW$siteN==1)), xlab = "Precipitation", ylab = "Relative Frequency")


# the shape is best described by exponential distribution which is a special case under gamma distribution





## question 8 
# calculate the annual precipitation for alberdeen 

alberdeen<- datW[datW$siteN==1, ]

annualprec_alber<- aggregate(alberdeen$PRCP, by=list(alberdeen$year),FUN="sum",na.rm=TRUE)

colnames(annualprec_alber) <- c("Year","Annual Precipitation")

precalber_hist <- hist(annualprec_alber$`Annual Precipitation`, freq = FALSE, main = "Annual Precipitation for Alberdeen", 
     xlab = "Precipitation in mm", 
     ylab = "Relative Frequency", col = "grey50")

## this looks like it's normally distributed
x.precalber <- seq(1000,3000, length.out= 100)
y.precalber <- dnorm(seq(1000,3000, length.out=100), 
                     mean(annualprec_alber$`Annual Precipitation`, na.rm = TRUE),
                     sd(annualprec_alber$`Annual Precipitation`, na.rm = TRUE))
y.precalber_scaled <- (max(precalber_hist$density)/max(y.precalber))* y.precalber

points(x.precalber,
       y.precalber_scaled, 
       type = "l", 
       col = "royalblue3",
       lwd = 4, 
       lty = 2)



##question 9

range(datW$year)
# the answer is 1930 to 2019
# to calculate the mean annual precipitation of a certain site, we can add up all the precipitation data 
# of that site and divide the sum by 90 years


sum_prec <- aggregate(datW$PRCP, by=list(datW$NAME), FUN="sum", na.rm= "TRUE")
annual_prec <- sum_prec/90

# FOR ALBERDEEN: 2107.077 mm 

# for livermore: 358.3578 mm

# for Mandan :415.7189 mm

# for Mormon :312.6600 mm

# for morrisville: 946.2300 mm


## to calculate the temperature for each site
averageTemp <- aggregate(datW$TAVE, by=list(datW$NAME), FUN="mean", na.rm= "TRUE")

# ALBERDEEN: 10.432268 deg C. This site has a cold and wet climate.  

#LIVERMORE, CA US: 15.381992 deg C. This site has a temporate and dry climate. 

#MANDAN EXPERIMENT STATION, ND US: 5.568997 deg C. This site has a cold and dry climate. 

#MORMON FLAT, AZ US: 22.019010 deg C. This site has a warm and dry climate. 

#MORRISVILLE  SW, NY US: 6.655229 deg C. This site has a cold and moist climate. 
