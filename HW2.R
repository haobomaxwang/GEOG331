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














