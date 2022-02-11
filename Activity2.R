# activity 2: 


#Vector

#make a vector of tree heights in meters
heights <- c(30,41,20,22)
#convert to cm
heights_cm <- heights*100
heights_cm

#set up a matrix with 2 columns and fill in by rows
#first argument is the vector of numbers to fill in the matrix
Mat<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=TRUE)
Mat

#set up a matrix that fills in by columns
#first argument is the vector of numbers to fill in the matrix
Mat.bycol<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=FALSE)
Mat.bycol

# what's in the first column of the two matrices
Mat[ ,1]
Mat.bycol[ ,1]
# what's in the first role of the two matrices
Mat[1, ]
Mat.bycol[1, ]


#set working directory 
setwd("Z:\\students\\hwang\\DATA\\noaa_weather")
#readin the data
datW <- read.csv("Z:\\students\\hwang\\DATA\\noaa_weather\\2011124.csv",
                 stringsAsFactors = T)

#see the structure of the dataset
str(datW)






















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

#get the mean across all sites
#the by function is a list of one or more variables to index over.
#FUN indicates the function we want to use
#if you want to specify any function specific arguments use a comma and add them after the function
#here we want to use the na.rm arguments specific to 

averageTemp <- aggregate(datW$TAVE, by=list(datW$NAME), FUN="mean", na.rm= "TRUE")

#change the automatic output of column names to be more meaningful
#note that MAAT is a common abbreviation for 
#Mean Annual Air Temperature

colnames(averageTemp) <- c("Name", "MAAT")


#convert site name to numbers 
#convert level to number for factor data type
#you will have to reference the level output or look at the row of data to see the character designation.
datW$siteN <- as.numeric(datW$NAME)

























#make a histogram for the first site in our levels
hist(datW$TAVE[datW$siteN=="1"], freq = FALSE, 
     main = paste(levels(datW$NAME)[1]), xlab = "Average daily temperature",
     ylab = "Relative frequency", col="grey50",
     border="white")

## 
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









### to know the range of dataset 
range(datW$TAVE[datW$siteN==1], na.rm = TRUE)


#### thinking about normal distribution on the first plot

h1 <- hist(datW$TAVE[datW$siteN == 1],
           freq=FALSE, 
           main = paste(levels(datW$NAME)[1]),
           xlab = "Average daily temperature (degrees C)", 
           ylab="Relative frequency",
           col="grey50",
           border="white")

#the seq function generates a sequence of numbers that we can 
## use to plot the normal across the range of temperature values 
x.plot <- seq(-10,30, length.out = 100)

#the dnorm function will produce the probability density based on a mean and standard deviation.

y.plot <-  dnorm(seq(-10,30, length.out = 100),
                 mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
                 sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
y.scaled <- (max(h1$density)/max(y.plot)) * y.plot

#points function adds points or lines to a graph  
#the first two arguements are the x coordinates and the y coordinates.

points(x.plot,
       y.scaled, 
       type = "l", 
       col = "royalblue3",
       lwd = 4, 
       lty = 2)

##
## the blue dashed is the normal distribution using the 
# mean and standard deviation calculated from the data. 

