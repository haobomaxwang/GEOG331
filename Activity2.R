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
