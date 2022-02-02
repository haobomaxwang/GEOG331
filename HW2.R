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

