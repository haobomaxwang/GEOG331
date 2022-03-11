#use built in iris dataset
#take a look at it 
head(iris)
#load in some tidyverse packages
library(dplyr)
library(ggplot2)

#####################################
##### Part 1: for loops         #####
#####################################

#Using only data for iris versicolor
#write a for loop
#that produces a regression table
#for each of the following relationships
#1. iris  sepal length x width
#2. iris  petal length x width
#3. iris sepal length x petal length

# hint: consider using a list, and also new vectors 
# for regression variables


# to print the answers

for(i in c(1,3)){
   print(iris[ , i][iris$Species=="versicolor"]
           *iris[ , i+1][iris$Species=="versicolor"])
}

# this is for sepal length * sepal width and petal length * petal width
# because the columns used in each calculation are next to each 
# other, we can use i and i+1 while i has the value 1 or 3.



for(i in c(1)){
  print(iris[ , i][iris$Species=="versicolor"]
        *iris[ , i+2][iris$Species=="versicolor"])
}

# this is to calculate sepel length * petal length 
# we use i and i +2, and i has a pre-determined number 1
# because sepel length is the first column



# to save the answer??????



for(i in c(1,3)){
  list=(iris[ , i][iris$Species=="versicolor"]
        *iris[ , i+1][iris$Species=="versicolor"])
}
# the problem is the list world only record the last calculation




#####################################
##### Part 2: data in dplyr     #####
#####################################

#use dplyr to join data of maximum height
#to a new iris data frame
height <- data.frame(Species = c("virginica","setosa","versicolor"),
                     Height.cm = c(60,100,11.8))


# new iris 
iris1<-left_join(iris, height, 
                    )
# Joining, by = "Species"

# the new data sheet called iris1 now includes the maximum height 




#####################################
##### Part 3: plots in ggplot2  #####
#####################################

#look at base R scatter plot
plot(iris$Sepal.Length,iris$Sepal.Width)

#3a. now make the same plot in ggplot
# basic template: 
# ggplot(data = <DATA>, mapping = aes(<MAPPINGS>)) +  
#                           <GEOM_FUNCTION>()

## Assign plot to a variable 
plot1<- ggplot(data = iris1, 
       mapping = aes(x = Sepal.Length, y = Sepal.Width))
  
## draw the plot
plot1 + 
  geom_point()

# note that 
# # This is the correct syntax for adding layers
#     surveys_plot +
#          geom_point()

# This will not add the new layer and 
# will return an error message
#      surveys_plot
#          + geom_point()
# so the + sign should go with the plot instead of the 
#added layer


#3b. make a scatter plot with ggplot and get rid of
# busy grid lines
plot1 + 
  geom_point()+ 
  
# this will remove the background color
theme_bw()+ 
# this will remove the grid lines
theme(panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank())


#3c. make a scatter plot with ggplot, remove grid lines, 
# add a title and axis labels, 
# show species by color, 
# and make the point size proportional to petal length



plot1 + 
  geom_point()+ 
  
  # this will remove the background color
  theme_bw()+ 
  # this will remove the grid lines
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
# title 
ggtitle("Sepal Length vs Sepal Width")+
# x label
xlab("Sepal Length cm")+
# y label
ylab("Sepal Width cm")+

# show species by color 
aes(color = Species)+

# point size porpotional to petal length 
aes(size = Petal.Length)


#####################################
##### Question: how did         #####
##### arguments differ between  #####
##### plot and ggplot?          #####
#####################################	