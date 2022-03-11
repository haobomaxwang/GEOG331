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

# we see that sepal length and sepal width are the 1st 
# and 2nd column in datasheet iris. And petal length and
# petal width the 3rd and 4th column


# create a list to store the linear models
sepal_lw_petal_lw<- list()

# lm function works as lm(y, x)
# so i+1 is the column rightside to i and thus is the y variable
# we only need i to be the 1st and 3rd column 
# thus, we specify that i is from c(1,3)

for(i in c(1,3)){
  linear_models<- lm(iris[,i+1][iris$Species=="versicolor"]~
       iris[,i][iris$Species=="versicolor"])
  #store the summary of linear_models to the list
  sepal_lw_petal_lw[[i]]<- summary(linear_models)
}

# clicking open the sepal_lw_petal_lw list 
# we can see that sepal length vs wide has an
# intercept of 0.87215 and correlation coefficient of 0.31972

# petal length vs wide has an interception of -0.08429 and
# correlation coefficient of 0.33105



# then let's look at sepal length and petal length 
# they are located on the 1st and 3rd column

# create a list for sepal length vs petal length 
sepal_length_petal_length<- list()

# because sepal length is the 1st column, i=1 refers to
# that column, and i+2 refers to the 3rd column which 
# is petal length

for (i in c(1)){
  linear_models2<- lm(iris[,i+2][iris$Species=="versicolor"]~
                       iris[,i][iris$Species=="versicolor"])
  
  sepal_length_petal_length[[i]]<- summary(linear_models2)
}
  
# we see sepal legnth and petal length has an intercept of
# 0.18512 and a correlation coefficient of 0.68647






#####################################
##### Part 2: data in dplyr     #####
#####################################

#use dplyr to join data of maximum height
#to a new iris data frame
height <- data.frame(Species = c("virginica","setosa","versicolor"),
                     Height.cm = c(60,100,11.8))


# create a new iris datasheet called iris1 
# and join this with height

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