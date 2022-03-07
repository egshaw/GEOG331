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
iris.versi <- iris[iris$Species == "versicolor",]


vars.1.df <- data.frame(iris.versi$Sepal.Length, iris.versi$Petal.Length, iris.versi$Sepal.Length)
          colnames(vars.1.df) <- c("Sepal.Length", "Petal.Length", "Sepal.Length")
vars.2.df <- data.frame(iris.versi$Sepal.Width, iris.versi$Petal.Width, iris.versi$Petal.Length)
          colnames(vars.2.df) <- c("Sepal.Width", "Petal.Width", "Petal.Length")

for (i in 1:3){
  fits[[i]] <-  lm(vars.1.df[,i] ~ vars.2.df[,i])
}
          
          
# hint: consider using a list, and also new vectors for regression variables



#####################################
##### Part 2: data in dplyr     #####
#####################################

#use dplyr to join data of maximum height
#to a new iris data frame
height <- data.frame(Species = c("virginica","setosa","versicolor"),
                     Height.cm = c(60,100,11.8))



#####################################
##### Part 3: plots in ggplot2  #####
#####################################

#look at base R scatter plot
plot(iris$Sepal.Length,iris$Sepal.Width)

#3a. now make the same plot in ggplot


#3b. make a scatter plot with ggplot and get rid of  busy grid lines


#3c. make a scatter plot with ggplot, remove grid lines, add a title and axis labels, 
#    show species by color, and make the point size proportional to petal length

#####################################
##### Question: how did         #####
##### arguments differ between  #####
##### plot and ggplot?          #####
#####################################	