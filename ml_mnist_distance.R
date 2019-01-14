library(dplyr)
library(purrr)
library(rvest)
library(tidyverse)
library(stringr)
library(dslabs)
library(caret)
library(matrixStats)

set.seed(0)
if(!exists("mnist")) mnist <- read_mnist()

ind <- which(mnist$train$labels %in% c(2,7)) %>% sample(500)
x <- mnist$train$images[ind,] #Predictors
y <- mnist$train$labels[ind]  #Labels

#Distance ----
y[1:3]
x_1 <- x[1,]
x_2 <- x[2,]
x_3 <- x[3,]

cat(sqrt(sum((x_1-x_2)^2)), sqrt(crossprod(x_1-x_2))) #Distance 1st 7 to 2nd 7
sqrt(sum((x_1-x_3)^2))
sqrt(sum((x_2-x_3)^2))

#Dist function ---- 
#Computes the distance of each value in a matrix and creates an object of class dist
d <- dist(x)
class(d)
glimpse(d)
as.matrix(d)[1:3,1:3] #Need to coerce the dist object into a matrix
image(as.matrix(d)[order(y),order(y)]) #Red squares show that digits that are the same are closer to each other

#Compute the distance of all pairs of 784 predictors----
d <- dist(t(x)) #Transpose the matrix, then use dist function
dim(as.matrix(d))
