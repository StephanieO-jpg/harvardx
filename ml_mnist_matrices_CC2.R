library(dplyr)
library(purrr)
library(rvest)
library(tidyverse)
library(stringr)
library(dslabs)
library(caret)
library(matrixStats)


data("mnist_27")
mnist <-read_mnist()
class(mnist$train$images) 
class(mnist$train$labels) 
x <- mnist$train$images
y <- mnist$train$labels

#R flips images so you need to use the [,28:1] to resort when displaying the image
grid <- matrix(x[3,], 28, 28)
image(1:28, 1:28, grid[,28:1]) 
y[3]

grey_area <- (x > 50 & x < 205)
boxplot(grey_area)
mean(grey_area)

