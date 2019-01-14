library(dplyr)
library(purrr)
library(tidyverse)
library(dslabs)
library(caret)

#Bootstrapping----
#A bootstrap sample is a smaller sample that is “bootstrapped” from a larger sample. 
#Bootstrapping is a type of resampling where large numbers of smaller samples of the same size
#are repeatedly drawn, with replacement, from a single original sample.

#Bootstrapping is loosely based on the law of large numbers, which states that if you sample over and over again, 
#your data should approximate the true population data. 

#The Bootstrap permits us to approximate a Monte Carlo simulation without access to the entire
#distribution. The general idea is relatively simple. We act as if the observed sample 
#is the population. We then sample (with replacement) datasets, of the same sample size 
#as the original dataset. 

#A series of test/training partitions are created using createDataPartition 
#while createResample creates one or more bootstrap samples. 
# - createFolds splits the data into k groups while 
# - createTimeSlices creates cross-validation split for series data. 
# - groupKFold splits the data based on a grouping factor.

set.seed(1995)
indexes <- createResample(mnist_27$train$y, 10)
summary(indexes)
total_occurs <- 0

#Questions 1 and 2
#Calculate the number of occurences for 3,4 and 7 in the first index----
sum(indexes$Resample01 == 3)
sum(indexes$Resample01 == 4)
sum(indexes$Resample01 == 7)

x <- 1:10
y <- c(3,4,7)

#Function to count the occurences per index
total_sum <- function(x, y) {
    sum(indexes[[x]] == y)  
}
sum(sapply(x, total_sum, y=3)) +
sum(sapply(x, total_sum, y=4)) +
sum(sapply(x, total_sum, y=7))


#Question 3 Monte Carlo for 75th percentile ----
set.seed(1)
N <- 100
B <- 10000

#Define an object `res` that contains a logical vector for simulated intervals that contain mu
res <- replicate(B, {
  y <- rnorm(N, 0, 1)
  res <- quantile(y, 0.75)
})

# Calculate the proportion of results in `res` that include mu. Print this value to the console.
mean(res)
sd(res)

#Display a histogram and Q-Q plot to show the Sample and Theoretical Quantiles 
par(mfrow=c(1,2))
hist(res)
qqnorm(res)
qqline(res)

#Use 10 bootstrap samples to estimate the standard error using just the initial sample y.---- 
set.seed(1)
B <- 10000
N <- 100
y <- rnorm(N, 0, 1)
set.seed(1)
y_indexes <- createResample(y, B)

x <- 1:B
boot_res <- function(x) {
  boot_res <- quantile(y[y_indexes[[x]]], 0.75)
}
mean(sapply(x, boot_res))
sd(sapply(x, boot_res))






