library(purrr)
library(rvest)
library(tidyverse)
library(stringr)
library(dslabs)
library(dplyr)
library(caret)

set.seed(1)
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

#Probability that a test is positive
mean(test)

#P. that an individual has the disease if the test is a negative
sum(test[disease==1])
sum(test[disease==0])
sum(test)

result <- table(disease, test)
result
result[2,1] / sum(result[,1])
mean(disease[test==0])
mean(disease[test==1])
mean(disease[test==1]==1)

#Normalised Risk against the disease prevalence = comparing the ratios
# 2% of the population have the disease. You have tested positive with a 14.7% chance of having it
# Therefore this is 7.35 times the population mean.
mean(disease[test==1]==1)/mean(disease==1)

sum(result[,2])
(result[1,2] / sum(result[,2]))


#Bayes Example 2 -----
set.seed(3)
prev <- 0.00025
N <- 100000
outcome <- sample(c(0,1), N, replace = TRUE, prob = c(prev,1-prev))

N_D <- sum(outcome==0)
N_H <- sum(outcome==1)
cat(N_D, N_H)

accuracy <- 0.99
test <- vector("character", N)
test[outcome==0] <- sample(c("+","-"), N_D, replace=TRUE, prob = c(accuracy,1-accuracy))
test[outcome==1] <- sample(c("-","+"), N_H, replace=TRUE, prob = c(accuracy,1-accuracy))
table(outcome,test)

