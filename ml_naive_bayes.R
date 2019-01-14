library(dplyr)
library(purrr)
library(tidyverse)
library(dslabs)
library(caret)

data("heights")
y <- heights$height
set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

#Normal dist. is a good approximation for the distrubion. So we can use the average and SD to estimate the data
params <- train_set %>% 
  group_by(sex) %>%
  summarize(avg = mean(height), sd = sd(height))
params

#Prevalence - Pi----
pi <- train_set %>%
  summarize(pi=mean(sex=="Female")) %>%
  .$pi  #This additonal reads the field pi from the vector that has been created 


#Naive Bayes Estimate of the conditional probability----
#In this particular case this is similar to a logistic regression case.
x <- test_set$height
f0 <- dnorm(x, params$avg[2], params$sd[2])  #Distribution generated for males
f1 <- dnorm(x, params$avg[1], params$sd[1])  #Distribution generated for females
p_hat_bayes <- f1*pi / (f1*pi + f0*(1-pi))

#Review sensistivity----
#To define sensitivity and specificity, we need a binary outcome. 
#When the outcomes are categorical, we can define these terms for a specific category
# the ability of an algorithm to predict a positive outcome when the actual outcome is positive
# Y_hat = 1 whenever = 1 
# [True Positive] / (TP+FN)
y_hat_bayes <- ifelse(p_hat_bayes > 0.5, "Female","Male")
sensitivity(data = factor(y_hat_bayes), reference= factor(test_set$sex))
#Review specificitiy----
#the ability of an algorithm to not to predict a positive Y_hat = 0 when Y = 0
# [True Negative] / (TN+FP)
specificity(data=factor(y_hat_bayes), 
            reference = factor(test_set$sex))
#Controlling prevalence----
#We ignore the pi prevalence that was set and "naively" set the value to 0.5.
p_hat_bayes_biased <- f1*0.5 / (f1*0.5 + f0*(1-0.5))
y_hat_bayes_biased <- ifelse(p_hat_bayes_biased > 0.5, "Female","Male")
sensitivity(data = factor(y_hat_bayes_biased), reference= factor(test_set$sex))
specificity(data = factor(y_hat_bayes_biased), reference= factor(test_set$sex))
