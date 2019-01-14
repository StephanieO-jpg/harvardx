library(dplyr)
library(purrr)
library(rvest)
library(tidyverse)
library(stringr)
library(dslabs)
library(caret)
data(heights)

y <- heights$sex #Predictor
x <- heights$height #Categorical Outcome. Male or Female

set.seed(2)
#Split the data in to two data sets. 
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
#Training set 
train_set <- heights[-test_index,]
#Test set  
test_set<- heights[test_index,]

#Algorithm 1 - simple case of guessing just the sex ----
y_hat <- sample(c("Male","Female"), length(test_index), replace=TRUE)
mean(y_hat == test_set$sex)

#Algorithm 2 ategorical outcomes should be coded as factors ----
y_hat <- sample(c("Male","Female"), length(test_index), replace=TRUE) %>%
  factor(levels=levels(test_set$sex))
mean(y_hat == test_set$sex)

#The average result here is roughly 52%. The set.seed ensures we get the same
#result as in the lecture. However this is not much better than guessing.
#Predict male if height is within 2 standard deviations from the avg. male height ----
heights %>% group_by(sex) %>% summarize(mean(height), sd(height))
y_hat <- ifelse(x>62, "Male", "Female") %>% factor(levels = levels(test_set$sex))
mean(y == y_hat)

#Examine 10 different cutoff levels to determine the best fit ----
cutoff <- seq(61,70)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
           factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex) 
})
plot(accuracy)
max(accuracy)
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff
#Now that the best value has been defined this can be used in the algorithm
y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex) 

#Confusion matrix
table(predicted = y_hat, actual = test_set$sex)
#Shows that women are being incorrectly recorded as men
prev <- mean(y == "Male")
prev
#This proves that the algorithm is biased. There are more men than women in the dataset.
#Therefore we need to assess and study the Sensitivity and Specificity separately
#Sensitivity -> The ability of an algorithm to predict a positive outcome when the actual outcome is positive
#Specificity -> The ability of an algorithm to not predict the positive outcome so y_hat equals )
confusionMatrix(data = y_hat, reference = test_set$sex)


# Weighted Harmonic Average (Sensitivity and Precision) or Recall and Positive Predictive Value

F_1 <- map_dbl(cutoff, function(x) {
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
    F_meas(data = y_hat, reference = factor(train_set$sex)) 
})
max_F1(F_1)
best_cutoff_F1 <- cutoff[which.max(F_1)]
best_cutoff_F1

y_hat_F1 <- ifelse(test_set$height > best_cutoff_F1, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
confusionMatrix(data = y_hat_F1, reference = test_set$sex)

#Receiver operating characteristic (ROC) Curve
#Plots Sensitivity and 1-Specificity
cutoffs <- c(50, seq(60,75), 80)
height_cutoffs <- map_df(cutoffs, function(x) {
  y_hat_ROC <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height Cutoffs", 
       FPR = 1-specificity(y_hat_ROC, test_set$sex),
       TPR = sensitivity(y_hat_ROC, test_set$sex))
})
height_cutoffs
height_cutoffs %>% ggplot(aes(FPR, TPR)) + geom_point() + geom_line() +
  ylab("Sensitivity") + xlab("1-Specificity/Negative Pred.Val")
