library(purrr)
library(rvest)
library(tidyverse)
library(stringr)
library(dslabs)
library(dplyr)
library(caret)

data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species       #Actual values

#Set up the training and test data ----
set.seed(2)
# line of code
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

levels(train$Species)

summary(train) #Find the max and min of the values
#Find the average values by feature
train %>% group_by(Species) %>% summarize(mean(Sepal.Length), sd(Sepal.Length))
train %>% group_by(Species) %>% summarize(mean(Sepal.Width  ), sd(Sepal.Width  ))
train %>% group_by(Species) %>% summarize(mean(Petal.Length  ), sd(Petal.Length  ))
train %>% group_by(Species) %>% summarize(mean(Petal.Width ), sd(Petal.Width ))

#Sepal Length ----
#The order is critical for the ifelse. The largest average must go first. If you put Versicolor before Virginica the results will be wrong
cutoff <- seq(5.5,7.2,0.1)
accuracy_Slength <- map_dbl(cutoff, function(x){
  y_hat_sl <- ifelse(train$Sepal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = levels(train$Species))
  mean(y_hat_sl == train$Species)
})
accuracy_Slength
plot(accuracy_Slength)
max(accuracy_Slength)
best_cutoff_sl <- cutoff[which.max(accuracy_Slength)]
best_cutoff_sl





#Petal Width ----
#The order is critical for the ifelse. The largest average must go first. If you put Versicolor before Virginica the results will be wrong
cutoff <- seq(1.1,2.3,0.1)
accuracy_Pw <- map_dbl(cutoff, function(x){
  y_hat_pw <- ifelse(train$Petal.Width > x, "virginica", "versicolor") %>% 
    factor(levels = levels(train$Species))
  mean(y_hat_pw == train$Species)
})
accuracy_Pw
plot(accuracy_Pw)
max(accuracy_Pw)
best_cutoff_pw <- cutoff[which.max(accuracy_Pw)]
best_cutoff_pw

#Petal Length ----
#The order is critical for the ifelse. The largest average must go first. If you put Versicolor before Virginica the results will be wrong
cutoff <- seq(3.8,6.1,0.1)
accuracy_Plength <- map_dbl(cutoff, function(x){
  y_hat_pl <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = levels(train$Species))
  mean(y_hat_pl == train$Species)
})
accuracy_Plength
plot(accuracy_Plength)
max(accuracy_Plength)
best_cutoff_pl <- cutoff[which.max(accuracy_Plength)]
best_cutoff_pl

#Apply the best cutoff.
y_hat_pl <- ifelse(test$Petal.Length > 4.7 & test$Petal.Width > 1.5, "virginica", "versicolor") %>% 
  factor(levels = levels(test$Species))
mean(y_hat_pl == test$Species)

#Use the best cutoff calculated on the training data to determine the overall accuracy in the test data
confusionMatrix(data = y_hat_pl, reference = test$Species)



#Solution from lecturer ----
foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==test$Species)
  })
}
predictions <- apply(test[,-5],2,foo)
sapply(predictions,max)	

#Combine Two cutoffs
