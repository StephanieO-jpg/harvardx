library(dplyr)
library(purrr)
library(rvest)
library(tidyverse)
library(stringr)
library(dslabs)
library(caret)
library(matrixStats)

#Load the heights data
data("heights")

#Set a range of k values to find out the max value. Count by 3s (1, 4, 7)
ks <- seq(1, 101, 3)

set.seed(1)

#Create a function to determine the accuracy of each k value
#The result of the map_df must be a list.
# See https://github.com/tidyverse/purrr/issues/179
accuracy <- map_df(ks, function(k) {
  test_index <- createDataPartition(y, times=1, p=0.5, list=FALSE)
  training_set <- heights %>% slice(-test_index)
  test_set <- heights %>% slice(test_index)
  knn_fit <- knn3(sex~height, data=training_set, k = k)
  y_hat <- predict(knn_fit, test_set, type="class")
  F_val <- F_meas(data = y_hat, reference = test_set$sex)
  
  list(k=k, F_val = F_val)
})
plot(accuracy$F_val,accuracy$k)
pos_max <- which.max(accuracy$F_val)
accuracy[pos_max,]



