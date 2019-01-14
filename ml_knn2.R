library(dplyr)
library(purrr)
library(rvest)
library(tidyverse)
library(stringr)
library(dslabs)
library(caret)
library(matrixStats)

#Load the heights data
data("tissue_gene_expression")

#Set a range of k values to find out the max value. 
ks <- seq(1, 11, 2)
x <- tissue_gene_expression$x
y <- tissue_gene_expression$y

#Create a function to determine the accuracy of each k value
#The result of the map_df must be a list.
# See https://github.com/tidyverse/purrr/issues/179
accuracy <- map_df(ks, function(k) {
  set.seed(1) 
  train_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE) 
  train_x <- x[train_index,]
  test_x <- x[-train_index,]
  train_y <- y[train_index]
  test_y <- y[-train_index]  
  
  knn_fit <- knn3(train_x,train_y , k = k)
  y_hat <- predict(knn_fit, test_x, type="class")
  test_error<- confusionMatrix(data = y_hat, reference = test_y, mode = "everything")$overall["Accuracy"]
  list(k=k, test_error = test_error)  #If this is missing the following error occurs: Error in bind_rows_(x, .id) : Argument 1 must have names
})
plot(accuracy$k, accuracy$test_error)
pos_max <- which.max(accuracy$test_error)
accuracy
