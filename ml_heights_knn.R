library(dslabs)
library(dplyr)
library(HistData)   #Data
library(caret)      #Machine Learning

#List of data sets contained within the library
data("heights")

#Knn Nearest Neighbour----
y <- heights$height
set.seed(1)
test_index <- createDataPartition(y, times=1, p=0.5, list=FALSE)
training_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

knn_fit <- mutate(training_set, y = as.numeric(sex=="Female")) %>%
  knn3(y ~ ., data = ., k = 5)

y_hat_knn <- predict(knn_fit, test_set, type = "class")
confusionMatrix(data = y_hat_knn, reference = test_set$sex)$overall[1]


