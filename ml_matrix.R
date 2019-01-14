library(dplyr)
library(purrr)
library(rvest)
library(tidyverse)
library(stringr)
library(dslabs)
library(caret)
library(matrixStats)

#First define the distance between observations based on the features
#Take the average of the K nearest points - neighbourhood - Estimatd conditional probability
#Larger K's result in smoother estimates while smaller k is more flexible and wigglier

#Compare the Knn to Logistic Regression
fit_glm <- glm(y ~ x_1 + x_2, data=mnist_27$train, family="binomial")
p_hat_logistic <- predict(fit_glm, mnist_27$test)
y_hat_logistic <- factor(ifelse(p_hat_logistic > 0.5, 7, 2))
confusionMatrix(data = y_hat_logistic, reference = mnist_27$test$y)$overall[1]

#knn3 function is from the caret package
# Formula - outcome ~ predictor_1 + predictor_2 + predictor_3
#y ~ . is a shortcut for listing all the predictors
knn_fit <- knn3(y ~ ., data = mnist_27$train, k = 200) #default neighbours
y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall[1]

#This improves over the logistic regression.

#Overtraining and Oversmoothing
mnist_27$true_p %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(breaks=c(0.5),color="black")

#find the relevant k that can be used (based on a sequence of odd values)
ks <- seq(3, 251, 2)
accuracy <- map_df(ks, function(k) {
  fit <- knn3(y ~ ., data = mnist_27$train, k = k)
  y_hat <- predict(fit, mnist_27$train, type = "class")
  cm_train <- confusionMatrix(data = y_hat, reference = mnist_27$train$y)
  train_error <- cm_train$overall["Accuracy"]
  
  y_hat <- predict(fit, mnist_27$test, type = "class")
  cm_test <- confusionMatrix(data = y_hat, reference = mnist_27$test$y)
  test_error <- cm_test$overall["Accuracy"]
  
  list(train = train_error, test = test_error)  
})

accuracy%>% ggplot() +
  # blue plot
  geom_point(aes(ks, accuracy$train), color="Red") +
  geom_line(aes(x=ks, y=accuracy$train), color="DarkRed") +
  # red plot
  geom_point(aes(x=ks, y=accuracy$test), color="Blue") + 
  geom_line(aes(x=ks, y=accuracy$test), color="DarkBlue")




