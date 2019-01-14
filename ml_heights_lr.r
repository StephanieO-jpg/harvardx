library(dslabs)
library(dplyr)
library(HistData)   #Data
library(caret)      #Machine Learning

#List of data sets contained within the library
d <- data(package = "HistData")

galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

#Create the training and test sets
y <- galton_heights$son
test_index <- createDataPartition(y, times = 1, p =0.5, list = FALSE)
train_set <- galton_heights %>% slice(-test_index)
test_set <- galton_heights %>% slice(test_index)

avg <- mean(train_set$son)
#Squared loss
#Average of the training set compared to each value of the test set
mean((avg-test_set$son)^2)

#Linear regression
fit <- lm(son ~ father, data = train_set)
#Formula for the regression line
y_hat <- fit$coef[1] + fit$coef[2]*test_set$father

#predict function - this function performs the regression line
y_hat <- predict(fit, test_set)

#Replace avg with y_hat to see if there is a better fit using linear regression
mean((y_hat - test_set$son)^2)
#This reduces the error (loss) and is a better fit than taking the average of the height of the sons
#So the linear regression is a better model over the basic average




