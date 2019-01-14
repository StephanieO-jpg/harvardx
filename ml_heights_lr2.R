library(dslabs)
library(dplyr)
library(HistData)   #Data
library(caret)      #Machine Learning

#List of data sets contained within the library
data("heights")

y <- heights$height
set.seed(2)

test_index <- createDataPartition(y, times=1, p=0.5, list=FALSE)
training_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

#Create the training and test set
training_set %>% 
  filter(round(height)==63) %>%
  summarize(mean(sex=="Female"))

#Convert the factors to 0 and 1.
#Generate the linear regression - i.e. train the algorithm
lm_fit <- mutate(training_set, y = as.numeric(sex=="Female")) %>%
  lm(y~height, data = .)

# The lm_fit contains the regression line. The intercept and variable coefficients are calculated
lm_fit

#Decision rule 
#First calculate the prediction based on the linear regression and the test set
p_hat <- predict(lm_fit, test_set)
y_hat <- ifelse(p_hat > 0.5, "Female", "Male")  
class(y_hat)
#Adding %>% factor() changes the values from "Male" or "Female" and makes them factors with levels
y_hat <- ifelse(p_hat > 0.5, "Female", "Male")  %>% factor()
class(y_hat)
y_hat

#Run the confusion matrix to get the details of the model
confusionMatrix(y_hat, test_set$sex)
