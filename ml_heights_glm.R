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
#Generate the logistic regression - ensures the estimate of the conditional probability
#is between 0 and 1 and the equation is: log(p/(1-p))
#- 0.5 => 1 to 1 
#- 0.75=> 3 to 1 

glm_fit <- mutate(training_set, y = as.numeric(sex=="Female")) %>%
  glm(y~height, data = ., family="binomial")

# The lm_fit contains the regression line. The intercept and variable coefficients are calculated
glm_fit

#Decision rule 
#First calculate the prediction based on the linear regression and the test set
p_hat_logit <- predict(glm_fit, test_set, type="response")
y_hat <- ifelse(p_hat > 0.5, "Female", "Male")  
class(y_hat)
#Adding %>% factor() changes the values from "Male" or "Female" and makes them factors with levels
y_hat <- ifelse(p_hat_logit > 0.5, "Female", "Male")  %>% factor()
class(y_hat)
y_hat

#Run the confusion matrix to get the details of the model
confusionMatrix(y_hat, test_set$sex)
