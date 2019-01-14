#Classification and Regression Trees (CART)
library(dplyr)
library(ggrepel)
library(purrr)
library(rvest)
library(tidyverse)
library(stringr)
library(dslabs)
library(caret)
library(matrixStats)
library(rpart)


#complexity parameter - to avoid overtraining the algorithm sets a minimum for how
#much the residual sum of squares must improve for another partition to be added.
#Sets a minimum number of observations - default is 20
#Minbucket - sets the min amount of obersations 
fit <- rpart(margin ~., data = polls_2008)
plot(fit, margin = 0.1)
text(fit, cex=0.75)

polls_2008 %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")

pruned_fit <- prune(fit, cp = 0.1)
pruned_fit
plot(pruned_fit, margin = 0.1)
text(pruned_fit, cex=0.75)

#How to pick the CP?
#Use cross validation
#Train function in caret
train_rpart <- train(margin ~ ., 
                     method = "rpart",  #Random forest partitions
                     tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)),
                     data = polls_2008)
ggplot(train_rpart)
train_rpart$finalModel #Displays the tree that can then be plotted
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex=0.75)
