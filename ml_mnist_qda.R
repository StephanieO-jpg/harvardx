library(dplyr)
library(purrr)
library(rvest)
library(tidyverse)
library(stringr)
library(dslabs)
library(caret)
library(matrixStats)

#Quadratic discriminant analysis
#Harder to use this for large number of predictors. 
#Multivariate normal - bivariate normal. Both folloz gaussian normal distribution.
data("mnist_27")

#So we need to calculate two means and SDs and a correlation for each case. 
params <- mnist_27$train %>%
  group_by(y) %>%
  summarise(avg1 = mean(x_1), avg2 = mean(x_2), sd_1 = sd(x_1), sd_2 = sd(x_2), r = cor(x_1,x_2))
params

mnist_27$train %>% mutate(y = factor(y)) %>% 
  ggplot(aes(x_1, x_2, fill = y, color=y)) + 
  geom_point(show.legend = FALSE) + 
  stat_ellipse(type="norm", lwd = 1.5) #Curve representing 95% of the data in that region

train_qda <- train(y ~ ., 
                   method = "qda",
                   data = mnist_27$train)

y_hat <- predict(train_qda, mnist_27$test)
confusionMatrix(data = y_hat, reference = mnist_27$test$y)$overall["Accuracy"]

#To simplify the number of parameters (in this case the SDs and the correlations)
params_reduced <- mnist_27$train %>%
  group_by(y) %>%
  summarise(avg1 = mean(x_1), avg2 = mean(x_2), sd_1 = sd(x_1), sd_2 = sd(x_2), r = cor(x_1,x_2))
params_reduced <- params_reduced %>% mutate(sd_1 = mean(sd_1), sd_2 = mean(sd_2), r = mean(r))
params_reduced
