library(dplyr)
library(purrr)
library(rvest)
library(tidyverse)
library(stringr)
library(dslabs)
library(caret)

data("mnist_27")
mnist_27$train %>% ggplot(aes(x_1, x_2, color = y)) +
  geom_point()

#Logistic regression
fit <- glm(y ~ x_1 + x_2, data=mnist_27$train, family="binomial")

#Decision model
p_hat <- predict(fit, newdata=mnist_27$test)
y_hat <- factor(ifelse(p_hat > 0.5, 7,2))
confusionMatrix(data=y_hat, reference=mnist_27$test$y)

#Access and plot the true conditional probability
mnist_27$true_p %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(breaks=c(0.5),color="black")
  