library(dplyr)
library(purrr)
library(rvest)
library(tidyverse)
library(stringr)
library(dslabs)
library(caret)

set.seed(2)
make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = 1, 
                      sigma_0 = 1,  sigma_1 = 1){
  
  y <- rbinom(n, 1, p)    #Creates a vector of 1000 boolean random values 
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}
dat <- make_data()

set.seed(1)
delta <- seq(0, 3, len = 25)
#Change the variable of the function make_data.
res <- sapply(delta, function(d){
  dat <- make_data(mu_1 = d)
  #Logistic regression
  fit_glm <- dat$train %>% glm(y ~ x, family = "binomial", data = .)
  #Decision model
  y_hat_glm <- ifelse(predict(fit_glm, dat$test) > 0.5, 1, 0) %>% factor(levels = c(0, 1))
  mean(y_hat_glm == dat$test$y)
})
qplot(delta, res)

