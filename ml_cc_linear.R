library(caret)    #Machine Learning

set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

set.seed(1)


#Calculate the RMSE (Root Mean Square Error) 100 times.
B <- 100
res <- replicate(B, {

# Set the index
  test_index <- createDataPartition(dat$y, times = 1, p =0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  
  #Linear regression
  fit <- lm(y ~ x, data = train_set)

#predict function - this function performs the regression line
  y_hat <- predict(fit, test_set)
  y_hat_rmse <- sqrt(mean((y_hat-test_set$y)^2))
})
mean(res)
sd(res)

