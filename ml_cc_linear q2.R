library(caret)    #Machine Learning

#repeat the above but using larger datasets
set.seed(1) 
n_large <- c(100, 500, 1000, 5000, 10000)

rmse_large <- function(n_large){
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n = n_large, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  
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
}
result <- sapply(n_large,rmse_large)
result
mean(result[,1])
sd(result[,1])
mean(result[,2])
sd(result[,2])
mean(result[,3])
sd(result[,3])
mean(result[,4])
sd(result[,4])
mean(result[,5])
sd(result[,5])
