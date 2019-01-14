library(caret)    #Machine Learning

set.seed(1)
n <- 1000
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

#Check the correlation between the variables
cor(dat)

# Set the index
set.seed(1)
test_index <- createDataPartition(dat$y, times = 1, p =0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

#Linear regression
fit_x_1 <- lm(y ~ x_1, data = train_set)
fit_x_2 <- lm(y ~ x_2, data = train_set)
fit_both <- lm(y ~ x_1 + x_2, data = train_set)

#predict function - this function performs the regression line
y_hat_x1 <- predict(fit_x_1, test_set)
y_hat_x2 <- predict(fit_x_2, test_set)
y_hat_both <- predict(fit_both, test_set)

y_hat_rmse_x1 <- sqrt(mean((y_hat_x1-test_set$y)^2))
y_hat_rmse_x2 <- sqrt(mean((y_hat_x2-test_set$y)^2))
y_hat_rmse_both <- sqrt(mean((y_hat_both-test_set$y)^2))

mean(y_hat_rmse_x1)
mean(y_hat_rmse_x2)
mean(y_hat_rmse_both)