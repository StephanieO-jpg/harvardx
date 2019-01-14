library(tidyverse)
library(purrr)
library(pdftools)
library(broom)

mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .) %>% tidy()
qplot(x_2, y, data = mnist_27$train)

total_range <- diff(range(mnist_27$train$x_2))
total_range

glimpse(mnist_27$train)

fit <- loess(as.numeric(y) ~ x_2, degree=1, span = 0.1, data=mnist_27$train)
mnist_27$train %>% 
  mutate(smooth = fit$fitted) %>%
  ggplot(aes(x_2, y)) +
  geom_point(size = 2, alpha = .2, color = "black") +
  geom_line(aes(x_2, smooth), color ="red")

mnist_27$train %>% 
  mutate(y = ifelse(y=="7", 1, 0)) %>%
  ggplot(aes(x_2, y)) + 
  geom_smooth(method = "loess")
