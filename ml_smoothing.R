library(dslabs)
library(dplyr)
library(tidyverse)
library(lubridate)

data("polls_2008")
qplot(day, margin, data = polls_2008)

resid <- ifelse(lm(margin~day, data = polls_2008)$resid > 0, "+", "-")
polls_2008 %>% 
  mutate(resid = resid) %>% 
  ggplot(aes(day, margin)) + 
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  geom_point(aes(color = resid), size = 3)

#Using summary with lm() provides more detailed information
summary(lm(margin~day, data = polls_2008))
#Use the plot to show:
#-Residual vs Fitted
#-Normal Q
#-Scale-Location
#-Residual vs Leverage
plot(lm(margin~day, data = polls_2008))

#Smoothing ----
span <- 7
#Switch between "box" and "normal" smoothing
fit <- with(polls_2008, 
            ksmooth(day, margin, x.points = day, kernel="box", bandwidth = span))

polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(day, smooth), color="red")

#Local Weighted Regression (loess) ----
#Taylors theorem - if you look closely enough at any smooth function it will look like a line
#Model for a 3 week window will be linear
#LOESS keeps the number of points in the calculation the same. The Span is a %
# --- it also uses a weighted version. Tukey Tri weight
#---- it fits the model robustly. Outliers are detected and downweighted 
total_days <- diff(range(polls_2008$day))
span <- 21/total_days

fit <- loess(margin ~ day, degree=1, span = span, data=polls_2008)

polls_2008 %>% mutate(smooth = fit$fitted) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(day, smooth), color="red", span = 0.15, method.args = list(degree=1))
