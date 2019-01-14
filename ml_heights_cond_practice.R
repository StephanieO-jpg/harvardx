library(dslabs)
library(dplyr)
library(ggplot2)
library(lubridate)
library(dslabs)
library(MASS)

data("heights")
heights %>% 
  mutate(height = round(height)) %>%
  group_by(height) %>%
  summarize(p = mean(sex == "Male")) %>%
  qplot(height, p, data =.)              

#Summary data by gender ----
heights_sum <- heights %>% 
  mutate(height = round(height)) %>%
  group_by(height) 

#This table will give a summary of total by gender by height
table(heights_sum)

#Calculate for males ----
heights_male <- heights %>% 
  mutate(height = round(height)) %>%
  group_by(height) %>%
summarize(p = mean(sex == "Male"))
heights_male
#Use quantiles to group data. ----
#This groups the data into quantile ranges so that the numbrer of 
#points are reduced and the data becomes easier to read. The sequences groups by a factor of 10.
ps <- seq(0, 1, 0.1)
heights %>% 
  mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)

ps <- seq(0, 1, 0.1)
heights_cut <- heights %>% 
  mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height), f = mean(sex == "Female")) 

quantile(heights$height, ps, include.lowest = TRUE)

#Bivariate normal distribution ----
#The “regular” normal distribution has one random variable; 
#A bivariate normal distribution is made up of two independent random variables. 
#The two variables in a bivariate normal are both are normally distributed, 
#and they have a normal distribution when both are added together. 
#Visually, the bivariate normal distribution is a three-dimensional bell curve.
#Francis Galton (1822-1911) was one of the first mathematicians to study the bivariate normal distribution in depth, during his study on the heights of parents and their adult children. Bravais, Gauss, Laplace, Plana also studied the distribution in the early nineteenth century (Balakrishnan & Lai, 2009).

Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
plot(dat)

ps <- seq(0, 1, 0.1)
dat %>% 
  mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%
qplot(x, y, data =.)