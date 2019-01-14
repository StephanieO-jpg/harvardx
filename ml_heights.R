library(dslabs)
library(dplyr)
library(lubridate)

data("reported_heights")

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type) 

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

#What is the propotion of females in class and online? ----
#(That is, calculate the proportion of the in class students who are female and the proportion 
#of the online students who are female.)
table(dat$sex, dat$type)
dat_female <- dat %>% filter(sex=="Female") %>% group_by(type) %>% summarize(count = n())
dat_total <- dat %>% group_by(type) %>% summarize(count = n())
dat_female$count / dat_total$count

# Answer
dat %>% group_by(type) %>% summarize(prop_female = mean(sex == "Female")) 

#use Type to predict the sex and determine what would the accuracy be ----
#By determing the percetange of females by course I can set the expectation in the algorithm that the class will be 
#most likely be female or male. It results in 63% accuracy. So full of bias.
y_hat <- ifelse(x =="inclass", "Female", "Male") %>% factor(levels = levels(y))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex)
confusionMatrix(data = y_hat, reference = y)

#The following line of code also shows the confusion matrix. 
table(y_hat, y)
