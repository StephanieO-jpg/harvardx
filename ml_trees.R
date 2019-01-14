#Classification and Regression Trees (CART)
library(dplyr)
library(ggrepel)
library(purrr)
library(rvest)
library(tidyverse)
library(stringr)
library(dslabs)
library(caret)
library(matrixStats)

#Try to predict the region using the fatty acids as predictors
data("olive")
glimpse(olive) #Show the variables (fatty acids in olives)
table(olive$region)
olive <- select(olive, - area) #Removes the area column from the table

#Knn model----
fit_knn <- train(region ~.,
                 method = "knn",
                 tuneGrid = data.frame(k = seq (1,15,2)), data = olive)
ggplot(fit_knn)

#Excellent faceted boxplot showing clearly acids by region
olive %>% gather(fatty_acid, percentage, -region) %>%
  ggplot(aes(region, percentage, fill = region)) +
  geom_boxplot() +
  facet_wrap(~fatty_acid, scales = "free")

#Comparison of two acids by region. Clearly shows that 
olive %>% ggplot(aes(eicosenoic, linoleic, fill = region, color = region)) +
  geom_point()

