library(dyplr)
library(rvest)
library(tidyverse)
library(stringr)
library(dslabs)
data(reported_heights)

reported_heights %>% mutate(new_height = as.numeric(height)) %>%
    filter(is.na(new_height)) %>%
    head(n=10)          

not_inches <- function(x, smallest = 50, tallest = 84) {
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest
  ind
  print(ind)
}

problems <- reported_heights %>% filter(not_inches(height)) %>% .$height
length(problems)

not_inches(c(70))




  