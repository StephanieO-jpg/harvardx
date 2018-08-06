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

pattern <- "^[4-7]'\\d{1,2}\"$"
yes <- c("5'7\"","6'2\"")
no <- c("6,2\"","6.2\"")
str_detect(yes, pattern)
str_detect(no,pattern)

animals <- c("cat", "puppy", "Moose", "MONKEY")
pattern_animals <- "[A-Z]$"
str_detect(animals, pattern_animals)

animals2 <- c("moose", "monkey", "meerkat", "mountain lion")
pattern2 <- "mo?"
str_detect(animals2, pattern2)

schools <- c("U. Kentucky","Univ New Hampshire","Univ. of Massachusetts","University Georgia","U California")
final <- schools %>% 
  str_replace("^Univ\\.?\\s|^U\\.?\\s", "University ") %>% 
  str_replace("^University of |^University ", "University of ")
final

problems <- c("5.3", "5,5", "6 1", "5 .11", "5, 12")
pattern_with_groups <- "^([4-7])[,\\.\\s](\\d*)$"
str_replace(problems, pattern_with_groups, "\\1'\\2")


