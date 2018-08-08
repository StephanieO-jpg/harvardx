library(dplyr)
library(purrr)
library(rvest)
library(tidyverse)
library(stringr)
library(dslabs)
data(reported_heights)

filename <- system.file("extdata/murders.csv", package="dslabs")
lines<-readLines(filename)
x <- str_split(lines,",")

dat <- data.frame(map_chr(x, 1),
                  map_chr(x, 2),
                  map_chr(x, 3),
                  map_chr(x, 4),
                  map_chr(x, 5)) %>%
  mutate_all(parse_guess) %>%
  setNames(col_names)

dat %>% head()


day <- c("Monday", "Tuesday")
staff <- c("Mandy, Chris and Laura", "Steve, Ruth and Frank")
schedule <- data.frame(day,staff)
print(schedule)

tidy <- schedule %>% 
  mutate(staff = str_split(staff, ", | and ")) %>% 
  unnest()
tidy