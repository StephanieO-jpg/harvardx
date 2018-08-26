library(dplyr)
library(purrr)
library(rvest)
library(tidyverse)
library(stringr)
library(dslabs)
data("gapminder")

gapminder %>% filter(country == "Italy" | country == "Belgium" 
                     | country == "Ireland" 
                     | country == "Poland" 
                     | country == "China" 
                     ) %>%
  ggplot(aes(year, fertility, color = country)) + 
  geom_line()



