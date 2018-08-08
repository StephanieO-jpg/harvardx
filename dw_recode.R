library(dplyr)
library(purrr)
library(rvest)
library(tidyverse)
library(stringr)
library(dslabs)
data("gapminder")

gapminder %>% filter(region =="Caribbean") %>%
  mutate(country= recode(country,
                         'Antigua and Barbuda'="Barbuda",
                         'Dominican Republic' = "DR",
                         'St. Vincent and the Grenadines' = "St. Vincent",
                         'Trinidad and Tobago'= "Trinidad")) %>%
  ggplot(aes(year, life_expectancy, color = country)) + 
  geom_line()


dat <- gapminder %>% filter(region == "Middle Africa") %>% 
  mutate(recode(country, 
                "Central African Republic" = "CAR", 
                "Congo, Dem. Rep." = "DRC",
                "Equatorial Guinea" = "Eq. Guinea"))
dat %>% filter(region == "Middle Africa")

