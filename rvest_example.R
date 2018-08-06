library(dplyr)
library(tidyverse)
library(rvest)
h <- read_html("http://www.foodnetwork.co.uk/recipes/beef-chili-0.html")
title <- h %>% html_node(".title-main") %>% html_text()
recipe <- h %>% html_node(".method-list") %>% html_text()
ingredients <- h %>% html_nodes(".ingredient-list") %>% html_text()

title
recipe
ingredients
