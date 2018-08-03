library(dplyr)
library(tidyverse)
path <- system.file("extdata",package="dslabs")
filename <- file.path(path, "life-expectancy-and-fertility-two-countries-example.csv")
raw_data <- read.csv(filename)
dat <- raw_data %>% gather(key, value, -country)
dat %>% separate(key, c("year","variable_name"),sep="_",extra="merge") %>%
        spread(variable_name,value)
head(dat)