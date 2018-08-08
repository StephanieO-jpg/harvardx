library(dslabs)
library(lubridate)

data("polls_us_election_2016")
class(polls_us_election_2016$startdate)
polls_us_election_2016 %>% filter(pollster == "Ipsos" & state =="U.S.") %>%
  ggplot(aes(startdate,rawpoll_trump)) +
  geom_line()

set.seed(2)
dates <- sample(polls_us_election_2016$startdate, 10) %>% sort
dates

data.frame(date = days(dates),
           month = month(dates),
           year = year(dates))

dates <- c("09-01-02", "01-12-07", "02-03-04")
ymd(dates)
mdy(dates)
dmy(dates)

now()
now("GMT")
OlsonNames()
