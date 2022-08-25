#
# Accuracy Test
#

# section 8.3

library(readxl)
library(dplyr)
library(seasonal)
library(ggplot2)
library(forecast)
library(tidyverse)
library(tsibble)
library(feasts)
library(lubridate)
library(tsibbledata)
library(fable)
library(zoo)
library(fst)
library(fpp3)
library(fabletools)
library(reticulate)






AAPL <- py$data_receive("AAPL")


# Re-index based on trading days
AAPL <- AAPL %>% 
  mutate(day = row_number()) %>%
  as_tsibble(index = day, regular = TRUE)


# Filter the time of interest
last.week <- AAPL %>% filter(Date <= "2022-04-08") 
last.week <- AAPL %>% filter(yearweek(Date) <= yearweek("2022 W10")) 


# Fit the models
apl_fit <- last.week %>%
  model(
    Mean = MEAN(Close),
    `Naïve` = NAIVE(Close),
    Drift = NAIVE(Close ~ drift())
  )


# Produce forecasts 
last <- AAPL %>%
  filter(yearweek(Date) == yearweek("2022 W14"))
apl_fc <- apl_fit %>%
  forecast(new_data = last)


# Plot the forecasts
apl_fc %>%
  autoplot(last.week, level = NULL) +
  autolayer(last, Close, colour = "black") +
  labs(y = "$US",
       title = "Google daily closing stock prices",
       subtitle = "(Jan 2015 - Jan 2016)") +
  guides(colour = guide_legend(title = "Forecast"))




closer <- AAPL %>% filter(yearweek(Date) >= yearweek("2022 W1"))

myfit <- last.week %>%
  model(
    Drift = NAIVE(Close ~ drift())
  )
report(myfit)

myfc <- forecast(myfit, h = 10) 
myfc %>% autoplot(closer)


apl_fc %>% autoplot(closer)


AAPL %>% 
  filter_index(. ~"525") %>%
  model(
    ETS(Close)
  ) %>% 
  forecast(h = 35) %>%
  autoplot(AAPL)




           