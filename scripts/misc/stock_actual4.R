#
# Stock Actual 4
# 

# going to update 'best model' technique


## ---- libraries
library(reticulate)
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
library(caret)
library(janitor)



## ---- f1
# Ensures no duplication of most recent date.
#
# Takes sum of weekly closing price & averages by days market was
# open.
#
my.prep <- function(data){
  dup <- tail(data,2)
  if(dup[1,1] == dup[2,1]){
    data <- slice(data,1:n()-1)
  }
  data$Week = yearweek(as.Date(data$Date))
  data$Wk = as.Date(data$Week, format = "%Y/%U")
  data$day = as.POSIXlt(data$Date)$wday
  
  ts <- data %>%
    group_by(Wk) %>%
    summarize(Close = as.double((sum(Close) / length(day)))) %>%
    as_tsibble(index = Wk)
  
  return(ts)
}




## ---- data.prep
#
# Tickers will change daily.
# Takes up to 250 of yahoo finances "highest movers."
tickers <- py$mylist


# creates dataset of all tickers received and populates with average 
# weekly closing price. 
ts <- my.prep(py$data_receive('AAPL')) # using AAPL to ensure all dates
df <- ts[,1]
for(i in 1:length(tickers)){
  ts <- my.prep(py$data_receive(tickers[i]))
  
  for(r in 1:nrow(ts)){
    df[r,i+1] = ts[r,2]
  }
  names(df)[i+1] <- tickers[i]
}

uhh <- t(df)
#### df$Wk <- as.numeric(as.POSIXct(df$Wk))
#### write.csv(df, "~/Projects/algotrader/data/Apr07_2022.csv", row.names=FALSE)


# above is good
# =================================================================================





data.ready <- function(name){
  r.data = df %>%
    select(Wk, name) %>%
    as_tsibble(index = Wk)
  
  return(r.data)
}                             # these are trying to iterate through df to properly format
                              # each ticker to find its best model.   needs more work

for(i in 1:6){#length(df)){
  name = colnames(df[,i+1])
  insert.ready = data.ready(name)
}



names(insert.ready)[2] <- "Close"
fit <- insert.ready %>%
  model(ARIMA(Close))
report(fit)

fc <- fit %>% forecast(h = 4)

fit %>% forecast(h = 12) %>%
  autoplot(insert.ready)
# this is all to test the above manipulations
ts <- insert.ready




# this is to compare with the above. data still plotting weird ------ FIND SOLUTION
ts <- ts(df$AAPL, frequency = 53, start = 2020)
f2 <- forecast(
  ts,
  method = c("arima"),
  h = 24
)
plot(f2)

fc2 <- f2 %>% forecast(ts, method = c("arima"), h=24)
#









# ================================================================


# this is good for finding best model but there are probably still better ways. 
# NEED TO GET data in uniform format!!!

train <- ts %>% filter_index(. ~"2021-08-03")
ts %>% autoplot()

## ARIMA
fit_arima <- train %>% model(ARIMA(Close))
report(fit_arima)

fit_arima %>% gg_tsresiduals(lag_max = 20)
augment(fit_arima) %>% # no residual correlation
  features(.innov, ljung_box, lag = 20, dof = 6) 

## ETS
fit_ets <- train %>% model(ETS(Close))
report(fit_ets)

fit_ets %>% gg_tsresiduals(lag_max = 20) # two autocorrelations outside of 95% limit
augment(fit_ets) %>% # oh yeah - correlation at 0.05% significance 
  features(.innov, ljung_box, lag = 20, dof = 6)

# Correlation present in ETS residuals 
# AIC, AICc, BIC much better for ARIMA


bind_rows(
  fit_arima %>% accuracy(),
  fit_ets %>% accuracy(),
  fit_arima %>% forecast(h = 6) %>% accuracy(ts), 
  fit_ets %>% forecast(h = 6) %>% accuracy(ts)
) %>% 
  select(-ME, -MPE, -ACF1)
# ARIMA better on all metrics for test and train data

# plotting
ts %>% 
  model(ARIMA(Close)) %>%
  forecast(h = 12) %>%
  autoplot(ts)

# fitting best model
fit <- ts %>%
  model(ARIMA(Close))

# forecasted values
fc <- fit %>% forecast(h = 12)


# ================================================================






s.test <- ts(df$AAPL, frequency = 53, start = 2020)

f2 <- forecast(
  s.test,
  method = c("arima"),
  h = 24
)
plot(f2)

#










