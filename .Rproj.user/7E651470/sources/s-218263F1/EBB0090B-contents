#
# Stock Actual
#

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


## ---- fun.data.prep
data.prep <- function(data){
  dup <- tail(data,2)
  if(dup[1,1] == dup[2,1]){
    data <- slice(data,1:n()-1)
  }
  data$Week = yearweek(as.Date(data$Date))
  data$day = as.POSIXlt(data$Date)$wday
  
  ts <- data %>%
    group_by(Week) %>%
    summarize(Close = (sum(Close) / length(day)),
              Volume = sum(Volume)) %>%
    as_tsibble(index = Week)
  
  return(ts)
}


## ---- fun.pred.plot
pred.plot <- function(ts){
  close.fit <- ts %>%
    model(TSLM(log(Close) ~ trend() + season()))
  close.fc <- forecast(close.fit, h=8)
  
  close.fc %>% autoplot(ts)
}

## ---- fun.pred.vals
pred.vals <- function(ts){
  close.fit <- ts %>%
    model(TSLM(log(Close) ~ trend() + season()))
  fc.vals <- as.data.frame(forecast(close.fit, h=8) %>%
    hilo())
  
  return(fc.vals)
}


## ---- most.active.tickers
tickers <- py$mylist

count = 1
df1 <- pred.vals(data.prep(py$data_receive(tickers[1])))
for(i in 1:20){
  df2 <- pred.vals(data.prep(py$data_receive(tickers[i])))
  df1 <- dplyr::bind_rows(df1, df2)
  #df1[i,1] = tickers[i]
  
}


count = 1
df1[1,1] = tickers[count] # initializing
for(i in 1:nrow(df1)){
  if(i != nrow(df1)){
    c1 <- as.integer(week(df1[i+1, 2]))
    c2 <- as.integer(week(df1[i,2]))
    
    if(c1 - c2 != 1){
      count = count + 1
    }
    df1[i+1,1] = tickers[count]
  }
}










AAPL <- data.prep(py$data_receive("AAPL"))
TSLA <- data.prep(py$data_receive("TSLA"))
BTC <- data.prep(py$data_receive("BTC-USD"))
DIDI <- data.prep(py$data_receive("DIDI"))
AMD <- data.prep(py$data_receive("AMD"))
LAZR <- data.prep(py$data_receive("LAZR"))
BRKB <- data.prep(py$data_receive("BRK-B"))

TWTR <- data.prep(py$data_receive("TWTR"))

NIO <- data.prep(py$data_receive("NIO"))

p1 <- pred.vals(AAPL, "AAPL")
p2 <- my.pred(TSLA)
my.pred(BTC)
my.pred(DIDI)
my.pred(AMD)
my.pred(LAZR)
my.pred(BRKB)
p1 <- pred.vals(AAPL)

pred.plot(AAPL)
pred.plot(TSLA)
pred.plot(TWTR)
pred.plot(NIO)
pred.plot(AMD)
pred.plot(DIDI)


p1 %>% autoplot(AAPL)

