#
# Stock Actual 2
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
  data$day = as.POSIXlt(data$Date)$wday
  
  ts <- data %>%
    group_by(Week) %>%
    summarize(Close = (sum(Close) / length(day))) %>%
    as_tsibble(index = Week)
  
  return(ts)
}


# tickers will change daily.
# takes up to 250 of yahoo finances "highest movers."
tickers <- py$mylist

# creates dataset of all tickers received and populates with average 
# weekly closing price. 
ts <- my.prep(py$data_receive(tickers[1]))
df <- ts[,1]
for(i in 1:length(tickers)){
  ts <- my.prep(py$data_receive(tickers[i]))
  
  for(r in 1:nrow(ts)){
    df[r,i+1] = ts[r,2]
  }
  
  names(df)[i+1] <- tickers[i]
}

#### write.csv(df, "~/Projects/Finance/data/Apr05_2022.csv", row.names=TRUE)




















