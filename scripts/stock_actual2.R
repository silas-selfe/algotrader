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


## ---- data.prep
# Tickers will change daily.
# Takes up to 250 of yahoo finances "highest movers."
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

#### write.csv(df, "~/Projects/algotrader/data/Apr05_2022.csv", row.names=FALSE)


# Takes data for each stock and runs it through 12 models. 
# It then returns descriptive statistics to 'tab.fill'
mod.comp <- function(ts, i){
  
  val <- switch(
    num = i,
    
    # log models
    ml <- ts %>%
      model(TSLM(log(Close))),
    ml <- ts %>%
      model(TSLM(log(Close) ~ trend())),
    ml <- ts %>%
      model(TSLM(log(Close) ~ season())), 
    ml <- ts %>%
      model(TSLM(log(Close) ~ fourier(K=12))),
    ml <- ts %>%
      model(TSLM(log(Close) ~ trend() + season())), 
    ml <- ts %>%
      model(TSLM(log(Close) ~ trend() + season() + fourier(K=12))), 
    
    # regular models
    ml <- ts %>%
      model(TSLM(Close)),
    ml <- ts %>%
      model(TSLM(Close ~ trend())),
    ml <- ts %>%
      model(TSLM(Close ~ season())),
    ml <- ts %>%
      model(TSLM(Close ~ fourier(K = 12))),
    ml <- ts %>%
      model(TSLM(Close ~ trend() + season())),
    ml <- ts %>%
      model(TSLM(Close ~ trend() + season() + fourier(K=12))),
  )
  
  comp <- augment(ml)
  
  other <- glance(ml) %>%
    select(adj_r_squared, CV, AIC, AICc, BIC)
  sd.diff = round(abs(sd(comp$Close)) - abs(sd(comp$.fitted)),3)
  var.diff = round(var(comp$Close) - var(comp$.fitted),3)
  
  log.table[i,2] = sd.diff
  log.table[i,3] = var.diff
  log.table[i,4:8] = round(as.double(other[,1:5]),3)
  
  return(c(sd.diff, var.diff, other[,1:5]))
}

# Sends data to 'mod.comp' and receives statistics which it then 
# populates a table with. It sums CV, AIC, AICc and BIC for each model
# to find the minimum total. The model with the minimum total is returned. 
tab.fill <- function(ts, table){
  for(i in 1:12){
    table[i,2:8] = mod.comp(ts,i)
    table[i,9] = sum(abs(table[i,5:8]))
  }
  best.model <- table[table$min == min(table$min),1]
  return(best.model)
}




df <- df %>% 
  as_tsibble(., index = Week) # works

best.model <- colnames(df[1,2:ncol(df)])
log.table <- as.data.frame(matrix(nrow = 12, ncol = 9))
names(log.table)[1:9] <- c("Model", "sd.diff", "var.diff", 
                           "adj_r_sq", "cv", "aic", "aicc", "bic","min")

log.table[1:12,1] <- c("Ml1", "Ml2", "Ml3", "Ml4", "Ml5", "Ml6",
                      "M1", "M2", "M3", "M4", "M5", "M6")

for(i in 1:(ncol(df)-1)){
  df2 <- df[,c(1,i+1)]
  names(df2)[2] <- "Close"
  
  best.model[i] <- tab.fill(df2, log.table)
}



end <- ncol(df)-1

for(i in 1:(ncol(df)-1)){
  print(i+1)
}













## - rough
df2 <- df[,c(1,101)]
names(df2)[2] <- "Close"

ml1 <- df2 %>%
  model(TSLM(log(Close)))


name <- colnames(df[,101])
m1.vals <- ml1 %>% forecast(h=12, bootstrap = TRUE, times=100)
m1.vals %>% autoplot(df2) + ggtitle(name)




