#
# Stock misc 3
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

#### df$Week <- as.numeric(as.POSIXct(df$Week))
#### write.csv(df, "~/Projects/algotrader/data/Apr07_2022.csv", row.names=FALSE)


# Takes data for each stock and runs it through 12 models. 
# It then returns descriptive statistics to 'tab.fill'
mod.comp <- function(ts, i){
  val <- switch(
    num = i,
    
    # log models
    ml <- ts %>% # Ml1
      model(TSLM(log(Close))),
    ml <- ts %>% # Ml2
      model(TSLM(log(Close) ~ trend())),
    ml <- ts %>% # Ml3
      model(TSLM(log(Close) ~ season())), 
    ml <- ts %>% # Ml4
      model(TSLM(log(Close) ~ fourier(K=12))),
    ml <- ts %>% # Ml5
      model(TSLM(log(Close) ~ trend() + season())), # use for na's
    ml <- ts %>% # Ml6
      model(TSLM(log(Close) ~ trend() + season() + fourier(K=12))), 
    
    # regular models
    ml <- ts %>% # M1
      model(TSLM(Close)),
    ml <- ts %>% # M2
      model(TSLM(Close ~ trend())),
    ml <- ts %>% # M3
      model(TSLM(Close ~ season())),
    ml <- ts %>% # M4
      model(TSLM(Close ~ fourier(K = 12))),
    ml <- ts %>% # M5
      model(TSLM(Close ~ trend() + season())),
    ml <- ts %>% # M6
      model(TSLM(Close ~ trend() + season() + fourier(K=12))),
  )
  comp <- augment(ml)
  
  other <- glance(ml) %>%
    select(adj_r_squared, CV, AIC, AICc, BIC)

  my.sum <- sum(abs(other[2:5]))
  return(my.sum)
}


tab.fill <- function(ts, table){
  for(i in 1:12){
    table[i,2] = mod.comp(ts,i)
  }
  best <- table[table$statSum == min(table$statSum),1]
  best = best[1]
  return(table)
}



df <- df %>% 
  as_tsibble(., index = Week) # works



best.model <- as.data.frame(matrix(nrow = ncol(df), ncol = 2))
log.table <- as.data.frame(matrix(nrow = 12, ncol = 2))
names(log.table)[1:2] <- c("Model", "statSum")
log.table[1:12,1] <- c("Ml1", "Ml2", "Ml3", "Ml4", "Ml5", "Ml6",
                       "M1", "M2", "M3", "M4", "M5", "M6")


for(i in 1:(ncol(df)-1)){
  df2 <- df[,c(1,i+1)]
  names(df2)[2] <- "Close"
  
  best.model[i,1] <- tickers[i]
  best.model[i,2] <- tab.fill(df2, log.table)
}

names(best.model)[1:2] <- c("Ticker","Model")
best.model$Model <- best.model$Model %>% replace_na('Ml5')


best.preds <- function(ts, b.m){ # best.model
  val <- switch(
    b.m,
    "Ml1" = 1,
    "Ml2"=2,
    "Ml3"=3,
    "Ml4"=4,
    "Ml5"=5,
    "Ml6"=6,
    "M1"=7,
    "M2"=8,
    "M3"=9,
    "M4"=10,
    "M5"=11,
    "M6"=12,
  )
  f.p <- switch( # final.pred
    num = val,
    
    # log models
    ml <- ts %>% # Ml1
      model(TSLM(log(Close))),
    ml <- ts %>% # Ml2
      model(TSLM(log(Close) ~ trend())),
    ml <- ts %>% # Ml3
      model(TSLM(log(Close) ~ season())), 
    ml <- ts %>% # Ml4
      model(TSLM(log(Close) ~ fourier(K=12))),
    ml <- ts %>% # Ml5
      model(TSLM(log(Close) ~ trend() + season())), # use for na's
    ml <- ts %>% # Ml6
      model(TSLM(log(Close) ~ trend() + season() + fourier(K=12))), 
    
    # regular models
    ml <- ts %>% # M1
      model(TSLM(Close)),
    ml <- ts %>% # M2
      model(TSLM(Close ~ trend())),
    ml <- ts %>% # M3
      model(TSLM(Close ~ season())),
    ml <- ts %>% # M4
      model(TSLM(Close ~ fourier(K = 12))),
    ml <- ts %>% # M5
      model(TSLM(Close ~ trend() + season())),
    ml <- ts %>% # M6
      model(TSLM(Close ~ trend() + season() + fourier(K=12))),
  )
  m.vals <- ml %>% forecast(h=12, bootstrap = TRUE, times=100) %>% hilo()
  
  #return(m.vals$.meav)
  return(m.vals)
}

#ttest <- best.preds(df2, b.m)


for(i in 1:(nrow(best.model)-1)){
  df2 <- df[,c(1,i+1)]
  names(df2)[2] <- "Close"
  b.m = best.model[i,2]
  
  all.vals <- best.preds(df2, b.m)
  best.model[i,3:14] <- round(all.vals$.mean,3)
  val.dates <- all.vals$Week
  print(i)
}

best.model[1,3:14] <- best.preds(df2,b.m)



format <- as.data.frame(t(best.model[,c(1,3:14)])) # still need to incorporate date into this
format <- format %>% row_to_names(row_number = 1)






