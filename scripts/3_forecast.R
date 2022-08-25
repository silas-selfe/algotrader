
# Forecasting


## ---- libraries

library(dplyr)
library(seasonal)
library(ggplot2)
library(forecast)
library(tsibbledata)
library(fable)
library(tsibble)
library(feasts)





## ---- training
bestModel <- function(stock){
  ticker <- colnames(stock)[2]
  names(stock)[2] <- "Close"
  
  ts <- stock %>%
    as_tsibble(index = Date)
  
  train <- ts %>% filter_index(. ~"2021-08-03")
  
  
  ## ARIMA
  fit_arima <- train %>% model(ARIMA(Close))
  
  ## ETS
  fit_ets <- train %>% model(ETS(Close))
  
  # Best Testing model
  decision <- bind_rows(
    fit_arima %>% accuracy(),
    fit_ets %>% accuracy(),
    fit_arima %>% forecast(h = 6) %>% accuracy(ts), 
    fit_ets %>% forecast(h = 6) %>% accuracy(ts)
  ) %>% 
    select(-ME, -MPE, -ACF1)
  
                  #       ARIMA              ETS
  type <- ifelse(decision$RMSE[3] - decision$RMSE[4] > 0,
                 "ETS", "ARIMA")
  return(type)
}





## ---- forecasting
futureVals <- data.frame(matrix(ncol = length(df),
                                nrow = 13))
names(futureVals) <- colnames(df)


# populating dates for next three months
futureVals[,1] <- data.frame(seq(as.Date(df[nrow(df),1]),
                                 as.Date(Sys.Date()+84), 
                                 by="weeks")) 


# forecasting each stock with its best performing model
for(i in 2:length(df)){
  best <- bestModel(na.omit(df[,c(1,i)]))
  ts <- ts(df[,i], frequency = 52, start = 2020)
  futureVals[1,i] <- ts[139]
  
  if(best == "ARIMA"){
    fc <- forecast(
      ts,
      method = c("arima"),
      h = 12
    )
  }else if(best == "ETS"){
    fc <- forecast(
      ts,
      method = c("ets"),
      h = 12
    )
  }
  
  for(r in 1:12){
    futureVals[r+1,i] <- fc$mean[r]
  }
}

futureVals <- futureVals[2:nrow(futureVals),]
df1 <- rbind(df, futureVals)


#### write.csv(df1, "~/Projects/algotrader/data/Aug25_22.csv", row.names=FALSE)


