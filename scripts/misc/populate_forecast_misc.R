# df_populate_misc



#

#library(readxl)
#library(seasonal)
#library(ggplot2)
#library(forecast)
#library(tidyverse)

#library(lubridate)
#library(fable)
#library(zoo)
#library(caret)
#library(janitor)



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



tickers <- py$mylist
df <- data.frame(seq(as.Date("2019-12-30"),as.Date(Sys.Date()), by="weeks"))
names(df) <- "Date"

for(i in 1:length(tickers)){
  ts1 <- my.prep(py$data_receive(tickers[i]))
  df[,i+1] <- ts1$Close[match(df$Date,ts1$Wk)]
  
  names(df)[i+1] <- tickers[i]
}



# forecasting_misc



#####

#library(tidyverse)
#library(lubridate)
#library(zoo)
#library(caret)
#library(janitor)
#library(reticulate)
#library(readxl)




for(i in 1:length(tickers)){
  best <- bestModel(na.omit(df[,c(1,i+1)]))
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
  
  for(r in 2:13){
    futureVals[r,i] <- fc$mean[r]
  }
  
}


ts1 <- my.prep(py$data_receive(tickers[3]))
train <- ts1 %>% filter_index(. ~"2021-08-03")
ts1 %>% autoplot()


## ARIMA
fit_arima <- train %>% model(ARIMA(Close))

## ETS
fit_ets <- train %>% model(ETS(Close))

# Best Testing model
decision <- bind_rows(
  fit_arima %>% accuracy(),
  fit_ets %>% accuracy(),
  fit_arima %>% forecast(h = 6) %>% accuracy(ts1), 
  fit_ets %>% forecast(h = 6) %>% accuracy(ts1)
) %>% 
  select(-ME, -MPE, -ACF1)
# ARIMA better on all metrics for test and train data



#report(fit_arima)

#fit_arima %>% gg_tsresiduals(lag_max = 20)
#augment(fit_arima) %>% # no residual correlation
#  features(.innov, ljung_box, lag = 20, dof = 6) 

#report(fit_ets)

#fit_ets %>% gg_tsresiduals(lag_max = 20) # two autocorrelations outside of 95% limit
#augment(fit_ets) %>% # oh yeah - correlation at 0.05% significance 
#  features(.innov, ljung_box, lag = 20, dof = 6)

# Correlation present in ETS residuals 
# AIC, AICc, BIC much better for ARIMA

# plotting
ts1 %>% 
  model(ARIMA(Close)) %>%
  forecast(h = 12) %>%
  autoplot(ts1)

# fitting best model
fit <- ts1 %>%
  model(ARIMA(Close))

# forecasted values
fc <- fit %>% forecast(h = 12)




ts <- ts(ts[,2], frequency = 52, start = 2020)
ts <- ts(df[,2], frequency = 52, start = 2020)

f2 <- forecast(
  ts,
  method = c("ets"),
  h = 12
)
plot(f2)



ts <- ts(df$SOFI, frequency = 52, start = 2020)
f2 <- forecast(
  ts,
  method = c("ets"),
  h = 24
) 
plot(f2)

fc2 <- f2 %>% forecast(ts, method = c("arima"), h=24)


f2 <- forecast(
  ts,
  method = c("ets"),
  h = 24
)

f2$fitted
f2$lower


stock <- (df[,c(1,4)])
bestModel(df[,c(1,4)])
bestModel(df[,4])


















# creates dataset of all tickers received and populates with average 
# weekly closing price. 
ts <- my.prep(py$data_receive('AAPL'))
ts <- my.prep(py$data_receive(tickers[1])) 
df <- ts[,1]
for(i in 1:length(tickers)){
  ts1 <- my.prep(py$data_receive(tickers[i]))
  
  for(r in 1:nrow(ts)){
    df[r,i+1] = ts1[r,2]
  }
  names(df)[i+1] <- tickers[i]
}















## GARBAGE
difftime(strptime(Sys.Date(), format = "%Y-%m-%d"),
         strptime("01.01.2020", format = "%d.%m.%Y"),units="weeks")