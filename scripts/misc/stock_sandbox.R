
# Stock Sandbox

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


test = py$data_receive("TSLA")


dup <- tail(test,2)
if(dup[1,1] == dup[2,1]){
  data <- slice(test,1:n()-1)
}

data$Week = yearweek(as.Date(data$Date))

ts <- data %>%
  group_by(Week) %>%
  summarize(Close = sum(Close)) %>%
  as_tsibble(index = Week)

tsdisplay(ts)
autoplot(ts)

# Classical Decomposition (additive)
m1 <- ts %>%
  model(
    classical_decomposition(Close, type="additive")
  ) %>%
  components() %>%
  autoplot()

# STL Model
m2 <- ts %>%
  model(
    STL(Close ~ trend(window = 7) +
          season(window = "periodic"),
        robust = TRUE)) %>%
  components() %>%
  autoplot()

# TSLM Model
m3 <- ts %>% 
  model(
    TSLM(Close ~ trend())) %>%
  report()

resid <- augment(m3)
ggplot(data=resid, mapping=aes(y=.resid, x=Week))+
  geom_point()+
  geom_line()+
  ggtitle("TSLM - Model Residuals")

mean(resid$.resid)


close.fit <- ts %>%
  model(TSLM(log(Close) ~ trend() + season()))

close.fc <- forecast(close.fit)

close.fc %>% autoplot(ts)






tickers = py$mylist





close.fit %>% gg_tsresiduals()


gg_subseries(ts)
gg_season(ts)















## - start

data <- py$data_receive("DIDI")
data <- py$data_receive("NIO")
data <- py$data_receive("TWTR")
data <- py$data_receive("AAPL")

  dup <- tail(data,2)
  if(dup[1,1] == dup[2,1]){
    data <- slice(data,1:n()-1)
  }
  data$Week = yearweek(as.Date(data$Date))
  data$day = as.POSIXlt(data$Date)$wday

  

  ts <- data %>%
    group_by(Week) %>%
    summarize(Close = (sum(Close) / length(day)),
              Volume = sum(Volume),
              Trend = ma(Date, order = Week)) %>%
    as_tsibble(index = Week)




  
  
  close.fit <- ts %>%
    model(TSLM(log(Close) ~ trend() + season()))
  close.fc <- forecast(close.fit, h=8)
  
  close.fc %>% autoplot(ts)

  
  close.fit <- ts %>%
    model(TSLM(Close ~ trend() + season()))
  close.fc <- forecast(close.fit, h=8)
  close.fc %>% autoplot(ts)
  

  
  
  fc.vals <- ts %>%
    model(TSLM(log(Close) ~ trend() + season())) %>%
    forecast(h=12, bootstrap = TRUE, times=100)
  fc.vals %>% autoplot(ts)
  
# 3 
  fc.vals <- ts %>%
    model(TSLM(Close ~ trend() + season())) %>%
    forecast(h=12, bootstrap = TRUE, times=100)
  fc.vals %>% autoplot(ts)
  
  
  
# 2
  fc.vals <- ts %>%
    model(TSLM(log(Close))) %>%
    forecast(h=12, bootstrap = TRUE, times=100)
  fc.vals %>% autoplot(ts)

# 1  
  fc.vals <- ts %>%
    model(TSLM(Close)) %>%
    forecast(h=12, bootstrap = TRUE, times=50)
  fc.vals %>% autoplot(ts)


  
  
  ts %>%
    model(
      classical_decomposition(Volume, type="additive")
    ) %>%
    components() %>%
    autoplot()
  
  

fc.vals <- ts %>%
  model(TSLM(Close)) %>%
  forecast(h=12, bootstrap = TRUE, times=100)

fc.vals %>% autoplot(ts)








lambda <- ts %>%
  features(Close, features = guerrero) %>%
  pull(lambda_guerrero)
#fc.vals %>% autoplot(box_cox(ts,lambda))


ts %>%
  model(
    classical_decomposition(Close, type="additive")
  ) %>%
  components() %>%
  autoplot(box_cox(Close, lambda)) + 
  theme(legend.position = "none")

fc.vals %>% autoplot(ts)



fc.vals <- ts %>%
  model(box_cox(TSLM(Close),lambda))# %>%
  forecast(h=12, bootstrap = TRUE, times=100)
fc.vals %>% autoplot(ts)


## - end



  close.fit <- ts %>%
    model(TSLM(log(Close) ~ trend() + season()))
  close.fc <- forecast(close.fit, h=8, bootstrap = TRUE, times=100)
  
  close.fc %>% autoplot(ts)

test <- ts
test[,2] <-  BoxCox(ts$Close, lambda)
  


fc.vals <- test %>%
  model(TSLM(Close)) %>%
  forecast(h=12, bootstrap = TRUE, times=50)
fc.vals %>% autoplot(test)






  
  ts <- data %>%
    group_by(Week) %>%
    summarize(Close = sum(Close),
              Days = length(day),
              avgC = Close / Days) %>%
    as_tsibble(index = Week)


  fc.vals <- ts %>%
    model(TSLM(log(Close))) %>%
    forecast(h=12, bootstrap = TRUE, times=100)# %>%
  hilo()
  
  fc.vals <- as.data.frame(forecast(close.fit, h=8) %>%
                             hilo())




  fc.vals <- ts %>%
    model(TSLM(log(Close))) %>%
    forecast(h=12, bootstrap = TRUE, times=1000) %>%
    hilo()
  
  #ticker = ticker
  fc.vals = as.data.frame(fc.vals)
  #names(fc.vals)[1] <- "Ticker"
  #fc.vals$Ticker = ticker

















test <- seasadj(stl(ts, s.window="periodic"))
adj.d <- as.data.frame(cbind(test, gas$Gas, gas$Quarter))








test <- test[,c(1,5,6)]
test <- test[,c(1,6)]


ts1 <- ts(test$`Adj Close`, frequency = 365, start=2020)
ts1 <- as.data.frame(ts(test, frequency = 365, start=2020))



ts %>%
  pivot_longer(-Date) %>%
  ggplot(aes(Date, value)) + 
  geom_line() + 
  geom_point() 


#dates = as.Date(test[,1], format = "%Y-%m-%d")




ts.2 <- test %>%
  group_by(Date) %>%
  #summarize(Demand = sum(Demand)) %>%
  #mutate(Month = yearmonth(Date)) %>%
  as_tsibble(index = Date)





data.ts.1 <- test %>%
  mutate(Date = yearmonth(test$Date))) %>%
  as_tsibble(index=Date)

data.ts.1 <- test %>%
  mutate(Month = yearmonth(test$Date)) %>%
  {table(.$Date, .$'Adj Close')}



ts %>%
  model(
    classical_decomposition('Adj Close', type="additive")
  ) %>%
  components() %>%
  autoplot() %>%
  fill_gaps('Adj Close' = as.integer(median('Adj Close')))


ts %>%
  model(
    classical_decomposition('Adj Close', type="additive")
  ) %>%
  components() %>%
  autoplot() %>%
  fill_gaps(Date = as.integer(median(Date)))



ts.4 = as.data.frame(ts(test, frequency=365, start=2018))



ts.4 <- test %>%
  #group_by(Date) %>%
  mutate(Month = yearmonth(test$Date)) %>%
  as_tsibble(index = Date)


#ts$s.adj = seasadj(stl(ts, s.window="periodic"))