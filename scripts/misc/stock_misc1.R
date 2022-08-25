#
# stock misc
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
    summarize(Close = (sum(Close) / length(day)),
              Volume = sum(Volume)) %>%
    as_tsibble(index = Week)
  
  return(ts)
}


data <- py$data_receive("AAPL")


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



df <- ts[,1]
for(i in 1:length(tickers)){
  ts <- my.prep(py$data_receive(tickers[1]))
  df[,i+1] = ts[,2]
  names(df)[i+1] <- tickers[i]
}


df <- ts[,1]
for(i in 1:length(tickers)){
  ts <- my.prep(py$data_receive(tickers[i]))
  
  for(r in 1:nrow(ts)){
    df[r,i+1] = ts[r,2]
    
  }
  
  
  
  names(df)[i+1] <- tickers[i]
}











# AAPL
ts <- my.prep(py$data_receive("AAPL"))
autoplot(ts)
gg_season(ts)
mean(ts$Close)

m1 <- ts %>%
  model(TSLM(log(Close) ~ trend() + season())) 

m1.vals <- m1 %>% forecast(h=12, bootstrap = TRUE, times=100)
m1.vals %>% autoplot(ts)

m1 %>% gg_tsresiduals()

# DIDI
ts <- my.prep(py$data_receive("NU"))
autoplot(ts)
gg_season(ts)
mean(ts$Close)

m2 <- ts %>%
  model(TSLM(log(Close) ~ trend() + season()))
  
m2.vals <- m2 %>% forecast(h=12, bootstrap = TRUE, times=100) %>% hilo()
m2.vals %>% autoplot(ts)

m2 %>% gg_tsresiduals()


# NIO
ts <- my.prep(py$data_receive("NIO"))
tsautoplot(ts)
gg_season(ts)
mean(ts$Close)

m3 <- ts %>%
  model(TSLM(Close ~ fourier(K = 12)))

m3.vals <- m3 %>% forecast(h=12, bootstrap = TRUE, times=100)
m3.vals %>% autoplot(ts)

m3 %>% gg_tsresiduals()

augment(m4) %>%
  ggplot(aes(x = Week)) +
  geom_line(aes(y = Close, colour = "Actual")) +
  geom_line(aes(y = .fitted, colour = "Fitted"))



# TWTR
ts <- my.prep(py$data_receive("TWTR"))
autoplot(ts)
gg_season(ts)
mean(ts$Close)

m4 <- ts %>%
  model(TSLM(log(Close) ~ trend() + fourier(K = 24))) 
m4.vals <- m4 %>% forecast(h=12, bootstrap = TRUE, times=100)
m4.vals %>% autoplot(ts)

report(fc.vals)


# TSLA
ts <- my.prep(py$data_receive("TSLA"))
autoplot(ts)
gg_season(ts)
mean(ts$Close)

m5 <- ts %>%
  model(TSLM(Close ~ trend() + season()))
m5.vals <- m5 %>% forecast(h = 12, bootstrap = TRUE, times = 100)
m5.vals %>% autoplot(ts)








comp <- augment(m5)
sum(comp$.innov)
sd(comp$Close)
sd(comp$.fitted)
var(comp$Close)
var(comp$.fitted)





fit <- ts %>%
  model(tslm = TSLM(Close ~ trend() + season()))# %>%
  forecast(h=12, bootstrap = TRUE, times=50)
  
fc <- fit %>%
  forecast(h=12, bootstrap = TRUE, times=50)

fc %>% autoplot(ts)


augment(fc.vals) %>%
  ggplot(aes(x = Week)) +
  geom_line(aes(y = Close, colour = "Actual")) +
  geom_line(aes(y = .fitted, colour = "Fitted"))

augment(fc) %>%
    ggplot(aes(x = Week)) +
    geom_line(aes(y = Close, colour = "Actual"))

plot(x=fc$Week  ,y = fc$.mean)
ggplot(data = fc) + 
  geom_line(mapping=aes(x = Week, y = .mean))


fit %>% gg_tsresiduals()
gg_season(ts)










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
      model(TSLM(log(Close) ~ fourier(K=11))),
    ml <- ts %>%
      model(TSLM(log(Close) ~ trend() + season())), 
    ml <- ts %>%
      model(TSLM(log(Close) ~ trend() + season() + fourier(K=11))), 
    
    # regular models
    ml <- ts %>%
      model(TSLM(Close)),
    ml <- ts %>%
      model(TSLM(Close ~ trend())),
    ml <- ts %>%
      model(TSLM(Close ~ season())),
    ml <- ts %>%
      model(TSLM(Close ~ fourier(K = 11))),
    ml <- ts %>%
      model(TSLM(Close ~ trend() + season())),
    ml <- ts %>%
      model(TSLM(Close ~ trend() + season() + fourier(K=11))),
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




test <- mod.comp(df2,1)
test <- best.log(df2,1)
test <- tab.fill(df2,log.table)


tab.fill <- function(ts, table){
  for(i in 1:12){
    table[i,2:8] = mod.comp(ts,i)
    table[i,9] = sum(abs(table[i,5:8]))
  }
  best.model <- table[table$min == min(table$min),1]
  #return(best.model)
  return(table)
}



tab.iter = best.log(df2, 6)



# from actual 4


vicD <- aus_livestock %>%
  dplyr::filter(Animal == "Pigs", State == "Victoria") %>%
  select(-Animal, -State) %>%
  mutate(Month = yearmonth(as.Date(Month, format="%Y/%U"))) %>%
  as_tsibble(index = Month)


idk <- df %>%
  select(df[,c(1,2)]) %>%
  as_tsibble(index = Wk)



ticker.best.pred <- function(ticker){
  
  ticker = ticker
  one.ticker <- df %>%
    select(Wk, ticker) %>%
    as_tsibble(index = Wk)
  
  return(one.ticker)
}

apl <- ticker.best.pred("AAPL")
ts <- apl

names(ts)[2] <- "Close"












