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
ts <- my.prep(py$data_receive("DIDI"))
autoplot(ts)
gg_season(ts)
mean(ts$Close)

m2 <- ts %>%
  model(TSLM(log(Close) ~ trend() + season()))
  
m2.vals <- m2 %>% forecast(h=12, bootstrap = TRUE, times=50)
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


































