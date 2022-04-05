#
# Stock misc 2
# 

ts <- my.prep(py$data_receive("TSLA"))



ml1 <- ts %>%
  model(TSLM(log(Close)))

ml2 <- ts %>%
  model(TSLM(log(Close) ~ trend())) 

ml3 <- ts %>%
  model(TSLM(log(Close) ~ season())) 

ml4 <- ts %>%
  model(TSLM(log(Close) ~ fourier(K=12)))

ml5 <- ts %>%
  model(TSLM(log(Close) ~ trend() + season())) 

ml6 <- ts %>%
  model(TSLM(log(Close) ~ trend() + season() + fourier(K=12))) 




m <- ts %>%
  model(TSLM(Close))

m <- ts %>%
  model(TSLM(Close ~ trend()))

m <- ts %>%
  model(TSLM(Close ~ season()))

m3 <- ts %>%
  model(TSLM(Close ~ fourier(K = 12)))

m <- ts %>%
  model(TSLM(Close ~ trend() + season()))

m <- ts %>%
  model(TSLM(Close ~ trend() + season() + fourier(K=12)))




comp <- augment(m5)
my.sd <- abs(sd(comp$Close)) - abs(sd(comp$.fitted))



