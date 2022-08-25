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

###
glance(ml4) %>%
  select(adj_r_squared, CV, AIC, AICc, BIC)


other <- glance(ml1) %>%
  select(adj_r_squared, CV, AIC, AICc, BIC)
###

m1 <- ts %>%
  model(TSLM(Close))

m2 <- ts %>%
  model(TSLM(Close ~ trend()))

m3 <- ts %>%
  model(TSLM(Close ~ season()))

m4 <- ts %>%
  model(TSLM(Close ~ fourier(K = 12)))

m5 <- ts %>%
  model(TSLM(Close ~ trend() + season()))

m6 <- ts %>%
  model(TSLM(Close ~ trend() + season() + fourier(K=12)))




comp <- augment(m5)
my.sd <- abs(sd(comp$Close)) - abs(sd(comp$.fitted))






# Very very very nifty -- probably the best route
turn <- aus_retail %>%
  index_by(Month) %>%
  filter(Industry == "Food retailing") %>%
  summarize(Turnover = sum(Turnover)) %>%
  as_tsibble(index = Month)
  
train <- turn %>%
  filter_index(.~ "2015 Nov")



my_dcmp_spec <- decomposition_model(
  STL(log(Turnover) ~ season(window = Inf)),
  ETS(season_adjust ~ season("N")), SNAIVE(season_year)
) 


myfit <- train %>%
  model(stl_ets = my_dcmp_spec) %>%
  forecast(h = "3 years")
fc <- myfit %>% accuracy(turn)

myfit2 <- train %>%
  model(
    ETS(Turnover),
    SNAIVE(Turnover)
  ) %>% 
  forecast(h = "3 years") %>%
  accuracy(turn)


compare <- rbind(fc, myfit2)
train %>%
  model(
    ETS(Turnover)
  ) %>%
  forecast(h = "3 years") %>%
  autoplot(turn)