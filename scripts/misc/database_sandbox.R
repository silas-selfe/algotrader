#
# database sandbox
#


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
    summarize(Close = (sum(Close) / length(day))) %>%
    as_tsibble(index = Wk)
  
  return(ts)
}





aapl <- py$data_receive("AAPL")
aapl$Date <- as.numeric(as.POSIXct(aapl$Date))
as.numeric(aapl$Date)

# write.csv(aapl, "~/Projects/algotrader/data/AAPL.Apr07_2022.csv", row.names=FALSE)
as.numeric(my_date)







