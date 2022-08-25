
# Populating Data


## ---- libraries
library(reticulate)
library(tsibbledata)
library(dplyr)
library(tsibble)
library(feasts)



##
# Ensures no duplication of most recent date.
#
# Takes sum of weekly closing price & averages by days market was
# open.
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


###
# Tickers change daily.
# Takes up to 250 of yahoo finances "highest movers."
tickers <- py$mylist
df <- data.frame(seq(as.Date("2019-12-30"),
                     as.Date(Sys.Date()), by="weeks"))
names(df) <- "Date"


# Populates df with average weekly closing price
for(i in 1:length(tickers)){
  ts1 <- my.prep(py$data_receive(tickers[i]))
  df[,i+1] <- ts1$Close[match(df$Date,ts1$Wk)]
  
  names(df)[i+1] <- tickers[i]
}


colSums(is.na(df))
# removing stocks with > 50% na
df <- df[, which(colMeans(!is.na(df)) > 0.5)]





