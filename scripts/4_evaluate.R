
# Comparing & Picking



## ---- libraries
library(margins)
#library(ISLR)


## ---- comparing

# preps data for other functions
dPrep <- function(from, to, df1, ticker){
  lastP <- nrow(df1) - (from+12)
  toP <- nrow(df1) -  (12-to)
  
  df_c <- df1[c(lastP:toP),c('Date',ticker)]
  x_axis <- seq(-from, to, 1)
  df_c[,1] <- x_axis
  
  return(df_c)
}


# difference between fit lines
momentum <- function(df_c){
  names(df_c) <- c("Date","price")
  
  # fit lines
  fit1 <- lm(price~poly(Date,2,raw=TRUE), data=df_c)
  fit2 <- lm(price~poly(Date,3,raw=TRUE), data=df_c)
  
  # comparing fit lines
  currentMomentum <- summary(fit1)$coefficients[1] - 
    summary(fit2)$coefficients[1]
  
  return(currentMomentum)
}

# plotting
my_plot <- function(df_c){
  ticker <- colnames(df_c[2])
  
  df_c$color = "black"
  df_c$color[df_c$Date>0] = "green"
  df_c$color[df_c$Date==0] = "red"
  df_c$color[df_c$Date<0] = "blue"
  names(df_c) <- c("Date","price","color")
  
  # fit lines
  fit1 <- lm(price~poly(Date,2,raw=TRUE), data=df_c)
  fit2 <- lm(price~poly(Date,3,raw=TRUE), data=df_c)
  
  plot(df_c$Date, df_c$price, pch = 1,col=df_c$color,
       main=ticker,
       xlab="Week",
       ylab="Price ($)")
  lines(df_c[,1], predict(fit1), col='green')
  lines(df_c[,1], predict(fit2), col='blue')
  
}






# testing
from = 24
to = 8
ticker = 'NIO'
momentum(dPrep(from, to, df1, ticker))
my_plot(dPrep(from, to, df1, ticker))


# finding most momentum
pickPrep <- data.frame(matrix(nrow=ncol(df1)-1),
                       ncol=2)
names(pickPrep) <- colnames(df1[,2:ncol(df1)])
names(pickPrep) <- c("Stock", "Score")

for(i in 2:length(df1)){
  pickPrep[i-1,1] <- ticker <- colnames(df1[i])
  pickPrep[i-1,2] <- momentum(dPrep(from, to, df1, ticker))
}

pickPrep <- pickPrep[order(-pickPrep$Score),]
my_plot(dPrep(from,to,df1,'SOFI'))



#
# above is good.
# def want to find better way to evaluate momentum. find where area between
# two curves starts to decrease, then find stocks where that's currently
# happening. 
# 8/25/22





