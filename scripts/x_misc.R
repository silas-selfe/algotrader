
# misc

# 4_evaluate


from = 24
to = 8

stock(24,8,df1, 'FTCH')


uh <- stock(24,8,df1, 'AMC')




fit1_co <- summary(fit1)$coefficients
fit2_co <- summary(fit2)$coefficients
(marg1 <- margins(fit1, data=df_c))


from = 24
to = 8
stock = 'AAPL'
stock <- df1[,c(1,5)]

df_c <- test[c(24:8), c(1,2)]
df_c <- df[c(lastP:toP),c('Date',stock)]

x_axis <- seq(-24, 8, 1)
df_c[,1] <- x_axis



test <- df1 %>%
  select(Date, TSLA) %>%
  as_tsibble(index = Date)

#library(zoo)
test %>%
  autoplot(TSLA)
  geom_line(aes(y='5-MA'), colour="#D55E00")


stock <- function(from, to, df1, ticker){
  lastP <- nrow(df1) - (from+12)
  toP <- nrow(df1) -  (12-to)
  
  df_c <- df1[c(lastP:toP),c('Date',ticker)]
  x_axis <- seq(-from, to, 1)
  df_c[,1] <- x_axis
  
  
  df_c$color = "black"
  df_c$color[df_c$Date>0] = "green"
  df_c$color[df_c$Date==0] = "red"
  df_c$color[df_c$Date<0] = "blue"
  #names(df_c) <- c("Date","price","color")
  
  # fit lines
  fit1 <- lm(price~poly(Date,2,raw=TRUE), data=df_c)
  fit2 <- lm(price~poly(Date,3,raw=TRUE), data=df_c)
  
  
  plot(df_c$Date, df_c$price, pch = 1,col=df_c$color,
       main=ticker,
       xlab="Week",
       ylab="Price ($)")
  lines(x_axis, predict(fit1), col='green')
  lines(x_axis, predict(fit2), col='blue')
  
  
  # comparing fit lines
  currentMomentum <- summary(fit1)$coefficients[1] - 
    summary(fit2)$coefficients[1]
  
  return(currentMomentum)
}



lastMonth <- nrow(df) - 24
nextMonth <- nrow(df) + 8

df2 <- df1[c(lastMonth:nextMonth),]



thisOne <- df2[,c(1,3)]
x_axis <- seq(-24,8,1)
thisOne[,1] <- x_axis

plot(df[,1],df[,3])
plot(df2[,1],df2[,3])


fit1 <- lm(PTON~poly(Date,2,raw=TRUE), data=thisOne)
fit2 <- lm(PTON~poly(Date,3,raw=TRUE), data=thisOne)

plot(thisOne$Date, thisOne$PTON, pch = 1,col=thisOne$Colour)

lines(x_axis, predict(fit1), col='green')
lines(x_axis, predict(fit2), col='blue')




thisOne$Colour="black"
thisOne$Colour[thisOne$Date>=0]="red"
thisOne$Colour[thisOne$col_name2<0]="blue"
