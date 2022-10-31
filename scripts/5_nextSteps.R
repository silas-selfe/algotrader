
# Next Steps


#library(zoo)


test <- df1 %>%
  select(Date, AAPL) %>%
  as_tsibble(index = Date)

test %>%
  autoplot(AAPL) #+
  geom_line(aes(y='5-MA'), colour="#D55E00")
