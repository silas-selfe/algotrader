


# pulling financial data

get_key_stats <- function(ticker) {
  get_key_stats_proto <- function(ticker) {
    jsonlite::flatten(jsonlite::fromJSON(glue::glue("https://query2.finance.yahoo.com/v10/finance/quoteSummary/{ticker}?modules=defaultKeyStatistics"))[[1]][[1]][[1]]) %>% 
      select(ends_with(".raw")) %>% 
      `names<-`(sub(".raw","", names(.))) %>% 
      mutate(
        ticker = ticker,
        date = Sys.Date()
      ) %>% 
      select(ticker, date, everything())
  }
  ticker_W <- get_key_stats_proto(ticker)
  ticker_L <- gather(ticker_W, ticker, date, factor_key = T)
  names(ticker_L)[1] <-"Key Stats"
  names(ticker_L)[2] <- "Values"
  ticker_L$Values <- format(ticker_L$Values, digits = 2, nsmall = 1, 
                            scientific = F)
  return(ticker_L)
  
  purrr::map_df(.x = ticker, ~ get_key_stats_proto(.x) )
}


test <- get_key_stats("AAPL")
