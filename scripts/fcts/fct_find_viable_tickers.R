find_viable_tickers <- function(df_ticker_candidates) {
  
  df_data <- BatchGetSymbols(tickers = df_ticker_candidates$ticker,
                  first.date = '2015-01-01',
                  thresh.bad.data = 0.1,
                  #cache.folder = 'bgs_cache', 
                  do.parallel = TRUE, 
                  be.quiet = TRUE
  )[[2]]
  
  df_ticker_candidates <- df_ticker_candidates %>%
    filter(ticker %in% df_data$ticker)
  
  return(df_ticker_candidates)
  
}
