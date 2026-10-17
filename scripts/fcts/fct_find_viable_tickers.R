find_viable_tickers <- function(df_ticker_candidates) {
  
  df_data <- yf_get(tickers = df_ticker_candidates$ticker,
                  first_date = '2015-01-01',
                  thresh_bad_data = 0.1,
                  #cache_folder = 'bgs_cache', 
                  do_parallel = TRUE, 
                  be_quiet = TRUE
  )
  
  df_ticker_candidates <- df_ticker_candidates %>%
    filter(ticker %in% df_data$ticker)
  
  return(df_ticker_candidates)
  
}
