calc_performance <- function(df_in) {
  
  tab <- df_in %>%
    group_by(ticker) %>%
    summarise(
      n_years = (max(ref.date) -
                   min(ref.date))[[1]]/365,
      total_ret = last(price.adjusted)/
        first(price.adjusted) - 1,
      ret_per_year = (1 + total_ret)^(1/n_years) - 1,
      max_drawdown = maxDrawdown(ret.adjusted.prices)
    ) %>%
    arrange(desc(total_ret)) %>%
    mutate(total_ret = percent(total_ret, accuracy = 0.01),
           ret_per_year = percent(ret_per_year, accuracy = 0.01),
           max_drawdown = percent(max_drawdown, accuracy = 0.01)) %>%
    rename(`Ticker` = ticker,
           `Number of Years` = n_years,
           `Total Return` = total_ret,
           `Return per Year` = ret_per_year,
           `Max. Drawdown` = max_drawdown)
  
  return(tab)
}

calc_cumret <- function(df_prices, ticker_in) {
  df_prices_temp <- df_prices %>%
    select(ref.date, ticker, ret.adjusted.prices) %>%
    filter(ticker == ticker_in) %>%
    na.omit()
  
  df_prices_temp$ret.adjusted.prices[1] <- 0 
  df_prices_temp$cum_ret <- cumprod(1+df_prices_temp$ret.adjusted.prices)
  
  return(df_prices_temp)
}
