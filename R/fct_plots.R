get_subtitle <- function(df_prices) {
  first_date <- min(df_prices$ref.date)
  last_date <- max(df_prices$ref.date)
  
  subtitle_out <- str_glue('Data from {first_date} to {last_date}')
  
  return(subtitle_out)
}

do_multiple_stock_plot <- function(df_prices, input) {
  
  first_date <- min(df_prices$ref.date)
  last_date <- max(df_prices$ref.date)
  
  p <- ggplot(df_prices, aes(x = ref.date, 
                             y = cum_ret,
                             color = ticker)) + 
    geom_line() + 
    labs(title = str_glue('Prices of {n_distinct(df_prices$ticker)} stocks'),
         subtitle = get_subtitle(df_prices),
         x = '',
         y = 'Cumulative Return',
         caption = my_caption
    ) + 
    theme_minimal() +
    scale_y_continuous(labels = scales::percent)
}

do_single_stock_plot <- function(df_prices, input) {
  
  first_date <- min(df_prices$ref.date)
  last_date <- max(df_prices$ref.date)
  
  p <- ggplot(df_prices, aes(x = ref.date, y = price.adjusted)) + 
    geom_line() + 
    labs(title = str_glue('Plot for {isolate(input$ticker)}'),
         subtitle = get_subtitle(df_prices),
         x = '',
         y = 'Adjusted Price',
         caption = my_caption) + 
    theme_minimal()
  
  return(p)
}

do_perf_plot <- function(df_in) {
  
  require(ggridges)
  
  k_vec <- seq(3, 5*12, by = 3)
  df_ret <- bind_rows(
    map(k_vec, calc_horizon_ret, df_in = df_in )
  ) 
  
  my_ticker <- df_in$ticker[1]
  
  p <- ggplot(df_ret, aes(y = factor(k_month), 
                          x = ret_year,
                          fill = stat(x))) +
    geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, 
                                 quantile_lines = TRUE, quantiles = 2) + 
    scale_fill_viridis_c(name = "Return per year", option = "C",
                         labels = percent) + 
    #scale_y_discrete(expand = c(0, 0)) +     # will generally have to set the `expand` option
    #scale_x_continuous(expand = c(0, 0)) +   # for both axes to remove unneeded padding
    #coord_cartesian(clip = "off") + # to avoid clipping of the very top of the top ridgeline
    theme_ridges() + 
    labs(title = str_glue('Annual Returns by Investment Horizon -- {my_ticker}'),
         subtitle = str_glue('Plot shows the distribution of annually equivalent returns by different horizon'),
         x = 'Return per year',
         y = 'Investment Horizon (months)',
         caption = my_caption) + 
    xlim(c(quantile(df_ret$ret_year, 0.025), 
           quantile(df_ret$ret_year, 0.975))) + 
    scale_x_continuous(labels = percent)
  
  return(p)
  
}

get_closest_date <- function(date_in, ref_dates) {
  my_dist <- abs(date_in - ref_dates)
  idx <- which.min(my_dist)
  
  return(idx)
}

calc_horizon_ret <- function(df_in, k_month) {
  
  k_days = floor(k_month*30)
  min_observations <- 5
  
  cli::cli_alert_info('Calculating return by horizon for k = {k_month} months')
  date_vec <- seq(first(df_in$ref.date),
                  last(df_in$ref.date),
                  by = k_days)
  
  if (length(date_vec) < min_observations) return(tibble())
  
  idx_closest_dates <- sapply(date_vec, get_closest_date, ref_dates = df_in$ref.date)
  idx_buy <- idx_closest_dates
  idx_sell <- lead(idx_closest_dates)
  
  buy_price <- df_in$price.adjusted[idx_buy]
  sell_price <- lead(df_in$price.adjusted[idx_sell])
  days_diff <- na.omit(
    as.numeric(df_in$ref.date[idx_sell] - df_in$ref.date[idx_buy])
  )
  
  ret <- na.omit(sell_price/buy_price - 1)
  
  tib_out <- tibble(
    k_month = k_month,
    #buy_price,
    #sell_price,
    ret = ret,
    ret_year = (1+ret)^(1/ (k_month/12)) - 1,
    ticker = df_in$ticker[1]
  )
  
  return(tib_out)
}
# 
# graphics.off()
# df_in <- BatchGetSymbols::BatchGetSymbols(tickers = 'ITSA3.SA', first.date = '2000-01-01', thresh.bad.data = 0.1)[[2]]
# p <- do_perf_plot(df_in)
# 
# 
# x11() ; p
