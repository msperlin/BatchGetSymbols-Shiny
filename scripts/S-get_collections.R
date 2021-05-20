library(BatchGetSymbols)
library(tidyverse)
library(furrr)

my_d <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my_d)

plan(multisession(workers = 10))

source('fcts/fct_get_b3_stocks.R')
source('fcts/fct_find_viable_tickers.R')

df_ibov_tickers <- get_all_b3_stocks() %>%
  find_viable_tickers()


df_nyse_tickers <- read_delim('data/NYSE.zip', 
                              delim = '\t') %>%
  mutate(collection = 'NYSE',
         ticker = Symbol,
         description = Description
         ) %>%
  select(collection, ticker, description) %>%
  find_viable_tickers()


df_collections <- bind_rows(
  df_ibov_tickers, 
  df_nyse_tickers
)

write_csv(df_collections, 
          'collections.csv')
