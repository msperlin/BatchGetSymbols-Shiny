# OPTIONS
years_back <- 5
cache_folder_indices <- 'cache-indices'

options(digits = 4,
        accuracy = 0.01)

# load static data
df_collections <- readr::read_csv('data/collections.csv')

available_collections <- sort(unique(df_collections$collection))

stock_indices <- readr::read_csv('data/stock_indices.csv',
                                  col_types = readr::cols())

n_available_stocks <- n_distinct(df_collections$ticker)

