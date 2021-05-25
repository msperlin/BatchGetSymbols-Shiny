# OPTIONS
years_back <- 5
cache_folder_indices <- file.path(tempdir(), 'bgs_cache')
cache_folder_stocks <- file.path(tempdir(), 'bgs_cache')

options(digits = 4,
        accuracy = 0.01)

# load static data
df_collections <- readr::read_csv('data/collections.csv',
                                  col_types = readr::cols())

available_collections <- sort(unique(df_collections$collection))

# stock_indices <- readr::read_csv('data/stock_indices.csv',
#                                   col_types = readr::cols())

n_available_stocks <- dplyr::n_distinct(df_collections$ticker)

