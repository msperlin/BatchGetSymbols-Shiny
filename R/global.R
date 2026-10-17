# OPTIONS
years_back <- 5

options(digits = 4,
        accuracy = 0.01)

# load static (pre-downloaded) price data -- no network access at runtime.
# rebuild it with: Rscript scripts/build_static_data.R
static_prices <- readRDS('data/static_prices.rds')

static_first_date <- min(static_prices$ref_date)
static_last_date  <- max(static_prices$ref_date)

# load the ticker catalog, keeping only tickers we have prices for
df_collections <- readr::read_csv('data/collections.csv',
                                  col_types = readr::cols())

df_collections <- dplyr::filter(df_collections,
                                ticker %in% static_prices$ticker)

available_collections <- sort(unique(df_collections$collection))

n_available_stocks <- dplyr::n_distinct(df_collections$ticker)

my_caption <- 'Data from Yahoo Finance | <www.msperlin.com/shiny/bgs>'
