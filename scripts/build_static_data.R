# Build a small, static snapshot of price data for the demo.
#
# Downloads a fixed set of tickers once and stores the result in
# data/static_prices.rds. The Shiny app then reads that file at startup
# and never touches the network, so it is fast and works offline.
#
# Re-run this whenever you want to refresh the snapshot:
#   Rscript scripts/build_static_data.R

suppressMessages({
  library(yfR)
  library(dplyr)
  library(readr)
})

first_date <- Sys.Date() - 5 * 365
last_date  <- Sys.Date()

df_collections <- read_csv("data/collections.csv", col_types = cols())

# Curated demo subset (must exist in data/collections.csv to keep descriptions)
b3 <- c(
  "PETR4.SA", "VALE3.SA", "ITUB4.SA", "BBDC4.SA", "ABEV3.SA",
  "B3SA3.SA", "MGLU3.SA", "WEGE3.SA", "BBAS3.SA", "ITSA4.SA"
)

nyse_candidates <- c(
  "JPM", "V", "UNH", "XOM", "JNJ", "WMT", "MA", "PG", "HD", "CVX",
  "KO", "MRK", "DIS", "BA", "CAT", "GE", "IBM", "MCD", "NKE", "T",
  "VZ", "PFE", "BAC", "WFC", "C", "GS", "MMM", "AXP", "CVS", "ABT"
)

indices <- df_collections$ticker[df_collections$collection == "Market Indices"]

b3_sel   <- intersect(b3, df_collections$ticker)
nyse_sel <- head(intersect(nyse_candidates, df_collections$ticker), 25)

tickers <- unique(c(b3_sel, nyse_sel, indices))

cat(
  "Fetching", length(tickers), "tickers for",
  as.character(first_date), "->", as.character(last_date), "\n"
)

df <- yf_get(
  tickers        = tickers,
  first_date     = first_date,
  last_date      = last_date,
  thresh_bad_data = 0.1,
  be_quiet       = TRUE
)

df <- df %>%
  select(
    ticker, ref_date, price_open, price_high, price_low, price_close,
    volume, price_adjusted, ret_adjusted_prices, ret_closing_prices,
    cumret_adjusted_prices
  )

saveRDS(df, "data/static_prices.rds", compress = "xz")

cat(
  "Saved", nrow(df), "rows for", dplyr::n_distinct(df$ticker),
  "tickers ->", "data/static_prices.rds\n"
)

missing <- setdiff(tickers, unique(df$ticker))
if (length(missing) > 0) {
  cat("No data retrieved for:", paste(missing, collapse = ", "), "\n")
}
