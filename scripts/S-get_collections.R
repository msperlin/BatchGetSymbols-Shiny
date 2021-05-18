library(BatchGetSymbols)
library(tidyverse)

my_d <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my_d)

get_all_b3_stocks <- function() {
  
  # get list of ibovespa's tickers
  myUrl <- 'http://bvmf.bmfbovespa.com.br/suplemento/ExecutaAcaoDownload.asp?arquivo=Titulos_Negociaveis.zip&server=L'
  destfile <- tempfile(fileext = '.zip')
  
  download.file(myUrl, destfile = destfile, mode = 'wb',quiet = T)
  unzip(zipfile = destfile, exdir = tempdir())
  
  textFile <- file.path(tempdir(), 'TITULOS_NEGOCIAVEIS.txt')
  
  my.txt <- readLines(textFile)
  idx <- sapply(my.txt, stringr::str_sub, start=1,end=2, USE.NAMES = F)=='02'
  my.txt <- my.txt[idx]
  cat(paste0(my.txt, collapse = '\n'), file = textFile)
  
  # dfOut <- read.fwf(file = my.txt, widths = c(2, 12, 18),
  #                   header = F,
  #                   col.names = c("Tipo", "Codigo", "CodigoBDI"),
  #                   skip = 1)
  
  my.width <- c(2,12,4,3,60,12,12,03,03,15,07,10,10)
  my.cols <- c("Tipo",
               "CodigoPapel",
               "CodigoEmpresa",
               'CodigoBDI','descBDI','isin_papel','isin_objeto','num','CodigoMercado',
               'DescMercado','num_serie','spec1','datavenc')
  dfOut <- read.fwf(file = textFile,
                    widths = my.width,
                    header = F,
                    col.names = my.cols,
                    skip = 1, fileEncoding = 'ISO 8859-1')
  
  file.remove(c(destfile, textFile))
  
  dfOut <- dplyr::filter(dfOut, CodigoBDI==2)
  
  tickers <- as.character(dfOut$CodigoPapel)
  tickers <- str_trim(tickers)
  tickers <- paste(tickers, '.SA', sep = '')
  
  return(tibble(
    collection = 'B3/Bovespa',
    ticker = tickers,
    sector = NA
  ))
  
}

df_ibov <- get_all_b3_stocks()

df_sp500 <- GetSP500Stocks() %>%
  mutate(collection = 'SP500 Components',
         ticker = Tickers,
         sector = GICS.Sector) %>%
  select(collection, ticker, sector)

df_collections <- bind_rows(
  df_ibov, 
  df_sp500
)

write_csv(df_collections, 
          'collections.csv')