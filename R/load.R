# https://tinyheero.github.io/jekyll/update/2015/07/26/making-your-first-R-package.html
# devtools::document()

EOD_DIR <- '\\\\amd\\d\\EODHistorical'

#' Load time series data from ticker and inputs
#'
#' @param ticker Name of ticker
#' @param source Data source (EODHistorical)
#' @param interval Interval of data to load (1m,1D,EOD)
#' @return data.frame
#' @export
load_ticker <- function(ticker, source='EODHistorical', interval='1D')
{
  if (source == 'EODHistorical' && interval == '1D') interval <- 'EOD'

  source_dir <- get_source_dir(source)
  if (source == 'EODHistorical')
  {
    file_ext <- '.parquet'
    source_dir <- EOD_DIR
    file <- paste0(EOD_DIR,'\\',interval,'\\',ticker,file_ext)
    df <- read_parquet(file) %>%
      mutate(date = as.Date(date)) %>%
      mutate(ticker = ticker) %>%
      select(date, ticker, everything())
  }
}

load_source_dir <- function(source)
{
  if (source == 'EODHistorical')
    return(EOD_DIR)
}


#' Load time series data from mulitple tickers
#'
#' @param ticker Vector of tickers
#' @param source Data source (EODHistorical)
#' @param interval Interval of data to load (1m,1D,EOD)
#' @return data.frame
#' @export
load_tickers <- function(tickers, source='EODHistorical', interval='1D')
{
  data.table::rbindlist(lapply(tickers, load_ticker, source=source, interval=interval))
}

