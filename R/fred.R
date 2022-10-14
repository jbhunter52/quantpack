#FredR


#library(tidyverse, fredr, data.table)

#' Sets FRED API Key from environment variable, fred_key
#'
#' @return data.frame
#' @export
set_fredr_key <- function()
{
  key <- Sys.getenv('fred_key')
  if (nchar(key) < 1)
    print('Cannot find fred_key env variable')
  fredr::fredr_set_key(key)
  return(fredr::fredr_has_key())
}


#' Load time series data from FRED
#'
#' @param ticker Name of ticker from FRED
#' @return data.frame
#' @export
get_rate <- function(ticker)
{
  fredr::fredr(series_id = ticker) %>%
    rename(ticker=series_id,
           close=value) %>%
    select(date, ticker, close)
}

#' Load time series data from FRED
#' Returns c('DGS1MO','DGS3MO','DGS6MO','DGS1','DGS2','DGS3','DGS5','DGS7','DGS10','DGS20','DGS30')
#'
#' @param tickers Vector of tickers from FRED
#' @return data.frame
#' @export
get_all_rates <- function()
{
  tickers <- c('DGS1MO','DGS3MO','DGS6MO','DGS1','DGS2','DGS3','DGS5','DGS7','DGS10','DGS20','DGS30')
  rates <- data.table::rbindlist(lapply(tickers, get_rate)) %>%
    group_by(ticker) %>%
    fill(close, .direction='down')
}


#' Gets all US treasury rate tickers
#' Returns c('DGS1MO','DGS3MO','DGS6MO','DGS1','DGS2','DGS3','DGS5','DGS7','DGS10','DGS20','DGS30')
#'
#' @return Vector of tickers from FRED
#' @export
get_all_USrate_tickers <- function()
{
  return (c('DGS1MO','DGS3MO','DGS6MO','DGS1','DGS2','DGS3','DGS5','DGS7','DGS10','DGS20','DGS30'))
}


#' Gets all possible spreads between tickers
#' Returns dataframe of spreads
#'
#' @return Dataframe of spreads
#' @export
get_rate_spreads <- function(rates, tickers_ordered)
{
  df <- rates %>%
    filter(ticker %in% tickers_ordered)

  dfs <- list()
  for (i in 1:length(tickers_ordered))
  {
    df_ <- df %>%
      filter(ticker == tickers_ordered[i]) %>%
      select(date, close) %>%
      setNames(c('date',tickers_ordered[i]))
    dfs[[i]] <- df_
  }

  dfspreads <- list()
  cnt <- 1
  for (i in length(tickers_ordered):2)
  {
    for (j in 1:i)
    {
      if (i==j) next

      sub_tickers <- c(tickers_ordered[i], tickers_ordered[j])

      if (min(dfs[[i]]$date) < min(dfs[[j]]$date))
      {
        df_ <- dfs[[i]] %>%
          left_join(dfs[[j]], by='date')
      }
      else
      {
        df_ <- dfs[[j]] %>%
          left_join(dfs[[i]], by='date')
      }

      sp_name <- paste(tickers_ordered[i],tickers_ordered[j],sep='_')
      df_ <- df_ %>%
        drop_na() %>%
        mutate('{tickers_ordered[i]}_{tickers_ordered[j]}' := get(tickers_ordered[i]) - get(tickers_ordered[j])) %>%
        select(c('date',sp_name))

      dfspreads[[cnt]] <- df_
      cnt <- cnt+1
    }
  }

  most_days <- 0
  mind <- Sys.Date()
  for (i in 1:length(dfspreads))
  {
    if (min(dfspreads[[i]]$date) < mind)
    {
      most_days <- i
      mind <- min(dfspreads[[i]]$date)
    }
  }

  spreads <- dfspreads[[most_days]]
  for (i in 1:length(dfspreads))
  {
    if (i == most_days) next

    spreads <- spreads %>%
      left_join(dfspreads[[i]], by='date')
  }

}
