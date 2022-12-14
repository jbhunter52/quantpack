% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load.R
\name{load_ticker}
\alias{load_ticker}
\title{Load time series data from ticker and inputs}
\usage{
load_ticker(ticker, source = "EODHistorical", interval = "1D")
}
\arguments{
\item{ticker}{Name of ticker}

\item{source}{Data source (EODHistorical)}

\item{interval}{Interval of data to load (1m,1D,EOD)}
}
\value{
data.frame
}
\description{
Load time series data from ticker and inputs
}
