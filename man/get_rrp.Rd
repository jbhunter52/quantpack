% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mom.R
\name{get_rrp}
\alias{get_rrp}
\title{Range over recent price history
(x - RcppRoll::roll_meanr(x, n = n)) / RcppRoll::roll_sdr(x, n = n)}
\usage{
get_rrp(x, n = 20)
}
\arguments{
\item{x}{price series}

\item{n}{lag}
}
\value{
vector
}
\description{
Range over recent price history
(x - RcppRoll::roll_meanr(x, n = n)) / RcppRoll::roll_sdr(x, n = n)
}
