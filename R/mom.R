

#' Time series momentum
#' (x / dplyr::lag(x,n=n))-1
#' @param x price series
#' @param n lag
#' @return vector
#' @export
get_mom <- function(x, n=1) {
  return ((x / dplyr::lag(x,n=n))-1)
}

#' Price to simple moving average
#' x / RcppRoll:roll_meanr(x, n = n) - 1
#' @param x price series
#' @param n lag
#' @return vector
#' @export
get_sma <- function(x, n=20) {
  return(x / RcppRoll:roll_meanr(x, n = n) - 1)
}

#' SMA ratio
#' RcppRoll::roll_meanr(x, n = n) / RcppRoll::roll_meanr(x, n = k) - 1
#' @param x price series
#' @param n short lag
#' @param k long lag
#' @return vector
#' @export
get_psma <- function(x, n=3, k=20) {
  return(RcppRoll::roll_meanr(x, n = n) / RcppRoll::roll_meanr(x, n = k) - 1)
}

#' Range over recent price history
#' (x - RcppRoll::roll_meanr(x, n = n)) / RcppRoll::roll_sdr(x, n = n)
#' @param x price series
#' @param n lag
#' @return vector
#' @export
get_rrp <- function(x, n=20) {
  return((x - RcppRoll::roll_meanr(x, n = n)) / RcppRoll::roll_sdr(x, n = n))
}


#' Absolute range from {-1,1} over period
#' 2*((x - minr) / (maxr-minr)) - 1
#' @param x price series
#' @param n lag
#' @return vector
#' @export
get_absr <- function(x, n=20){
  minr <- RcppRoll::roll_minr(x, n = n)
  maxr <- RcppRoll::roll_maxr(x, n = n)
  return( 
    2*((x - minr) / (maxr-minr)) - 1
  )
}

#rolling days since high
# get_rolling_dsh <- function(x, n=20)
# {
#   nn <- rep(n, length(x))
# tibbletime::rollify(
#   function(x, nn) {
#     idx_of_high <- which.max(x)
#     days_since_high <- -length(x) + idx_of_high + (nn/2)
#     return(days_since_high)
#   }, 
#   window = n, na_value = NA)
# }