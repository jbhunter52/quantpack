#' Black-Scholes Function returning euro option prices
#'
#' @param S, Spot security price
#' @param K, Strike price
#' @param T, Time to maturity, fractional years
#' @param r, Risk-free interest rate
#' @param sig Sigma, option yearly volatility
#' @param type Option type "C" or "P"
#' @return option value scalar
#' @export
bs <-function(S, K, T, r, sig, type="C")
{
  d1 <- (log(S/K) + (r + sig^2/2)*T) / (sig*sqrt(T))
  d2 <- d1 - sig*sqrt(T)
  if(type=="C"){
    value <- S*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
  }
  if(type=="P"){
    value <- K*exp(-r*T)*pnorm(-d2) - S*pnorm(-d1)
  }
  return(value)
}

#' Numerical computation of options implied volatility using BS
#'
#' @param S, Spot security price
#' @param K, Strike price
#' @param T, Time to maturity, fractional years
#' @param r, Risk-free interest rate
#' @param market Market price
#' @param type Option type "C" or "P"
#' @return Option implied yearly volatility
#' @export
bs_iv <-function(S, K, T, r, market, type)
{
  sig <- 0.20
  sig.up <- 1000
  sig.down <- 0.00001
  count <- 0
  err <- BS(S, K, T, r, sig, type) - market

  ## repeat until error is sufficiently small or counter hits 1000
  while(abs(err) > 0.0001 && count<10000){
    if(err < 0){
      sig.down <- sig
      sig <- (sig.up + sig)/2
    }else{
      sig.up <- sig
      sig <- (sig.down + sig)/2
    }
    err <- BS(S, K, T, r, sig, type) - market
    #print(paste(sig, err))
    count <- count + 1
  }

  ## return NA if counter hit 1000
  if(count==1000){
    return(NA)
  }else{
    return(sig)
  }
}
