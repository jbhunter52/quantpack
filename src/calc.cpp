#include <Rcpp.h>
using namespace Rcpp;
using std::sqrt;
using std::log;


// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//


// [[Rcpp::export]]
NumericVector testchar(StringVector type)
{
  int n = type.size();
  NumericVector values(n);
  for (int i = 0; i < n; i++)
  {
    if(strcmp(type(i), "C") == 0)
    {
      values[i] = 1.0;
    }
    else
    {
      values[i] = 0.0;
    }
  }
  return values;
}


//' Black-Scholes Function returning euro option prices
//' Supports vectors in Rcpp for loop
//'
//' @param S, Spot security price
//' @param K, Strike price
//' @param T, Time to maturity, fractional years
//' @param r, Risk-free interest rate
//' @param sig Sigma, option yearly volatility
//' @param type Option type "C" or "P"
//' @return option value scalar
//' @export
// [[Rcpp::export]]
NumericVector calc_bs_cpp_loop(NumericVector S, NumericVector K, NumericVector T, NumericVector R, NumericVector sig, StringVector type)
{
  int n = S.size();
  NumericVector values(n);

  for (int i = 0; i < n; i++)
  {
    double s = S[i];
    double k = K[i];
    double t = T[i];
    double _r = R[i];
    double _sig = sig[i];

    double d1 = (log(s/k) + (_r + (_sig*_sig)/2)*t) / (_sig*sqrt(t));
    double d2 = d1 - _sig*sqrt(t);
    double value;
    if(strcmp(type(i), "C") == 0)
    {
      double pnorm_d1 = pnorm(NumericVector(1,d1),0.0,1.0)[0];
      double pnorm_d2 = pnorm(NumericVector(1,d2),0.0,1.0)[0];
      value = s*pnorm_d1 - k*exp(-_r*t)*pnorm_d2;
    }
    else if(strcmp(type(i), "P") == 0)
    {
      double pnorm_md1 = pnorm(NumericVector(1,d1),0.0,1.0)[0];
      double pnorm_md2 = pnorm(NumericVector(1,d2),0.0,1.0)[0];
      value = k*exp(-_r*t)*pnorm_md2 - s*pnorm_md1;
    }
    else
    {
      value = NumericVector::get_na();
    }
    values[i] = value;

  }
    return values;
}

double bs_cpp_single(double S, double K, double T, double R, double sig, std::string op_type)
{

    double d1 = (log(S/K) + (R + (sig*sig)/2)*T) / (sig*sqrt(T));
    double d2 = d1 - sig*sqrt(T);
    double value;

    if(op_type.compare("C") == 0)
    {
      value = S*pnorm(NumericVector(1,d1),0.0,1.0)[0] - K*exp(-R*T)*pnorm(NumericVector(1,d2),0.0,1.0)[0];
    }
    else
    {
      value =  - S*pnorm(NumericVector(1,-d1),0.0,1.0)[0] + K*exp(-R*T)*pnorm(NumericVector(1,-d2),0.0,1.0)[0];
      //Rcout << value << ",";
    }
    return value;
}

//' Black-Scholes Function returning euro option prices
//' Supports vectors in Rcpp with vectorized calculation
//'
//' @param S, Spot security price
//' @param K, Strike price
//' @param T, Time to maturity, fractional years
//' @param r, Risk-free interest rate
//' @param sig Sigma, option yearly volatility
//' @param type Option type "C" or "P"
//' @return option value scalar
//' @export
// [[Rcpp::export]]
NumericVector calc_bs_cpp(NumericVector S, NumericVector K, NumericVector T, NumericVector R, NumericVector sig, StringVector type)
{
  int n = S.size();
  NumericVector values(n);


    NumericVector d1 = (log(S/K) + (R + (sig*sig)/2)*T) / (sig*sqrt(T));
    NumericVector d2 = d1 - sig*sqrt(T);

    NumericVector isCall(n);
    for (int i = 0; i < n; i++)
    {
      if (strcmp(type(i), "C"))
        isCall[i] = 0;
      else
        isCall[i] = 1;
    }

    NumericVector pnorm_d1 = pnorm(d1,0.0,1.0);
    NumericVector pnorm_d2 = pnorm(d2,0.0,1.0);

    NumericVector LHS = ifelse(isCall == 1,
                                     S*pnorm_d1,
                                     -S*pnorm(-d1,0.0,1.0));

    NumericVector RHS = ifelse(isCall == 1,
                               - K*exp(-R*T)*pnorm(d2,0.0,1.0),
                               K*exp(-R*T)*pnorm(-d2,0.0,1.0));

    NumericVector value = LHS+RHS;
    return value;
}

//' Numerical computation of options implied volatility using BS
//' Supports vectors
//'
//' @param S, Spot security price
//' @param K, Strike price
//' @param T, Time to maturity, fractional years
//' @param r, Risk-free interest rate
//' @param market Market price
//' @param type Option type "C" or "P"
//' @return Option implied yearly volatility
//' @export
// [[Rcpp::export]]
NumericVector calc_bs_iv_cpp(NumericVector S, NumericVector K, NumericVector T, NumericVector r, NumericVector market, StringVector op_type)
{
  int n = S.size();
  NumericVector sig_all(n);
  NumericVector counts(n);
  NumericVector errs(n);
  for (int i = 0; i < n; i++)
  {
    double sig = 0.2;
    double sig_up = 1000.0;
    double sig_down = 0.00001;
    int count = 0;
    double err = bs_cpp_single(S[i], K[i], T[i], r[i], sig, Rcpp::as<std::string>(op_type[i])) - market[i];
    // repeat until error is sufficiently small or counter hits 1000
    while ((abs(err) > 0.0001) & (count<10000))
    {
      if (err < 0)
      {
        sig_down = sig;
        sig = (sig_up + sig)/2;
      }
      else
      {
        sig_up = sig;
        sig = (sig_down + sig)/2;
      }

      err = bs_cpp_single(S[i], K[i], T[i], r[i], sig, Rcpp::as<std::string>(op_type[i])) - market[i];
      count = count + 1;
    }
    sig_all[i] = sig;
    counts[i] = (double)count;
    errs[i] = err;
  }
  //return errs;
  return sig_all;
}



// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R

bs <-function(S, K, T, r, sig, type)
{
  d1 <- (log(S/K) + (r + sig^2/2)*T) / (sig*sqrt(T))
  d2 <- d1 - sig*sqrt(T)

  isCall <- as.numeric(type == 'C')

  pnorm_d1 <- pnorm(d1)
  pnorm_d2 <- pnorm(d2)

  LHS <- ifelse(isCall == 1,
                             S*pnorm(d1),
                             -S*pnorm(-d1));

  RHS <- ifelse(isCall == 1,
                             -K*exp(-r*T)*pnorm(d2),
                             K*exp(-r*T)*pnorm(-d2));
  value <- LHS+RHS
  #if (type == 'P') cat(paste0(value,','))
  return(value)
}

## Function to find BS Implied Vol using Bisection Method
implied.vol <-function(S, K, T, r, market, type)
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
    err <- bs(S, K, T, r, sig, type) - market
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


bs(116,117,.159,0.0122,0.35,'C')
calc_bs_cpp(116,117,.159,0.0122,0.35,'C')
calc_bs_iv_cpp(116,117,.159,0.0122,6.095508,'C')


bs(116,110,.159,0.0122,0.35,'P')
calc_bs_cpp(116,110,.159,0.0122,0.35,'P')
calc_bs_iv_cpp(116,110,.159,0.0122,3.655307,'P')


*/
