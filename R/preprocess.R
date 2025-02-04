#' Center data, x-mean(x)
#'
#' @param x data
#' @return centered data
#' @export
pp_center <-function(x)
{
  return(x-mean(x))
}

#' Center and scale data, x-mean(x)
#'
#' @param x data
#' @return centered and scaled data
#' @export
pp_center_scale <-function(x)
{
  return((x-mean(x))-sd(x))
}


#' BoxCox transformation, (x^lambda-1)/lambda
#'
#' @param x data
#' @return transformed data
#' @export
pp_boxcox <-function(x, lambda)
{
  shift <- 0
  if (any(x <= 0)) {
    shift <- abs(min(x)) + 1  # Compute the shift amount
    x <- x + shift  # Shift data
  }

  if (lambda == 0)
  {
    t <-log(x)-shift
    return (t)
  }
  t <- (x^lambda-1)/lambda
  t <- t-shift
  return(t)
}

#' BoxCox transformation w/ find optimal lambda, (x^lambda-1)/lambda
#'
#' @param x data
#' @return transformed data
#' @export
pp_boxcox_opt <-function(x)
  {
    log_likelihood <- function(lambda) {
      n <- length(x)
      x_trans <- pp_boxcox(x, lambda)
      sigma2 <- var(x_trans)  # Variance of transformed data
      ll <- -n / 2 * log(sigma2)  # Log-likelihood function
      return(-ll)  # Minimizing the negative log-likelihood
  }

  result <- optim(par = 0, fn = log_likelihood, method = "BFGS")
  return (pp_boxcox(x, result))
}


