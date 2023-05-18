#'The log-sum-exp function
#'
#'@param x Is a vector of numeric values
#'
#'@return The log sum of exponentials
#'
#'@examples
#'##let x be a numeric vector
#'##x <- c(1:2000)
#'##log_summed_exps(x) = 2000.459
#'
#'@export

log_summed_exps <- function(x){
  if(!is.vector(x)){
    warning("Input is not in a vector format")
  }
  x <- sort(x) # max(x)
  x1 <- x[-length(x)]
  res <- x[length(x)] + log(1 + sum(exp(x1-x[length(x)])))
  return(res)
}
