##' Implementation of Differential Evolution Strategy (D. Jagodzinski, J. Arabas)
##' 
##'
##' @param par Initial parameter combination .
##' @param fn Function to minimize.
##' @param lower Lower bounds of search space.
##' @param upper Upper bounds of search space.
##' @param max_eval Maximium number of function evaluations to perform.
##'
##' @return Best solution found.
##' 
##' @export
##' 
library('BBmisc')
library('stringr')
library('bbob')

# https://math.stackexchange.com/questions/827826/average-norm-of-a-n-dimensional-vector-given-by-a-normal-distribution
expected_norm_vec <- function(x){
  N <- length(x)
  numerator = sqrt(2)*gamma((N+1)/2)
  denominator = gamma(N/2)
  return(numerator/denominator)
}

des_algorithm <- function(par, fn, lower, upper, max_eval) {
  
  dim <- length(upper)
  best_par <- NULL
  best_value <- Inf
  
  # initialize non-tunable parameters
  t <- 1
  Delta <- 0
  delta <- expected_norm_vec(rnorm(dim, 0, diag(dim)))
  
  # initialize tunable parameters (with default values)
  f <- 1/sqrt(2)
  lambda <- 10
  mu <- 5
  c <- 4/(dim+4)
  H <- 6+3*sqrt(dim)
  
  # initialize first random population
  P <- matrix(runif(lambda*dim, min=lower, max=upper),nrow=lambda,ncol=dim)
  
  # single test iteration
  P_midpoint <- colSums(P)/lambda
  P_midpoint_val <- fn(P_midpoint)
  vals_and_agents <- cbind(apply(P,1,fn), P)
  P_ordered <- vals_and_agents[order(-vals_and_agents[,1]),]
  mu_midpoint <- colSums(head(P_ordered[,-1], mu)) / mu
  Delta <- rbind(Delta, (1-c) * tail(Delta,1) + c * (mu_midpoint - P_midpoint))
  
  for (i in 1:lambda) {
    
  }
  t <- t + 1
}

bbo_benchmark(des_algorithm, "l-bfgs-b", "optim_l-bfgs-b",budget=1)


random_alg <- function(par, fn, lower, upper, max_eval) {
  dim <- length(upper)
  best_par <- NULL
  best_value <- Inf
  
  for (i in 1:max_eval) {
    par <- runif(dim, min=lower, max=upper)
    value <- fn(par)
    if (value < best_value) {
      best_par <- par
      best_value <- value
    }      
  }
  list(par=best_par, value=best_value)
}