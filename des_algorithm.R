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
des_algorithm <- function(par, fn, lower, upper, max_eval, tunable_parameters = NULL) {
  
  q <- function(x) {
    below_lower <- x < lower
    above_upper <- x > upper
    
    fitness <- fn(x)
    if(any(below_lower)) {
      fitness <- fitness + sum(sapply(x[below_lower]-lower[below_lower], function(y)y*y))
    }
    if(any(above_upper)) {
      fitness <- fitness + sum(sapply(x[above_upper]-upper[above_upper], function(y)y*y))
    }
    return(fitness)
  }
  
  stop_criterion <- function(P, s, epsilon) {
    lambda <- dim(P)[1]
    dim <- dim(P)[2]
    sigma <- sqrt(sapply(colSums(sweep(P,2,s)), function(x)x*x)/(lambda-1))
    return((sum(sigma)/numeric(dim))<epsilon || evals > max_eval)
  }
  
  dim <- length(upper)
  best_par <- NULL
  best_value <- Inf
  evals <- 0
  
  # initialize non-tunable parameters
  t <- 1
  Delta <- 0
  delta <- expected_norm_vec(rnorm(dim, 0, 1))
  
  # initialize tunable parameters (with default values)
  if(is.null(tunable_parameters)) {
    f <- 1/sqrt(2)
    lambda <- 10
    mu <- 5
    c <- 4/(dim+4)
    H <- 6+3*sqrt(dim)
    epsilon <- 1e-8/expected_norm_vec(rnorm(dim,0,1))
  } else {
    f <- tunable_parameters[['f']]
    lambda <- tunable_parameters[['lambda']]
    mu <- tunable_parameters[['mu']]
    c <- tunable_parameters[['c']]
    H <- tunable_parameters[['H']]
    epsilon <- tunable_parameters[['epsilon']]
  }
  # initialize population history
  P.history <- array(0,dim=c(lambda, dim, H))
  
  
  # initialize first random population
  P <- matrix(runif(lambda*dim, min=lower, max=upper),nrow=lambda,ncol=dim)
  
  repeat{
    P_midpoint <- colSums(P)/lambda
    P_midpoint_val <- q(P_midpoint)
    vals_and_agents <- cbind(apply(P,1,q), P)
    evals <- evals + lambda + 1
    P_ordered <- vals_and_agents[order(vals_and_agents[,1]),]
    mu_midpoint <- colSums(head(P_ordered[,-1], mu)) / mu
    
    if(P_ordered[1,1] < best_value) {
      best_par <- P_ordered[1,-1]
      best_value <- P_ordered[1,1]
    }
    Delta <- (1-c) * Delta + c * (mu_midpoint - P_midpoint)
    
    P.history <- array(c(P_ordered[,-1],P.history[,,-H]),dim=c(lambda,dim,H))
    for (i in 1:lambda) {
      # it's 0,H-1 in the paper but we use indexing from 1 to H for historical populations
      # h = 1 is for current population (t), H is for earliest stored (t-H+1) 
      h <- runif(1,1,min(H,t)) 
      
      j <- runif(1,1,mu)
      k <- runif(1,1,mu)
      d <- f*(P.history[j,,h] - P.history[k,,h]) + Delta*delta*rnorm(1)
      P[i,] <- mu_midpoint + d + epsilon*rnorm(dim, 0, 1)
    }
    t <- t + 1
    if(stop_criterion(P,mu_midpoint,epsilon)) {
      break
    }
  }
  list(par=best_par, value=best_value)
}

# https://math.stackexchange.com/questions/827826/average-norm-of-a-n-dimensional-vector-given-by-a-normal-distribution
expected_norm_vec <- function(x){
  N <- length(x)
  numerator = sqrt(2)*gamma((N+1)/2)
  denominator = gamma(N/2)
  return(numerator/denominator)
}
