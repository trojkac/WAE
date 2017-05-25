library('BBmisc')
library('stringr')
library('bbob')

des_optimizer <- function(par, fun, lower, upper, max_eval) {
  n<-length(lower)
  tunable_params = list(
    f=1/sqrt(2),
    lambda=4*n,
    mu=2*n,
    c=4/(n+4),
    H=6+3*sqrt(n),
    epsilon=1e-8/expected_norm_vec(rnorm(n,0,1))
  )
  des_algorithm(par,fun,lower,upper,max_eval,tunable_params)
}

run_benchmark <- function() {
  alg_id <- sprintf("default_des")
  bbo_benchmark(des_optimizer, alg_id, "data",budget=10000, dimensions = c(2,3,5,10))
}