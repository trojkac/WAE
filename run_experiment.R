library('BBmisc')
library('stringr')
library('bbob')
library('DEoptim')
des_optimizer <- function(par, fun, lower, upper, max_eval) {
  population.number <- length(lower)*10 # default population number of DEoptim
  ctrl <- DEoptim.control(itermax = max_eval/population.number,trace=FALSE)
  DEoptim(fun,lower,upper,ctrl)
}

run_benchmark <- function() {
  alg_id <- sprintf("DEoptim")
  dir <- sprintf("data/%s_%s",alg_id,format(Sys.time(), "%d%h%H%M"))
  bbo_benchmark(des_optimizer, alg_id, dir,budget=10000, dimensions = c(2,3,5,10,20,40))
}
