`theta.max` <-
function(x, betas){
  opt <- function(theta) -sum(dbinom(x, 1, plogis(theta - betas), log = TRUE))
  out <- optim(log(sum(x)/(length(x)/sum(x))), opt, method = "BFGS", hessian = TRUE)
  out$par
  }

