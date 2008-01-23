`choose.M` <-
function(x, theta, params, ind.dichot, ...){
   posterior(x, theta, params, ind.dichot, ...)/dnorm(theta, ...)
}

