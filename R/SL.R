`SL` <-
function(params1,params2, con = .001, ...){
   ### These are the mean/sigma linking constants for starting values
   A <- sd(params2$'3pl'$b)/sd(params1$'3pl'$b)
   B <- mean(params2$'3pl'$b) - A*mean(params1$'3pl'$b)
   result <- c(A,B)

   ### Begin NR loop
   change <- rep(1, 2)
   while(any(abs(change) > con)) {
      derivs <- SLderivs(params1, params2, ..., A = A, B = B)
      change <- solve(derivs$hessian) %*% derivs$gradient
      result <- result - change
      A <- result[1]
      B <- result[2]
   }
   list(A = result[1], B = result[2])
}

