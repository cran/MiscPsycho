`plaus.val` <-
function(x, params, PV = 5, ind.dichot, M = 3, max = 5000, ...){
   L <- 0  
   i <- 1
   result  <- numeric(max)
   	while(L < PV){
	   theta   <- rnorm(1, 0, 1)
	   U       <- runif(1, 0, 1)
	   p.theta <- posterior(x, theta = theta, params = params, ind.dichot = ind.dichot, ...)
	   ratio   <- p.theta/ (M * dnorm(theta))
	   if(U <= ratio) result[i] <- theta
	   L <- length(result[result!=0])
	   i <- i + 1
	   }
   values <- result[result!=0]
   values
 }

