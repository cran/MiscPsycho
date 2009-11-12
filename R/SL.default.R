SL.default <-
function(params1, params2, con = 1e-3, ...){
	### These are the mean/sigma linking constants for starting values
	A <- sd(params2$'3pl'$b)/sd(params1$'3pl'$b)
	B <- mean(params2$'3pl'$b) - A*mean(params1$'3pl'$b)
	result <- c(A,B)
	meanSigma <- result

	### Begin NR loop
	change <- rep(1, 2)
	while(any(abs(change) > con)) {
		derivs <- SLderivs(params1, params2, ..., A = A, B = B)
		change <- solve(derivs$hessian) %*% derivs$gradient
		result <- result - change
		A <- result[1]
		B <- result[2]
	}
	#result <- list("A" = result[1], "B" = result[2], "AmeanSig" = meanSigma[1], "BmeanSig" = meanSigma[2])
	result <- list("coefficients" = result, "StartVals" = meanSigma)
	class(result) <- "SL"
	result
	
}

