summary.mml <- 
function(object, ...){
	tab <- data.frame(Estimate = coef(object),
			StdError = object$Std.Error)	
	row.names(tab) <- c('mu', 'sigma')
	res <- list(call = object$call, coefficients = tab)
	class(res) <- "summary.mml"
	res
}