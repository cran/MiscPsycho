summary.jml <-
function(object, ...){
	tab <- data.frame(Estimate = coef(object),
			StdError = object$se,
			Infit = object$Infit, 
			Outfit = object$Outfit)	
	res <- list(call = object$call, coefficients = tab, 
			N = nrow(object$model.frame), iter = object$Iterations)
	class(res) <- "summary.jml"
	res
}

