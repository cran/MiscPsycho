summary.alpha <- function(object, ...){
	tab <- data.frame(alpha = coef(object))	
	res <- list(coefficients = tab, numItems = object$N, condAlpha = object$condAlpha)
	class(res) <- "summary.alpha"
	res
}