summary.SSI <-
function(object, ...){
	tab <- data.frame(Zscore = coef(object),
			Percentile = object$percentile,
			ID = object$ID) 
	res <- list(call = object$call, coefficients = tab, 
			N = nrow(object$model.frame), k = object$k)
	class(res) <- "summary.SSI"
	res
}

