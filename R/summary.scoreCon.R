summary.scoreCon <-
function(object, ...){
	tab <- data.frame(Estimate = coef(object),
			StdError = object$se,
			Raw.Score = object$raw.score)
	res <- list(call = object$call, coefficients = tab)
	class(res) <- "summary.scoreCon"
	res
}

