summary.classical <- function(object, ...){
	tab <- data.frame(P.Values = object$P.Values,
			Std.Errors = object$Std.Error,
			PointBiserials = object$Point.Biserials)
	res <- list(call = object$call, coefficients = tab)
	class(res) <- "summary.classical"
	res
}