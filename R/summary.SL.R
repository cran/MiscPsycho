summary.SL <-
function(object, ...){
	tab <- data.frame(A.Parameter = coef(object)[1],
			B.Parameter = coef(object)[2])	
	res <- list(coefficients = tab, MS.A = object$StartVals[1], 
			MS.B = object$StartVals[2])
	class(res) <- "summary.SL"
	res
}

