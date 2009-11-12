print.SSI <-
function(x, digits = 2, ...){
	cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
	cat("Coefficients:\n")
	cat("Z.scores:", format(coef(x)[1:min(5, length(coef(x)))], ...), "\n")
	cat("Percentiles:", format(x$percentile[1:min(5, length(coef(x)))], ...))
	cat("\n")
	invisible(x)
   }

