 print.classical <- function(x, digits = 2, ...){
	cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
	cat("Classical Item Analysis:\n")
	cat("P-Values:", format(x$P.Values[1:min(5, length(x$P.Values))], ...), "\n")
	cat("Point Biserial Correlations:", format(x$Point.Biserials[1:min(5, length(x$Point.Biserials))], ...), "\n")
	cat("\n")
	invisible(x)
   }