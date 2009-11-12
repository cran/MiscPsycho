print.alpha <- function(x, digits = 2, ...){
	cat("\n Call:\n", deparse(x$call), "\n\n", sep = "")
	cat("Cronbach's Coefficient Alpha:", coef(x), "\n")	
	invisible(x)
   }