print.SL <-
function(x, digits = 2, ...){
	cat("Stocking Lord Coefficients:\n")
	cat("A parameter:", coef(x)[1], "\n")
	cat("B parameter:", coef(x)[2], "\n")
	cat("\n")
	cat("Starting values from Mean/Sigma:\n")
	cat("A parameter:", x$StartVals[1], "\n")
	cat("B parameter:", x$StartVals[2], "\n")
	invisible(x)
   }

