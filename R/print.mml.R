print.mml <- 
function(x, digits = 2, ...){
   cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
   cat("Coefficients:\n")
		print.default(format(coef(x), digits = digits), print.gap=2,
		quote = FALSE)
   invisible(x)
   }