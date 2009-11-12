print.scoreCon <-
function(x, digits = 2, ...){
   cat("Coefficients:\n")
		print.default(format(coef(x), digits = digits), print.gap=2,
		quote = FALSE)
   invisible(x)
   }

