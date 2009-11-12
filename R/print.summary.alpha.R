print.summary.alpha <- function(x, ...){
   cat("Cronbach's Coefficient Alpha:", "\n")
   cat("\n")
   printCoefmat(x$coefficients)
   cat("\n")
   cat("Number of test items:", x$numItems, "\n")
   cat("\n")
   cat("Conditional Alpha:", "\n")
   cat("\n")
   cat("The data below show what alpha would be if the item were removed.\n") 
   cat("\n")
   print(x$condAlpha)
   }