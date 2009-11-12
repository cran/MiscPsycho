print.summary.classical <- function(x, ...){
   cat("Call:\n")
   print(x$call)
   cat("\n")
   printCoefmat(x$coefficients)
}