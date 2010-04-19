print.summary.mml <- 
function(x, ...){
   cat("Call:\n")
   print(x$call)
   cat("\n")
   cat("\n")
   printCoefmat(x$coefficients)
   }