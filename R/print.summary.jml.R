print.summary.jml <-
function(x, ...){
   cat("Call:\n")
   print(x$call)
   cat("\n")
   cat("Number of iterations to completion:", x$iter, "\n")
   cat("Number of individuals used in estimation:", x$N, "\n")
   cat("\n")
   printCoefmat(x$coefficients)
   }

