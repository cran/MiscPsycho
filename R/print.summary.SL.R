print.summary.SL <-
function(x, ...){
   cat("Final Stocking Lord Parameter Estimates:", "\n")
   cat("\n")
   printCoefmat(x$coefficients)
   cat("\n")
   cat("Starting Values Taken From Mean/Sigma:", "\n")
   cat(x$MS.A, x$MS.B, "\n")
   }

