print.summary.SSI <-
function(x, ...){
   cat("Call:\n")
   print(x$call)
   cat("\n")
   cat("Norms based on k = ", x$k, "similar students", "\n")
   cat("\n")
}

