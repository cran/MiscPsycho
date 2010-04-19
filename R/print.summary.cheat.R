print.summary.cheat <-
function(x, ...){
   cat("\n Number of Possible Cheating Pairs:", length(x$pairs), "\n")
   cat("\n Possible Cheating Pairs:", x$pairs, "\n")
   cat("\n Number of Exact Matches:", x$ExactMatch, "\n")
   cat("\n Observed Z Values:", round(x$Zobs, 2), "\n")
   cat("\n Critical Z:", round(x$Zcrit, 2), "\n")
   cat("\n Expected Number of Matches:", x$Mean, "\n")
   cat("\n Variance:", x$Var, "\n")
}

