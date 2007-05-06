`alpha` <-
function(columns){
k <- ncol(columns)
colVars <- apply(columns, 2, var)
total   <- var(apply(columns, 1, sum))
a <- (total - sum(colVars)) / total * (k/(k-1))
a
 }

