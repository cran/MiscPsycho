alpha.fit <- function(dat, ...){
	alpha.fun <- function(dat){
		k <- ncol(dat)
		colVars <- apply(dat, 2, var)
		total   <- var(apply(dat, 1, sum))
		result <- (total - sum(colVars)) / total * (k/(k-1))
		result
	}
	result <- alpha.fun(dat)
	condAlpha <- numeric(ncol(dat))
	n  <- ncol(dat)
	for(i in 1:n){
		condAlpha[i] <- alpha.fun(dat[,-i])
	}
	condAlpha <- data.frame(Item = 1:n, alpha = condAlpha)	
	list("coefficients" = result, "N" = ncol(dat), "condAlpha" = condAlpha)
}
