alpha.default <- function(dat, ...){
	result <- alpha.fit(dat, ...)
	result$call <- match.call()
	class(result) <- "alpha"
	result
	}