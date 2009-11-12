SSI.default <-
function(mf, y, k, ...){
	result <- SSI.fit(mf, y, k)
	result$call <- match.call()
	class(result) <- "SSI"
	result
}

