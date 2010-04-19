mml.default <- 
function(data, Q = 20, params, ...){
	result <- mml.fit(data, Q, params, ...)
	result$call <- match.call()
	class(result) <- "mml"
	result
}