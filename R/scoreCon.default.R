scoreCon.default <-
function(b_vector){
	result <- scoreCon.fit(b_vector)
	result$call <- match.call()
	class(result) <- "scoreCon"
	result
}

