jml.default <-
function(dat, con = 1e-3, bias=FALSE, ...){
	result <- jml.fit(dat, con = con, bias = bias)
	result$call <- match.call()
	class(result) <- "jml"
	result
}

