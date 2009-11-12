cheat.default <-
function(dat, key, wrongChoice, alpha = .01, rfa = c('nr', 'uni', 'bsct'), bonf = c('yes','no'), con = 1e-12, lower = 0, upper = 50, ...){
	result <- cheat.fit(dat, key, wrongChoice, alpha = alpha, rfa = rfa, bonf = bonf,
	con = con, lower = lower, upper = upper)
	result$call <- match.call()
	class(result) <- "cheat"
	result
}

