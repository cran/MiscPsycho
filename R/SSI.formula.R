SSI.formula <-
function(formula, data, id, k, na.action, subset, ...){
	mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "id", "na.action", "subset"), names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1L]] <- as.name("model.frame")
    mf <- eval(mf, parent.frame())
	y <- model.response(mf)
	result <- SSI.default(mf, y, k,  ...)
	result$call <- match.call()
	result$formula <- formula
	result
}

