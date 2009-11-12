alpha.formula <- function(formula, data, na.action, subset, ...){
	mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "na.action", "subset"), names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1L]] <- as.name("model.frame")
    mf <- eval(mf, parent.frame())
	mt <- attr(mf, "terms")
	dat <- mf
	result <- alpha.default(dat, ...)
	result$call <- match.call()
	result$formula <- formula
	result
}