classical.formula <- function(formula, data, na.action, subset, designSE = FALSE, group,  ...){
	mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "na.action", "subset", "group"), names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1L]] <- as.name("model.frame")
    mf <- eval(mf, parent.frame())
	mt <- attr(mf, "terms")
	N <- ncol(mf)
	if(designSE){
		data <- mf[, 1:(N-1)]
		group <- mf[, N]
	} else {
	data <- mf
	}
	result <- classical.default(data, designSE, group, ...)
	result$call <- match.call()
	result$formula <- formula
	result
}