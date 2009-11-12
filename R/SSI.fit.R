SSI.fit <-
function(mf, y, k, ...){
	L <- nrow(mf)
	if(k > L) {
		stop("k cannot be larger than the total number of possible comparisons. 
		The total number of possible comparisons is ", L)
	}
	if(!is.numeric(y)) {
		stop("The outcome variable must be numeric") 
	}
	z.score <- numeric(L)
	varnames <- colnames(mf)
	numCompare <- ncol(mf) - 1
	for (i in 1:L){
		comp <- matrix(0, nrow(mf), length(mf)-1)
		for(j in 1:numCompare){
			comp[, j]  <- (mf[i, varnames[j]] - mf[, varnames[j]])^2
		}
	result <- data.frame(comp, nn = sqrt(rowSums(comp)))
	result <- result[order(result$nn) , ]
	x <- attr(result[1:k, ], 'row.names') # These are the row names of the k nearest neighbors
	x <- c(i,x) # make sure student is on top in set of interest	
	tmp <- data.frame(id = mf[x, ncol(mf)], y = y[x]) # now subset and get scores of the nearest neighbors
	tmp <- tmp[!duplicated(tmp[, 1]),] # make sure student is not in data twice
	tmp$scaled <- scale(tmp$y)		
 	z.score[i] <- tmp[1, 'scaled'] # it is '1' because I always put the student on top
	cat('Iteration', i, '\t', round(i/length(z.score),3) * 100, 'percent complete', '\n') # monitor progress
	}
	mf$z.score <- z.score
	mf$percentile <- round(pnorm(z.score),3)
	list("coefficients" = z.score, "percentile" = mf$percentile, 
		"k" = k, "ID" = mf$'(id)', "model.frame" = mf)
}

