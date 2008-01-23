`piPlot` <-
function(dat, params){
	dat <- as.matrix(dat)
	dat <- dat[rowSums(dat)!=0,]         # get rid of all incorrect
	dat <- dat[rowSums(dat)!=ncol(dat),] # get rid of all perfect
  	theta <- apply(dat, 1, theta.max, params)
	plot(density(theta), ylim=c(0,1), lty=1, main = "Person-Item Plot", xlab = "Theta")
	lines(density(params), lty=2)
}

