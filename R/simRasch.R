`simRasch` <-
function(Nt, Nb, mu=0, sigma=1) {	
	theta <- rnorm(Nt, mu, sigma)
	b <- runif(Nb,-3,3)
	results <- matrix(numeric(length(theta) * length(b)), ncol = length(b))
	results <- as.data.frame(results)
	results <- cbind(results, theta)
	for(i in seq(along=b)) {
		results[,i] <- 1/ (1 + exp(b[i]-results$theta))
	}
	response.matrix <- subset(results, select=(-theta))
	uniform <- matrix(c(runif(Nt*length(b))), ncol=length(b))
	for(i in 1:length(b)){
		response.matrix[,i] <- ifelse(response.matrix[,i] < uniform[,i], 0, 1) 
		}
	list(data = response.matrix, generating.values = b, theta = theta)
}

