`fit.Stats` <-
function(dat, params){
	raschO <- function(b,theta){
  		1 - (1 / (1 + exp(outer(theta,b,'-'))))
   		}
	theta <- apply(dat, 1, theta.max, params)
	p <- raschO(as.vector(params), theta)
	z2 <-((dat - p)/ sqrt(p * (1- p)))^2
	v  <- sqrt(p * (1- p))^2
	IF <- colSums(z2*v) / colSums(v) 
	OF  <- colMeans(z2)
	data.frame(IF,OF)	
}

