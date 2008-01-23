`scoreCon` <-
function(b_vector){ # b_vector is the vector of item parameters
	q <- length(b_vector)
	max.score <- q-1
	mat <- matrix(numeric(q * q), ncol = q)
	for(i in 1:q){
		mat[,i] <- c(rep(1, i), rep(0, q-i))
		}
	se <- function(x, betas){
  	opt <- function(theta) -sum(dbinom(x, 1, plogis(theta - betas), log = TRUE))
  		out <- optim(log(sum(x)/(length(x)/sum(x))), opt, method = "BFGS", hessian = TRUE)
  		round(1/sqrt(out$hessian), 3)
  		}
	scores <- apply(mat[,1:max.score], 2, theta.max, b_vector)
	S.Errors <- apply(mat[,1:max.score], 2, se, b_vector)
	Raw.Score <- colSums(mat)[1:max.score]
	data.frame(Raw.Score = Raw.Score, Theta = scores, SE = S.Errors)
	}

