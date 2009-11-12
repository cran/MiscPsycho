jml.fit <-
function(dat, con = 1e-3, bias=FALSE, ...){
	rasch <- function(theta,b) 1/ (1 + exp(b-theta)) # Rasch function

	############ Analytic Derivatives ############
	# Vector of first derivatives
	gradient <- function(dat, b_vector, theta){
		-1 * (colSums(dat) - rowSums(apply(as.matrix(theta), 1, rasch, b_vector)))
	}
	# Hessian Matrix (Second derivatives)
	hessian <- function(theta, b_vector){
		-1 * diag(rowSums(apply(as.matrix(theta), 1, rasch, b_vector) * 
		(1-apply(as.matrix(theta), 1, rasch, b_vector))))
	}
	##############################################
	dat <- as.matrix(dat)
	dat <- dat[rowSums(dat)!=0,]         # get rid of all incorrect
	dat <- dat[rowSums(dat)!=ncol(dat),] # get rid of perfect scores	
	b_vector <- numeric(ncol(dat))        # starting values
	change <- rep(1, ncol(dat))
	iter <- 0
	while(any(abs(change) > con)) {
		theta <- apply(dat, 1, theta.max, b_vector)
		change <- solve(hessian(theta, b_vector)) %*% gradient(dat, b_vector, theta)
		b_vector <- b_vector - change # updated items params
		iter <- iter + 1
		}
	b_vector <- b_vector - mean(b_vector) # center on zero
	if(bias){ # correct for JML bias
		b_vector <- b_vector * ((length(b_vector)-1)/length(b_vector))
		} else { b_vector <- b_vector
	}
	SE = 1/sqrt(-1 * diag(hessian(theta, b_vector)))
	fit <- fit.Stats(dat, b_vector)
	list("coefficients" = b_vector, "se"= SE, "Infit" = fit$IF,
		"Outfit" = fit$OF, "Iterations" = iter, "model.frame" = dat)
}

