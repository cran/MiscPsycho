SLderivs <-
function(params1, params2, A = 1, B = 0, control=list()){
	a1 <- params1$'3pl'$a
	b1 <- params1$'3pl'$b
	c1 <- params1$'3pl'$c
	a2 <- params2$'3pl'$a
	b2 <- params2$'3pl'$b
	c2 <- params2$'3pl'$c
	con <- list(D = 1.7, mu = 0, sigma = 1, Q = 30)
	con[names(control)] <- control
	gq <- gauss.quad.prob(con$Q, dist = "normal", mu = con$mu, sigma = con$sigma)
	nodes <- gq$nodes
	whts <- gq$weights
	L <- length(a1)    # Number of test items
	M <- length(nodes) # Number of quadrature points

	pl3T <- function(theta, a,b,c, A = 1, B = 0){
		c + (1 - c) / (1 + exp(-con$D * a/A * (theta - (A*b + B))))
	}
            
	p1 <- p2 <- p3 <- p4 <- p5 <- p6 <- p7 <- p8 <- p9 <- p10 <- p11 <- p12 <- p13 <- p14 <- 
	matrix(c(numeric(L*M)), ncol=M, nrow=L)
	for(i in 1:L){
		for(k in 1:M){
         
	   ### This is the loop for score 1
		p1[i,k] <- pl3T(nodes[k], a2[i], b2[i], c2[i]) 
        p2[i,k] <- pl3T(nodes[k], a1[i], b1[i], c1[i], A = A, B = B)
		p3[i,k] <- (1-c1[i]) * exp(-con$D*a1[i]/A * (-B-A*b1[i]+nodes[k])) * 
		(con$D*a1[i]/A*b1[i] + con$D*a1[i]/A^2 *(-B-A*b1[i]+nodes[k]))/
		(1 + exp(-(con$D*a1[i]/A*(-B-A*b1[i]+nodes[k]))))^2

	   ### This part does the loop for score 2
        p4[i,k] <- (con$D *a1[i] * (1-c1[i])) * exp(-con$D*a1[i]/A * (-B-A*b1[i]+nodes[k]))/
		(A * (1 + exp(-(con$D*a1[i]/A*(-B-A*b1[i]+nodes[k]))))^2)

         ### This part does the loop for score 3
        p5[i,k] <- (1-c1[i]) * exp(-con$D*a1[i]/A * (-B-A*b1[i]+nodes[k])) *
		(-(2*con$D*a1[i]/A^2*b1[i]) - 2*con$D*a1[i]/A^3 *(-B-A*b1[i]+nodes[k]))/
		(1 + exp(-(con$D*a1[i]/A*(-B-A*b1[i]+nodes[k]))))^2
    
        p6[i,k] <- (1-c1[i]) * exp(-con$D*a1[i]/A * (-B-A*b1[i]+nodes[k])) * 
		((-(con$D*a1[i]/A*b1[i]) - con$D*a1[i]/A^2 *(-B-A*b1[i]+nodes[k])))^2/
		(1 + exp(-(con$D*a1[i]/A*(-B-A*b1[i]+nodes[k]))))^2
         
        p7[i,k] <- 2*(1-c1[i]) * exp(-2*con$D*a1[i]/A * (-B-A*b1[i]+nodes[k])) * 
        (((con$D*a1[i]/A*b1[i]) + con$D*a1[i]/A^2 *(-B-A*b1[i]+nodes[k])))^2/
        (1 + exp(-(con$D*a1[i]/A*(-B-A*b1[i]+nodes[k]))))^3

		### This part does the loop for score 4
        p9[i,k] <- (2*con$D^2 *a1[i]^2 * (1-c1[i])) * exp(-2*con$D*a1[i]/A * (-B-A*b1[i]+nodes[k]))/
		(A^2 * (1 + exp(-(con$D*a1[i]/A*(-B-A*b1[i]+nodes[k]))))^3)

		p10[i,k] <- (con$D^2 *a1[i]^2 * (1-c1[i])) * exp(-con$D*a1[i]/A * (-B-A*b1[i]+nodes[k]))/
		(A^2 * (1 + exp(-(con$D*a1[i]/A*(-B-A*b1[i]+nodes[k]))))^2)

        p11[i,k] <- (2 * con$D^2 * a2[i]^2 * (1-c2[i]) * exp(-(2*con$D*a2[i]/A*(-B-A*b2[i]+nodes[k]))))/
        (A^2 * (1 + exp(-(con$D*a2[i]/A*(-B-A*b2[i]+nodes[k]))))^3)

		### This part does the loop for score 5
		p12[i,k] <- (con$D *a1[i] * (1-c1[i])) * exp(-con$D*a1[i]/A * (-B-A*b1[i]+nodes[k]))/
		(A^2 * (1 + exp(-(con$D*a1[i]/A*(-B-A*b1[i]+nodes[k]))))^2)

		p13[i,k] <-  (2*con$D *a1[i] * (1-c1[i])) * exp(-2*con$D*a1[i]/A * (-B-A*b1[i]+nodes[k])) * 
        ((con$D*a1[i]/A*b1[i]) + con$D*a1[i]/A^2 *(-B-A*b1[i]+nodes[k]))/         
        (A*(1 + exp(-(con$D*a1[i]/A*(-B-A*b1[i]+nodes[k]))))^3)

		p14[i,k] <- (con$D *a1[i] * (1-c1[i])) * exp(-con$D*a1[i]/A * (-B-A*b1[i]+nodes[k])) * 
        ((con$D*a1[i]/A*b1[i]) + con$D*a1[i]/A^2 *(-B-A*b1[i]+nodes[k]))/         
        (A*(1 + exp(-(con$D*a1[i]/A*(-B-A*b1[i]+nodes[k]))))^2)
		}
	}
	score1.result <- sum(2 * (colSums(p1) - colSums(p2)) * colSums(p3) * whts)
	score2.result <- sum(2 * colSums(p4) * (colSums(p1) - colSums(p2)) * whts)	 
	score3.result <- sum((2*colSums(p3)^2) + (2 * (colSums(p1) - colSums(p2))) * ((colSums(p5) + colSums(p6)) - 
	colSums(p7)) * whts)
	score4.result <- sum((2 * colSums(p4)^2) + 2 * (colSums(p10) - colSums(p9)) * (colSums(p1) - colSums(p2)) *
	whts)
	score5.result <- sum((2 * colSums(p4)) * colSums(p3) + (2 * (colSums(p1) - colSums(p2))) *(colSums(p14) - (
	colSums(p12) + colSums(p13))) * whts)
	list(gradient = c(score1.result, score2.result), 
		hessian = matrix(c(score3.result, score5.result,score5.result,score4.result), ncol=2))
}

