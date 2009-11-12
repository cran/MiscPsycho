cheat.fit <-
function(dat, key, wrongChoice, alpha = .01, rfa = c('nr', 'uni', 'bsct'), bonf = c('yes','no'), con = 1e-12, lower = 0, upper = 50, ...){
	bonf <- tolower(bonf)
	bonf <- match.arg(bonf)
	rfa  <- match.arg(rfa)
	rfa  <- tolower(rfa)
	dat <- t(dat)
	correctStuMat <- numeric(ncol(dat))
	for(i in 1:ncol(dat)){
		correctStuMat[i] <- mean(key==dat[,i], na.rm= TRUE)
	}
   
	correctClsMat <- numeric(length(key))
	for(i in 1:length(key)){
		correctClsMat[i] <- mean(key[i]==dat[i,], na.rm= TRUE)
	}
	
	### this is here for cases if all students in a class
	### did not answer the item
	correctClsMat[is.na(correctClsMat)] <- 0

	pCorr <- function(R,c,q){

	numer <- function(R,a,c,q){
		result <- sum((1-(1-R)^a)^(1/a), na.rm= TRUE)-c*q
        result 
	}

    denom <- function(R,a,c,q){
		result <- sum(na.rm= TRUE, -((1 - (1 - R)^a)^(1/a) * (log((1 - (1 - R)^a)) * (1/a^2)) + 
		(1 - (1 - R)^a)^((1/a) - 1) * ((1/a) * ((1 - R)^a * log((1 - R))))))
		result
    }

	aConst <- function(R, c, q, con){
		a <- .5 # starting value for a
		change <- 1
		while(abs(change) > con) {
			r1 <- numer(R,a,c,q)
			r2 <- denom(R,a,c,q)
			change <- r1/r2
			a <- a - change
		}
    a
	}

	bisect <- function(R, c, q, lower, upper, con){
		f <- function(a) sum((1 - (1-R)^a)^(1/a)) - c * q
		if(f(lower) * f(upper) > 0)
			stop("endpoints must have opposite signs")
		while(abs(lower-upper) > con){
			x = .5 * (lower+upper)
			if(f(x) * f(lower) >=0) lower = x
			else upper = x
		}
    .5 * (lower+upper)
   }

	if(rfa == 'nr'){
		if(any(correctClsMat==1)) correctClsMat[correctClsMat==1]<-.9999 else correctClsMat
		if(any(correctClsMat==0)) correctClsMat[correctClsMat==0]<-.0001 else correctClsMat
		a <- aConst(R,c,q, con)
    } else if(rfa == 'uni'){
		f <- function(R, a, c, q) sum((1 - (1-R)^a)^(1/a)) - c * q 
		a <- uniroot(f, c(lower,upper), R = R, c = c, q = q)$root
	} else if(rfa == 'bsct'){
		a <- bisect(R, c, q, lower = lower, upper = upper, con)
	}

	result <- (1-(1-R)^a)^(1/a)
	result
} # end pCorr function
      
	stuProbs <- matrix(0, ncol=ncol(dat), nrow=nrow(dat))
	for(i in 1:ncol(dat)){
		if(correctStuMat[i]==1){
			stuProbs[,i] <- 1
		} else if(correctStuMat[i]==0){
			stuProbs[,i] <- 0
		} else {
		stuProbs[,i] <- pCorr(correctClsMat, correctStuMat[i], q = length(key))
		}
    }
	 
	stuProbs[which(is.na(dat))] <- NA # this is a bit kludgy. But, it places NAs in the right place

	matchProb <- function(StuMat, wrongMat, numItems){
	ind <- combn(c(1:ncol(StuMat)),2) # These are all the possible combinations
	result <- matrix(0, ncol=ncol(ind), nrow=numItems)
	for(j in 1:ncol(ind)){
		for(i in 1:numItems){
			if(is.na(StuMat[i,ind[1,j]]) | is.na(StuMat[i,ind[2,j]]) ) {
				result[i,j] <- NA 
				} else { result[i,j] <- StuMat[i,ind[1,j]] * StuMat[i,ind[2,j]] + 
				(1-StuMat[i,ind[1,j]]) * (1-StuMat[i,ind[2,j]]) * sum(wrongChoice[[i]]^2)
			}
		}
	}
	result <- data.frame(result)
	names(result) <- paste('S',paste(ind[1,], ind[2,], sep=''), sep='')
	result
	}
 
	aa <- matchProb(stuProbs, wrongChoice, numItems = nrow(dat))

	matchTotal <- function(dat){
	ind <- combn(c(1:ncol(dat)),2) # These are all the possible combinations
	Match <- numeric(ncol(ind))
	for(j in 1:ncol(ind)){
		Match[j] <- sum(dat[, ind[1,j]] == dat[, ind[2,j]], na.rm=TRUE) ### added this just now
    }
	Match <- data.frame(Match)
	row.names(Match) <- paste('S',paste(ind[1,], ind[2,], sep=':'), sep='')
	Match
	}

	exactMatch <- matchTotal(dat)
	means <- colSums(aa, na.rm=TRUE)
	vars <- colSums(aa*(1-aa), na.rm= TRUE)
	cheaters <- (exactMatch - .5 - means)/sqrt(vars)
	totalCompare <- nrow(cheaters)
	if(bonf == 'yes'){
		alpha <- alpha/nrow(cheaters) 
		Zcrit <- qnorm(alpha, mean = 0, sd = 1, lower.tail = FALSE)
		} else {
        Zcrit <- qnorm(alpha, mean = 0, sd = 1, lower.tail = FALSE)
    }

	cheaters <- cbind(data.frame(cheaters), exactMatch)
	names(cheaters)[1] <- 'Zobs'
	names(cheaters)[2] <- 'Nexact'
	cheaters$Zcrit <- Zcrit
	cheaters$Mean <- means
	cheaters$Var <- vars
	cheaters <- cheaters[cheaters$Zobs > Zcrit, ]
	result <-  list("pairs" = c(row.names(cheaters)), "Ncheat" = nrow(cheaters), 
	"TotalCompare" = totalCompare, "alpha" = alpha, 
	"ExactMatch" = cheaters$Nexact, "Zobs" = cheaters$Zobs, "Zcrit" = Zcrit, 
	"Mean" = cheaters$Mean, "Variance" = cheaters$Var, "Probs" = stuProbs)
	result 
}

