classical.default <- function(data, designSE = FALSE, group, na.rm = TRUE, use = 'everything', ...){
	
	if(designSE) N <- ncol(data)-1
			
	pb <- function(data){
		pbResult <- numeric(ncol(data))
		for(i in 1:ncol(data)) {
			pbResult[i] <- cor(data[,i], rowSums(data[,-i]), use = use)
		}
	pbResult
	}	
	
	se <- function(data){
		p <- colMeans(data, na.rm = na.rm)
		N <- apply(data, 2, function(x) length(x[!is.na(x)]))
		q <- 1 - p
		sqrt((p * q)/N)
	}
	
	design <- function(data, score, group, na.rm=na.rm){
		xx <- with(data, tapply(score, group, sum, na.rm = na.rm))
		ss <- mean(xx, na.rm = na.rm)
		k <- length(xx)
		totvarY <- sum((xx-ss)^2) * k/(k-1)
		totalY <- sum(xx)
		xx <- with(data, tapply(score, group, function(x) length(x[!is.na(x)]) ))
		ss <- mean(xx, na.rm = na.rm)
		k <- length(xx)
		totvarN <- sum((xx-ss)^2) * k/(k-1)
		x1 <- with(data, tapply(score, group, sum, na.rm = na.rm))
		x2 <- with(data, tapply(score, group, function(x) length(x[!is.na(x)]) ))
		x1bar <- mean(x1)
		x2bar <- mean(x2)
		cc <- sum((x1-x1bar) * (x2-x2bar)) * k/(k-1)
		N <- length(score[!is.na(score)])
		se <- sqrt((N^2*totvarY - 2*cc*N*totalY + totvarN * totalY^2)/N^4)
		se
	}
	
	if(designSE){
		pVals <- colMeans(data[, 1:N], na.rm = na.rm) 
		} else { 
		pVals <- colMeans(data, na.rm = na.rm) 
	}
	
	if(designSE){
		errors <- numeric(N)
		for(i in 1:N){
			errors[i] <- design(data, data[,i], data$'(group)', na.rm=na.rm)
		}
	} else {
		errors <- se(data)
	}
	
	if(designSE){
		pBis <- pb(data[, 1:N])
		} else {
		pBis <- pb(data)
	}
	
	result <- list("P.Values" = pVals, "Std.Error" = errors, "Point.Biserials" = pBis)
	result$call <- match.call()
	class(result) <- "classical"
	result
}



