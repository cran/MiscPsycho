classical.default <- function(data, designSE = FALSE, group, ...){
	pb <- function(data, ...){
		pbResult <- numeric(ncol(data))
		for(i in 1:ncol(data)) {
			pbResult[i] <- cor(data[,i], rowSums(data[,-i]), ...)
		}
	pbResult
	}	
	
	se <- function(data){
		p <- colMeans(data)
		N <- apply(data, 2, length)
		q <- 1 - p
		sqrt((p * q)/N)
	}
	
	design <- function(data, score, group){
		xx <- with(data, tapply(score, group, sum, na.rm=TRUE))
		ss <- mean(xx, na.rm=T)
		k <- length(xx)
		totvarY <- sum((xx-ss)^2) * k/(k-1)
		totalY <- sum(xx)
		xx <- with(data, tapply(score, group, length))
		ss <- mean(xx, na.rm=T)
		k <- length(xx)
		totvarN <- sum((xx-ss)^2) * k/(k-1)
		x1 <- with(data, tapply(score, group, sum, na.rm=TRUE))
		x2 <- with(data, tapply(score, group, length))
		x1bar <- mean(x1)
		x2bar <- mean(x2)
		cc <- sum((x1-x1bar) * (x2-x2bar)) * k/(k-1)
		N <- nrow(data)
		se <- sqrt((N^2*totvarY - 2*cc*N*totalY + totvarN * totalY^2)/N^4)
		se
	}

	pVals <- colMeans(data) 
	if(designSE){
		errors <- numeric(ncol(data))
		for(i in 1:ncol(data)){
			errors[i] <- design(data, data[,i], group)
		}
	} else {
		errors <- se(data)
	}
	pBis <- pb(data, ...)       	
	result <- list("P.Values" = pVals, "Std.Error" = errors, "Point.Biserials" = pBis)
	result$call <- match.call()
	class(result) <- "classical"
	result
}