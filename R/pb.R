pb <-
function(data, ...){
	result <- numeric(ncol(data))
		for(i in 1:ncol(data)) {
		result[i] <- cor(data[,i], rowSums(data[-i]), ...)
	}
	result
}

