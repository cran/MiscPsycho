wrongProb.default <-
function(dat, key, ...){
	dat <- t(dat)
	wrongChoice <- vector("list", nrow(dat))
	for(i in 1:nrow(dat)){
		mm <- as.numeric(dat[i, which(dat[i,] != key[i])])
		wrongChoice[[i]] <- table(mm)/length(mm)
    }
	class(wrongProb) <- "wrongProb"
	wrongChoice
}

