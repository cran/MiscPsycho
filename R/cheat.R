`cheat` <-
function(dat, key, alpha = .01, bonf = c('yes','no'), con = 1e-5){
   bonf <- tolower(bonf)
   bonf <- match.arg(bonf)

   correctStuMat <- numeric(ncol(dat))
   for(i in 1:ncol(dat)){
      correctStuMat[i] <- mean(key==dat[,i])
   }
   
   correctClsMat <- numeric(length(key))
   for(i in 1:length(key)){
      correctClsMat[i] <- mean(key[i]==dat[i,])
   }
   
   wrongChoice <- vector("list", nrow(dat))
   for(i in 1:nrow(dat)){
      mm <- as.numeric(dat[i, which(dat[i,] != key[i])])
      wrongChoice[[i]] <- table(mm)/length(mm)
   }

   pCorr <- function(R,c,q){

      numer <- function(R,a,c,q){
         result <- sum((1-(1-R)^a)^(1/a))-c*q
         result 
      }

      denom <- function(R,a,c,q){
         result <- sum(-((1 - (1 - R)^a)^(1/a) * (log((1 - (1 - R)^a)) * (1/a^2)) + 
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
         a      <- a - change
      }
      a
   }

   a <- aConst(R,c,q, con)
   result <- (1-(1-R)^a)^(1/a)
   result
}
      
   stuProbs <- matrix(0, ncol=ncol(dat), nrow=nrow(dat))
   for(i in 1:ncol(dat)){
      stuProbs[,i] <- pCorr(correctClsMat, correctStuMat[i], q = length(key))
      }

   matchProb <- function(StuMat, wrongMat, numItems){
   ind <- combn(c(1:ncol(StuMat)),2) # These are all the possible combinations
   result <- matrix(0, ncol=ncol(ind), nrow=numItems)
   for(j in 1:ncol(ind)){
      for(i in 1:numItems){
         result[i,j] <- StuMat[i,ind[1,j]] * StuMat[i,ind[2,j]] + (1-StuMat[i,ind[1,j]]) * (1-StuMat[i,ind[2,j]]) * sum(wrongChoice[[i]]^2)
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
      Match[j] <- sum(dat[, ind[1,j]] == dat[, ind[2,j]])
      }
   Match <- data.frame(Match)
   row.names(Match) <- paste('S',paste(ind[1,], ind[2,], sep=':'), sep='')
   Match
   }

   exactMatch <- matchTotal(dat)
   means <- colSums(aa)
   vars <- colSums(aa*(1-aa))
   cheaters <- (exactMatch - .5 - means)/sqrt(vars)

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
   cheaters <- subset(cheaters, Zobs >= Zcrit)
   if(nrow(cheaters) != 0){   
   cheaters
      } else {
      print('No cheaters detected in these data')
   }
}

