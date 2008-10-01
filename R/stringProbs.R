`stringProbs` <-
function(dat, N = 10){
   ss <- sample(nrow(dat), N)
   dat_sample <- dat[c(ss),1]
   dat_all <- dat[,2]
   prmat <- matrix(0, nrow=length(dat_all), ncol=length(dat_sample))
      for(i in seq(along=dat_all)) {
         for(j in seq(along=dat_sample)) {
            prmat[i,j] <- stringMatch(dat_sample[j], dat_all[i])
         }
     }
prmat <- round(prmat,2)
result <- data.frame(table(prmat)/sum(table(prmat)))
names(result)[c(1,2)] <- c('Distance', 'Prob')
result$Distance <- as.numeric(levels(result$Distance))
result$CumProb <- 1- cumsum(result$Prob)
result
}

