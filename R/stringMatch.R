stringMatch <-
function(string.1, string.2, normalize=c('YES', 'NO'), penalty = 1, case.sensitive = FALSE) {
   normalize <- toupper(normalize)
   normalize <- match.arg(normalize)
   if(case.sensitive == FALSE) {   
      string.1 <- tolower(string.1)
      string.2 <- tolower(string.2)
   }
   string.1 <- as.character(string.1)
   string.2 <- as.character(string.2)
   s1 <- strsplit(string.1,split="")[[1]]
   s2 <- strsplit(string.2,split="")[[1]]
   n <- length(s1)
   m <- length(s2)
   d <- matrix(0, nrow=n+1, ncol=m+1)
   d[,1] <- 1:(n+1)
   d[1,] <- 1:(m+1)
   d[1,1] <- 0
   for (i in 2:(n+1)) {
      for (j in 2:(m+1)) {
         if (s1[i-1] == s2[j-1]) cost <- 0 else cost <- penalty
         d[i,j] <- min(d[i-1,j] + 1, # insertion
         d[i,j-1] + 1, # deletion
         d[i-1,j-1] + cost) # substitution
         }
      }
   switch(normalize, YES = 1-d[n+1,m+1]/max(n,m), # normalize to [0,1]
   NO = d[n+1,m+1]) # Return edit distance
}

