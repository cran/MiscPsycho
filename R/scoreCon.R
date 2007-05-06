`scoreCon` <-
function(b_vector){ # b_vector is the vector of item parameters
q <- length(b_vector)
max.score <- q-1
mat <- matrix(numeric(q * q), ncol = q)
for(i in 1:q){
mat[,i] <- c(rep(1, i), rep(0, q-i))
}
scores <- apply(mat[,1:max.score], 2, theta.max, b_vector)
Raw.Score <- colSums(mat)[1:max.score]
data.frame(Raw.Score = Raw.Score, Theta = scores)
}

