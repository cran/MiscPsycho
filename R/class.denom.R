`class.denom` <-
function(x,b, mu=0, sigma=1){
   gauss_denom <- gauss.quad.prob(49, dist='normal', mu=mu, sigma=sigma)
   mat <- rbind(like.mat(x,b,gauss_denom$nodes),gauss_denom$weights) 
   sum(apply(mat, 2, prod))
   }

