`class.acc` <-
function(x,b,prof_cut, mu=0, sigma=1, aboveQ=TRUE){
   result <- class.numer(x,b,prof_cut, mu,sigma, aboveQ)/class.denom(x,b, mu, sigma)
   return(result)
   }

