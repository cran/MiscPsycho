`class.numer` <-
function(x,b, prof_cut, mu=0, sigma=1, aboveQ){
   gauss_numer <- gauss.quad(49,kind="laguerre")
   if(aboveQ==FALSE){   
      mat <- rbind(like.mat(x,b, (prof_cut-gauss_numer$nodes)), dnorm(prof_cut-gauss_numer$nodes, mean=mu, sd=sigma))
      } else { mat <- rbind(like.mat(x,b, (gauss_numer$nodes+prof_cut)), dnorm(gauss_numer$nodes+prof_cut, mean=mu,                 sd=sigma))
   }   
   f_y <- rbind(apply(mat, 2, prod), exp(gauss_numer$nodes), gauss_numer$weights)
   sum(apply(f_y,2,prod))
   }

