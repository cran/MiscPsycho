`posterior` <-
function(x, theta, params, ind.dichot, control=list()){
if (any.dicht <- !is.null(ind.dichot)) {
        x1 <- x[ind.dichot]
        x2 <- x[-ind.dichot]
    } else {
        x2 <- x
    }
    any.polyt <- length(x2)
    a <- params$'3pl'$a
    b <- params$'3pl'$b
    c <- params$'3pl'$c
    aa <- params$gpcm$a
    d <- params$gpcm$d
    con <- list(D = 1.7, mu = 0, sigma = 1, Q = 149)
    con[names(control)] <- control
    gpcm <- function (theta, d, score, a) {
        Da <- con$D * a
        exp(sum(Da * (theta - d[1:score]))) / sum(exp(cumsum(Da * (theta - d))))
    }
   log.dich.part <- if (any.dicht) {
      pr <- c + (1 - c) / (1 + exp(- con$D * a * (theta - b)))
      sum(dbinom(x1, 1, pr, log = TRUE))
      } else 0
  log.poly.part <- if (any.polyt) {
     sum(log(mapply(gpcm, d, aa, theta = theta, score = x2)))
     } else 0
   result.numer <- exp(log.dich.part + log.poly.part) * dnorm(theta, con$mu, con$sigma)
   gq <- gauss.quad.prob(con$Q, dist = "normal", mu = con$mu, sigma = con$sigma)
   nodes <- gq$nodes
   whts  <- gq$weights
   result.denom <- numeric(con$Q)
     for (i in seq_along(nodes)) {
            log.dich.part <- if (any.dicht) {
                pr <- c + (1 - c) / (1 + exp(- con$D * a * (nodes[i] - b)))
                sum(dbinom(x1, 1, pr, log = TRUE))
            } else 0
            log.poly.part <- if (any.polyt) {
                sum(log(mapply(gpcm, d, aa, theta = nodes[i], score = x2)))
            } else 0
		result.denom[i] <- exp(log.dich.part + log.poly.part) * whts[i]
   } 
   result.numer/ sum(result.denom)
}

