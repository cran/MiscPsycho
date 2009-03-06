\name{class.acc}
\alias{class.acc}
\title{Classification Accuracy Statistic: Integration over the Posterior}
\description{
Computes the probability that individual i has a true score above (or below) cutscore m. In other words, computes the proportion of the posterior distribution that falls above (or below) a cutpoint.
}
\usage{
class.acc(x, prof_cut, params, ind.dichot, aboveC = FALSE, control=list())
}
\arguments{
  \item{x}{A vector of item responses}
  \item{prof_cut}{Cut score}
  \item{params}{Item parameters organized as a list of lists}
  \item{ind.dichot}{Indicator denoting which items in the vector x are dichotomous}
  \item{aboveC}{Test for above or below a cut score}
  \item{control}{A list of control parameters,
     \describe{
        \item{D}{A constant usually fixed at 1.7 to bring the logistic function into coincidence with the probit}
        \item{mu}{Mean of the prior distribution}
        \item{sigma}{Standard deviation of the prior distribution}
        \item{Q}{Number of quadrature points used in the Gauss-Hermite approximation}
      }
    }
  }
\value{
  \item{prob}{Returns the probability that individual i has a true score above (or below) the cut score specified}
}
\author{Harold C. Doran}
\examples{
a <- c(1.45, 1.84, 2.55, 2.27, 3.68, 4.07, 2.26, 1.87, 2.19, 1.33)
b <- c(-.6, -.82, -1.6, -.87, -1.41, -1.33, -1.16, -.11, -.64, -1.23)
params <- list("3pl" = list(a = a, b = b, c = rep(0, 10)), 
               "gpcm" = NULL)
x <- c(rep(0,9),1)
class.acc(x, prof_cut = 0, params, ind.dichot = c(1:10), aboveC=TRUE)
}

\keyword{misc}






