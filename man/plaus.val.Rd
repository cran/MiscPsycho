\name{plaus.val}
\alias{plaus.val}
\title{IRT Plausible Values}
\description{Draw random samples from the posterior density of an IRT Model}
\usage{
plaus.val(x, params, PV = 5, ind.dichot, M = 3, max = 5000, ...)
}
\arguments{
  \item{x}{Vector of item responses}
  \item{params}{Item parameters organized as a list of lists}
  \item{PV}{Number of plausible values desired}
  \item{ind.dichot}{Denotes which items in x are multiple choice}
  \item{M}{Constant}
  \item{max}{Maximum length of vector for storing results}
  \item{\dots}{Further arguments passed to posterior}
}
\details{Uses Rejection Sampling to generate random samples from the IRT posterior density
}
\value{Returns random samples from the posterior density of an IRT model
}
\author{Harold C. Doran}
\examples{
# all mix
params <- list("3pl" = list(a = c(1,1), b = c(0, 1), c = c(0,0)),
               "gpcm" = list(a = c(1,1), d = list(item1 = c(0,1,2,3,4), item2 = c(0,.5,1, 1.5))))

plaus.val(x = c(0,1,2,2), params = params, ind.dichot = c(1,2))
plaus.val(x = c(0,1,2,2), params = params, ind.dichot = c(1,2), control=list(mu=2, sigma=3))
}
\keyword{misc}

