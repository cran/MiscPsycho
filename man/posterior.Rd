\name{posterior}
\alias{posterior}
\title{IRT Posterior Density}
\description{Compute the density of theta}
\usage{
posterior(x, theta, params, ind.dichot, control = list())
}
\arguments{
  \item{x}{A vector of observed item responses}
  \item{theta}{Ability estimate}
  \item{params}{Item parameters organized as a list of lists}
  \item{ind.dichot}{Indicator denoting which items in x are multiple choice}
  \item{control}{A control list of parameters. See details below}
}
\details{
  \item{D}{A constant usually fixed at 1.7 to bring the logistic function into coincidence with the probit}
  \item{mu}{Mean of the prior distribution}
  \item{sigma}{Standard deviation of the prior distribution}
  \item{Q}{Number of quadrature points used in the Gauss-Hermite approximation}
}
\author{Harold C. Doran}
\examples{
params <- list("3pl" = list(a = c(1,1), b = c(0, 1), c = c(0,0)),
               "gpcm" = list(a = c(1,1), d = list(item1 = c(0,1,2,3,4), item2 = c(0,.5,1, 1.5))))
posterior(x = c(0,1,2,2), theta = 1, params = params, ind.dichot=c(1,2)) 
}
\keyword{misc}

