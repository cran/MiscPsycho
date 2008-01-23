\name{theta.max}
\alias{theta.max}
\title{Maximum Likelihood for IRT}
\description{Returns the MLE for the Rasch Model given a set of item parameters
}
\usage{
theta.max(x, betas)
}
\arguments{
  \item{x}{Vector of item responses}
  \item{betas}{Vector of Rasch location parametersRas}
}
\author{Harold Doran}
\seealso{irt.ability for a more general function}
\examples{
x <- c(1,1,1,0,0,1)
b <- c(-1, -.5, 0, 0, .5, 1)
theta.max(x, b)
}
\keyword{misc}

