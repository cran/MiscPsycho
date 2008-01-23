\name{choose.M}
\alias{choose.M}
\title{Constant Tuning}
\description{Find a Constant M that Improves the Acceptance Rate}
\usage{
choose.M(x, theta, params, ind.dichot, ...)
}
\arguments{
  \item{x}{A vector of observed responses to all items}
  \item{theta}{An arbitrarily chosen value of theta}
  \item{params}{Item parameters organized as a list of lists}
  \item{ind.dichot}{An indicator denoting the position the multiple choice items in x}
  \item{\dots}{Additional arguments passed to posterior}
}
\details{
This is a function that can be used to supplement the plaus.val function. See the MP vignette for details
}
\value{Returns a numeric value that can be the constant M in the plaus.val function}
\author{Harold C. Doran}

\examples{
params <- list("3pl" = list(a = c(1,1), b = c(0, 1), c = c(0,0)), 
				"gpcm" = list(a = c(1,1), d = list(item1 = c(0,1,2,3,4), item2 = c(0,.5,1, 1.5))))
choose.M(x = c(0,1,2,2), theta = -5, params = params, ind.dichot=c(1,2))
}
\keyword{misc}
