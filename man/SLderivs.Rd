\name{SLderivs}
\alias{SLderivs}
\title{Stocking-Lord Derivatives}
\description{
This function computes the gradient vector and the hessian needed for the SL function to operate
}
\usage{
SLderivs(params1, params2, A = 1, B = 0, control = list())
}
\arguments{
  \item{params1}{item parameters of the common items used for linking organized as a list of lists for test A}
  \item{params2}{item parameters of the common items used for linking organized as a list of lists for test B}
  \item{A}{The A parameter for the SL transformation}
  \item{B}{The B parameter for the SL transformation}
  \item{control}{See function SL for details on control elements}
}
\author{Harold C. Doran}

\examples{
## Not run
## SLderivs(params1, params2, control=list(Q=10), A = A, B = B)
}
\keyword{misc}
