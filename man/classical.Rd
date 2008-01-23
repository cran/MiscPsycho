\name{classical}
\alias{classical}
\title{Classical Item Analysis}
\description{
Gives a basic classical item analysis of the test items
}
\usage{
classical(data)
}
\arguments{
  \item{data}{A data frame or matrix with response variables}
}
\author{Harold C. Doran}
\examples{
xx <- simRasch(200, 10)
classical(xx$data)
}
\keyword{misc}
