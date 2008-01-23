\name{scoreCon}
\alias{scoreCon}
\title{Score Conversion Table}
\description{
Creates a score conversion table using the estimated item parameters
}
\usage{
scoreCon(b_vector)
}
\arguments{
  \item{b_vector}{A vector of Rasch item parameters}
}
\value{
Returns a data frame containing the raw score and the associated theta estimate
}
\author{Harold C. Doran}
\examples{
% xx <- simRasch(200, 10)
% aa <- jml(xx$data)
% scoreCon(aa$params)
}

\keyword{misc}
