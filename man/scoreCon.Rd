\name{scoreCon}
\alias{scoreCon}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Score Conversion Table}
\description{
Creates a score conversion table using the estimated item parameters
}
\usage{
scoreCon(b_vector)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{b_vector}{A vector of item parameters}
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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{misc}
