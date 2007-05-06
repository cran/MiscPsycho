\name{simRasch}
\alias{simRasch}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Rasch Simulator}
\description{
Simulates item response data for the Rasch model
}
\usage{
simRasch(Nt, Nb, mu = 0, sigma = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Nt}{Number of individuals}
  \item{Nb}{Number of items}
  \item{mu}{Mean of person distribution}
  \item{sigma}{Standard deviation of person distribution}
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{data}{The simulated response data}
  \item{generating.values}{The true difficulty parameters used to generate the data}
  \item{theta}{The true ability estimates used to generate the data}
}
\author{Harold C. Doran}
\examples{
% xx <- simRasch(200, 10) # 200 persons 10 items
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}
