\name{simRasch}
\alias{simRasch}
\title{Rasch Simulator}
\description{
Simulates item response data for the Rasch model
}
\usage{
simRasch(Nt, Nb, mu = 0, sigma = 1)
}
\arguments{
  \item{Nt}{Number of individuals}
  \item{Nb}{Number of items}
  \item{mu}{Mean of person distribution}
  \item{sigma}{Standard deviation of person distribution}
}
\value{
  \item{data}{The simulated response data}
  \item{generating.values}{The true difficulty parameters used to generate the data}
  \item{theta}{The true ability estimates used to generate the data}
}
\author{Harold C. Doran}
\examples{
simRasch(200, 10) # 200 persons 10 items}

\keyword{misc}
