\name{scoreCon}
\alias{scoreCon}
\alias{scoreCon.default}
\title{Function to create a score conversion table from Rasch item parameters}
\description{Creates a score conversion table using the estimated item parameters}
\usage{
scoreCon(b_vector)
\method{scoreCon}{default}(b_vector)
}

\arguments{
\item{b_vector}{vector of estimated item parameters}
}

\value{
  A list with class \code{"scoreCon"} containing the following components:
  \item{Estimate}{the maximum likelihood estimate (MLE, or ability estimate) conditional on the item parameters}
  \item{Std.Error}{the estimated standard error of the ability estimate}
  \item{Raw.Score}{the associated raw test score. Since Rasch does not use pattern scoring, all raw scores have the 
  same MLE.}
}

\author{
Harold Doran
}

\note{Scores cannot be provided for individuals with 0 correct or all incorrect because the likelihood function is 
unbounded in these cases. Other scoring methods, such as the EAP or MAP are available for these scenarios. See \code{\link{irt.ability}} for more general scoring methods}

\examples{
set.seed(1234)
tmp <- data.frame(item1 = sample(c(0,1), 20, replace=TRUE), item2 = sample(c(0,1), 20, replace=TRUE),
item3 = sample(c(0,1), 20, replace=TRUE),item4 = sample(c(0,1), 20, replace=TRUE),item5 = sample(c(0,1), 20, replace=TRUE))

## Estimate item parameters from JML (or provide them as a numeric vector)
fm1 <- jml(~ item1 + item2 + item3 + item4 + item5, data = tmp)
summary(scoreCon(coef(fm1)))
}

\keyword{misc}

