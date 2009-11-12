\name{wrongProb}
\alias{wrongProb}
\alias{wrongProb.default}
\alias{wrongProb.formula}
\title{
Function to find the probability of choosing the wrong option.
}
\description{
This functions supplements the \code{cheat} function. It uses classical test methods for computing the probability of choosing the incorrect test option to the ith item. 
}
\usage{
wrongProb(...)
\method{wrongProb}{default}(dat, key, ...)
\method{wrongProb}{formula}(formula, data, na.action = NULL, subset, key, ...)
}

\arguments{
  \item{formula}{an object of class \code{"formula"} (or one that can be
          coerced to that class): a symbolic description of the model
          to be fitted.  The details of model specification are given
          under \code{Details}.}
  \item{data}{an optional data frame, list or environment (or object
          coercible by \code{as.data.frame} to a data frame) containing
          the variables in the model.  If not found in \code{data}, the
          variables are taken from \code{environment(formula)},
          typically the environment from which \code{cheat} is called.}
  \item{dat}{A data frame or matrix with item responses. Implemented only for the default method.}
  \item{na.action}{a function which indicates what should happen when the data
          contain \code{NA}s.  Defaults to \code{getOption("na.action")}.}
  \item{subset}{an optional vector specifying a subset of observations to be
          used.}
  \item{key}{a numeric vector containing the correct resposnes to each test item}
  \item{\dots}{Not implemented}
}

\value{a list cnotaining the probabilitiy of choosing each incorrect option for each test item appearing on the test.}

\author{Harold Doran}

\seealso{See Also as \code{\link{cheat}}}
\examples{

NumStu   <- 30
NumItems <- 50
dat <- matrix(0, nrow=NumStu, ncol=NumItems) 
set.seed(1234)
for(i in 1:NumStu){
   dat[i,] <- sample(1:4, NumItems, replace=TRUE)
}
dat <- data.frame(dat)

## Add in explicit answer copying 
dat[(NumStu+1),] <- dat[NumStu,]
dat[(NumStu+2),] <- c(dat[(NumStu-1), 1:25], dat[(NumStu-2), 26:50 ])

## Answer Key
set.seed(1234)
key <- sample(1:4, NumItems, replace=TRUE)

## Formula interface
ff <- as.formula(paste('~', paste( names(dat), collapse= "+")))
mm <- wrongProb(ff, data = dat, key = key)

## Default interface
mm <- wrongProb(dat, key = key)
}

\keyword{misc}

