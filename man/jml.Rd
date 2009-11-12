\name{jml}
\alias{jml}
\alias{jml.default}
\alias{jml.formula}
\alias{fit.Stats}
\title{Joint Maximum Likelihood Estimation for Rasch Item Parameters}
\description{JML is used to estimate item parameters for the Rasch model.}
\usage{
jml(...)
\method{jml}{default}(dat, con = 1e-3, bias=FALSE, ...)
\method{jml}{formula}(formula, data, na.action, subset, con = 1e-3, bias = FALSE, ...)
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
          typically the environment from which \code{jml} is called.}
  \item{dat}{A data frame or matrix with item responses. Implemented only for the jml.default method.}
  \item{na.action}{a function which indicates what should happen when the data
          contain \code{NA}s.  Defaults to \code{getOption("na.action")}.}
  \item{subset}{an optional vector specifying a subset of observations to be
          used.}
  \item{con}{Convergence criterion}
  \item{bias}{Implements the correction for bias as described by Wright and Stone}
  \item{\dots}{Not implemented}
}

\value{
  A list with class \code{"jml"} containing the following components:
  \item{Estimate}{the value of Rasch item parameter (b-value)}
  \item{Std.Error}{the standard error of the item parameter}
  \item{Infit}{The Rasch infit statistic}
  \item{Outfit}{The Rasch outfit statistic}
  \item{Iterations}{the number of Newton-Raphson iterations used}
  \item{model.frame}{the data matrix used for estimating item parameters. In JML estimation,
  individuals with all items correct and all items incorrect cannot be used in the calibration. Hence, they are 
  dropped from the original data matrix provided by the \code{data} argument.}
}

\details{
   Models for \code{jml} are specified symbolically.  A typical model
     has the form \code{~item1 + item2} where the terms to the right of the 
	\code{~} are the columns of the data matrix containing the binary item responses. 
}

\author{Harold Doran}
\examples{
set.seed(1234)
tmp <- data.frame(item1 = sample(c(0,1), 20, replace=TRUE), item2 = sample(c(0,1), 20, replace=TRUE),
item3 = sample(c(0,1), 20, replace=TRUE),item4 = sample(c(0,1), 20, replace=TRUE),item5 = sample(c(0,1), 20, replace=TRUE))

## Formula interface
fm1 <- jml(~ item1 + item2 + item3 + item4 + item5, data = tmp)
summary(fm1)
coef(fm1)
plot(fm1)

## Default interface
fm1 <- jml(tmp)
summary(fm1)
}

\keyword{misc}



