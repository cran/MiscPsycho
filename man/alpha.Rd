\name{alpha}
\alias{alpha}
\alias{alpha.default}
\alias{alpha.formula}
\title{Cronbach's Coefficient Alpha}
\description{
  Computes Cronbach's alpha. This reduces to KR-20 when the columns of the data matrix are dichotomous.
}
\usage{
alpha(...)
\method{alpha}{default}(dat, ...)
\method{alpha}{formula}(formula, data, na.action, subset, ...)
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
          typically the environment from which \code{alpha} is called.}
  \item{dat}{A data frame or matrix with item responses. Implemented only for the alpha.default method.}
  \item{na.action}{a function which indicates what should happen when the data
          contain \code{NA}s.  Defaults to \code{getOption("na.action")}.}
  \item{subset}{an optional vector specifying a subset of observations to be
          used.}
    \item{\dots}{Not implemented}
}

\value{
  A list with class \code{"alpha"} containing the following components:
  \item{alpha}{coefficient alpha}
  \item{numItems}{the number of test items used in the computation}
  \item{condAlpha}{The alpha that would be realized if the item were excluded}
}

\details{
The conditional alpha is accesible via the \code{summary} method for objects of class \code{alpha}
}

\author{Harold C. Doran}
\examples{
set.seed(1234)
tmp <- data.frame(item1 = sample(c(0,1), 20, replace=TRUE), item2 = sample(c(0,1), 20, replace=TRUE),
item3 = sample(c(0,1), 20, replace=TRUE),item4 = sample(c(0,1), 20, replace=TRUE),item5 = sample(c(0,1), 20, replace=TRUE))

## Formula interface
fm1 <- alpha(~ item1 + item2 + item3 + item4 + item5, data = tmp)
summary(fm1)
coef(fm1)

## Default interface
fm1 <- alpha(tmp)
summary(fm1)


}
\keyword{misc}
