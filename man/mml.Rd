\name{mml}
\alias{mml}
\alias{mml.default}
\alias{mml.formula}
\title{Marginal Maximum Likelihood Estimation for Population Parameters}
\description{MML is used to estimate the population parameters (i.e., mean and variance) for an IRT model. Here, we treat item parameters as known and maximize the marginal distribution to obtain parameter estimates for the (normal) population distribution}
\usage{
mml(...)
\method{mml}{default}(data, Q = 20, params, ...)
\method{mml}{formula}(formula, data, na.action, subset, Q = 20, params, ...)
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
          typically the environment from which \code{mml} is called.}
   \item{params}{item parameters organized as a list of lists. For more info see the \bold{irt.ability} function help page.}
  \item{na.action}{a function which indicates what should happen when the data
          contain \code{NA}s.  Defaults to \code{getOption("na.action")}.}
  \item{subset}{an optional vector specifying a subset of observations to be
          used.}
  \item{Q}{Number of quadrature points used for Gauss-Hermite quadrature}
  \item{\dots}{A list of control parameters,
	\describe{
        \item{D}{a constant usually fixed at 1.7 to bring the logistic function into coincidence with the probit.}
        \item{startVal}{Optional starting values to use for parameters.}
		}
	}
}

\value{
  A list with class \code{"mml"} containing the following components:
  \item{Estimate}{the estimates of the population parameters}
  \item{Std.Error}{the standard error of the parameters}
}

\details{
   Models for \code{mml} are specified symbolically.  A typical model
     has the form \code{~item1 + item2} where the terms to the right of the 
	\code{~} are the columns of the data matrix containing the binary item responses. 
}

\author{Harold Doran}
\examples{
N <- 5
aa <- simRasch(200,N, mu=0, sigma=1)
tmp <- aa$data
b <- aa$gen
params <- list("3pl" = list(a = rep(1,N), b = b, c = rep(0,N)), "gpcm" = NULL)
colnames(tmp) <- paste('item', 1:5, sep='')


## Formula interface
fm1 <- mml(~ item1 + item2 + item3 + item4 + item5, data = tmp, params = params, control=list(D=1, startVal=c(0,1)))
summary(fm1)
coef(fm1)

## Default interface
fm1 <- mml(tmp, params = params, control=list(D=1, startVal=c(0,1)))
summary(fm1)
}

\keyword{misc}



