\name{SL}
\alias{SL}
\alias{SL.default}
\title{Stocking Lord Equating Procedure}
\description{
This function estimates the A and B parameters (interept and slope) such that two tests with common items can be linked. Common items must be multiple choice items based on the 3-parameter logistic model.
}
\usage{
SL(...)
\method{SL}{default}(params1, params2, con = 1e-3, ...)
}

\arguments{
  \item{params1}{item parameters of the common items used for linking organized as a list of lists for test A. See \code{\link{irt.ability}} for more details.}
  \item{params2}{item parameters of the common items used for linking organized as a list of lists for test B. See irt.ability for more details.}
  \item{con}{Criterion for convergence}
  \item{\dots}{a list of control parameters. These are actually passed from the function \code{SLderivs},
    \describe{
        \item{D}{a constant usually fixed at 1.7 to bring the logistic function into coincidence with the probit.}
        \item{mu}{mean of the normal prior distribution.}
        \item{sigma}{standard deviation of the normal prior distribution.}
        \item{Q}{number of quadrature points used in the Gauss-Hermite approximation.}
        }
    }
 }

\value{
A list with class \code{"SL"} containing the following components:
  \item{A.Parameter}{The Stocking-Lord intercept}
  \item{B.Parameter}{The Stocking-Lord slope}
  \item{A Parameter}{Mean/Sigma intercept used as the starting values}
  \item{B Parameter}{Mean/Sigma slope used as the starting values}
}
\references{Kolen, M.J., Brennan, R.L. (2004). Test equating, scaling, and linking. Second Edition. Springer.}
\author{Harold Doran}

\examples{
## This is 3PL and are values from page 171 of Kolen and Brennan
params1 <- list("3pl" = list(a = c(.4,1.7,1.2), b = c(-1.1, .9,2.2), c = c(.1,.2,.1)), "gpcm" = NULL)
params2 <- list("3pl" = list(a = c(.5,1.6,1), b = c(-1.5, .5,2), c = c(.1,.2,.1)), "gpcm" = NULL)
SL(params1, params2, control=list(Q=30, mu=0, sigma=1))
}
\keyword{misc}

