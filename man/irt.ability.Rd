\name{irt.ability}
\alias{irt.ability}
\title{IRT Ability Estimates}
\description{Returns the MLE, MAP, or the EAP given a set of item parameters}
\usage{
irt.ability(x, params, ind.dichot = NULL, std.err = FALSE, method = c("MLE", "MAP", "EAP"), control = list(), ...)
}
\arguments{
  \item{x}{A vector of observed item responses}
  \item{params}{Item parameters organized as a list of lists}
  \item{ind.dichot}{Denotes which items in x are multiple choice items}
  \item{std.err}{If TRUE returns the standard error}
  \item{method}{MLE, MAP, or EAP}
  \item{control}{A list of control parameters. See details below}
  \item{\dots}{Further arguments passed to optim}
}
\details{
  Function can be used for a mixture of items based on dichotomously scored data and polytomously scored data. The control parameters include:
  \item{D}{A constant usually fixed at 1.7 to bring the logistic function into coincidence with the probit}
  \item{mu}{Mean of the prior distribution}
  \item{sigma}{Standard deviation of the prior distribution}
  \item{Q}{Number of quadrature points used in the Gauss-Hermite approximation}
  \item{\code{start_val}}{User can choose a starting value for the optimization routine}
}
\value{Returns the MLE, MAP, or EAP depending on method}
\author{Harold C. Doran and Dimitris Rizopoulos}
\examples{
## all mix
params <- list("3pl" = list(a = c(1,1), b = c(0, 1), c = c(0,0)),
               "gpcm" = list(a = c(1,1), d = list(item1 = c(0,1,2,3,4), item2 = c(0,.5,1, 1.5))))
irt.ability(c(0,1,2,2), params, ind.dichot = c(1,2), method = "MLE")
irt.ability(c(0,1,2,2), params, ind.dichot = c(1,2), method = "MAP")
irt.ability(c(0,1,2,2), params, ind.dichot = c(1,2), method = "EAP")

## under the logit link
irt.ability(c(0,1,2,2), params, ind.dichot = c(1,2), method = "MLE", control = list(D = 1))
irt.ability(c(0,1,2,2), params, ind.dichot = c(1,2), method = "MAP", control = list(D = 1))
irt.ability(c(0,1,2,2), params, ind.dichot = c(1,2), method = "EAP", control = list(D = 1))


## if all are dichotomous
params <- list("3pl" = list(a = c(1,1), b = c(0, 1), c = c(0,0)), "gpcm" = NULL)
irt.ability(c(0,1), params, ind.dichot = c(1,2), method = "MLE")
irt.ability(c(0,1), params, ind.dichot = c(1,2), method = "MAP")
irt.ability(c(0,1), params, ind.dichot = c(1,2), method = "EAP")

## if all are polytomous
params <- list("3pl" = NULL, "gpcm" = list(a = c(1,1), d = list(item1 = c(0,1,2,3,4), item2 = c(0,.5,1, 1.5))))
irt.ability(c(2,3), params, method = "MLE")
irt.ability(c(2,3), params, method = "MAP")
irt.ability(c(2,3), params, method = "EAP")

## With standard error
irt.ability(c(2,3), params, method = "MLE", std.error = TRUE)
irt.ability(c(2,3), params, method = "MAP", std.error = TRUE)
irt.ability(c(2,3), params, method = "EAP", std.error = TRUE)

## From Test Scoring (Thissen) Page 115

params <- list("3pl" = list(a = c(1,2), b = c(0, 1), c = c(0,0)), 
               "gpcm" = NULL)
irt.ability(c(0,1), params, ind.dichot = c(1,2), method = "MLE", control=list(D=1))

a <- c(1.45, 1.84, 2.55, 2.27, 3.68, 4.07, 2.26, 1.87, 2.19, 1.33)
b <- c(-.6, -.82, -1.6, -.87, -1.41, -1.33, -1.16, -.11, -.64, -1.23)
params <- list("3pl" = list(a = a, b = b, c = rep(0, 10)), 
               "gpcm" = NULL)
x <- c(rep(0,9),1)
irt.ability(x, params, ind.dichot = c(1:10), method = "EAP", control=list(D=1))
}
\keyword{misc}
