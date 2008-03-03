\name{irt.ability}
\alias{irt.ability}
\title{IRT Ability Estimates}
\description{Returns the MLE, MAP, or the EAP given a set of item parameters}
\usage{
irt.ability(x, params, ind.dichot = NULL, std.err = FALSE, method = c("MLE", "MAP", "EAP"), control = list())
}
\arguments{
  \item{x}{a numeric vector of observed item responses.}
  \item{params}{item parameters organized as a list of lists. For more info see the \bold{Details} section and 
    the \bold{Examples} below.}
  \item{ind.dichot}{a numeric vector that denotes which items in \code{x} are multiple choice items. 
    See the \bold{Examples} below.}
  \item{std.err}{logical; if TRUE it returns the standard error.}
  \item{method}{a character string indicating which method to use; available options are Maximum Likelihood Estimation, 
    Maximum A Posteriori and Expected A Posteriori.}
  \item{control}{a list of control parameters,
    \describe{
        \item{D}{a constant usually fixed at 1.7 to bring the logistic function into coincidence with the probit.}
        \item{mu}{mean of the normal prior distribution.}
        \item{sigma}{standard deviation of the normal prior distribution.}
        \item{Q}{number of quadrature points used in the Gauss-Hermite approximation.}
        \item{\code{start_val}}{a starting value for the optimization routine.}
    }
  }
}

\details{
   The function can be used for a mixture of dichotomously and polytomously scored items. For dichotomous items the 
  Birnbaum's three parameter model is assumed for which \eqn{a} denotes the discrimination parameter, \eqn{b} the 
  difficulty parameter and \eqn{c} the guessing parameter. For polytomous items the generalized partial credit model
  is assumed for which \eqn{a} denotes the discrimination parameter and \eqn{d} the threshold parameters. See the 
  \bold{Examples} below.}

\value{a numeric value. If \code{std.err = TRUE}, it has also the attribute 'std.err' that contains the estimated 
    standard error for the estimated ability.}

\references{
  Thissen, D and Wainer, H. (2001). Test Scoring. Lawrence Erlbaum.
}

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
irt.ability(c(2,3), params, method = "MLE", std.err = TRUE)
irt.ability(c(2,3), params, method = "MAP", std.err = TRUE)
irt.ability(c(2,3), params, method = "EAP", std.err = TRUE)

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
