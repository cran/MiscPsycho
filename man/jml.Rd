\name{jml}
\alias{jml}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Joint Maximum Likelihood}
\description{
Estimates the Rasch model using joint maximum likelihood (JML)
}
\usage{
jml(dat, con = 0.001, bias = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{A data frame or matrix containing item responses}
  \item{con}{Convergence criterion}
  \item{bias}{Correction for JML bias}
}
\details{
JML estimation only has good asymptotic properties. Marginal Maximum Likelihood (MML) is a better technology for parameter estimation.
However, this option is provided for users of Winsteps who may want to explore their data in R prior to working in Winsteps.
JML is known to return biased parameter estimates. If bias = TRUE, then a correction for bias is made using (L-1)/L * b where L
is the total number of test items and b is the original item parameter
}
\value{
jml returns item parameter estimates, their asymptotic standard errors, the sample size per item, and the Infit and Outfit statistics found in Winsteps.
}
\references{Best Test Design. Wright and Stone (1979)}
\author{Harold C. Doran}
\seealso{ltm}
\examples{
% xx <- simRasch(200, 10)
% aa <- jml(xx$data)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{models}
