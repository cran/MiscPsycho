\name{class.denom}
\alias{class.denom}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
class.denom(x, b, mu = 0, sigma = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{b}{ ~~Describe \code{b} here~~ }
  \item{mu}{ ~~Describe \code{mu} here~~ }
  \item{sigma}{ ~~Describe \code{sigma} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(x,b, mu=0, sigma=1){
   gauss_denom <- gauss.quad.prob(49, dist='normal', mu=mu, sigma=sigma)
   mat <- rbind(like.mat(x,b,gauss_denom$nodes),gauss_denom$weights) 
   sum(apply(mat, 2, prod))
   }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}

