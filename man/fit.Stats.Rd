\name{fit.Stats}
\alias{fit.Stats}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fit.Stats(dat, params)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{ ~~Describe \code{dat} here~~ }
  \item{params}{ ~~Describe \code{params} here~~ }
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
function(dat, params){
raschO <- function(b,theta){
  1 - (1 / (1 + exp(outer(theta,b,'-'))))
   }
theta <- apply(dat, 1, theta.max, params)
p <- raschO(as.vector(params), theta)
z2 <-((dat - p)/ sqrt(p * (1- p)))^2
v  <- sqrt(p * (1- p))^2
IF <- colSums(z2*v) / colSums(v) 
OF  <- colMeans(z2)
data.frame(IF,OF)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}

