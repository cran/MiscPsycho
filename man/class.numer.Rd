\name{class.numer}
\alias{class.numer}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
class.numer(x, b, prof_cut, mu = 0, sigma = 1, aboveQ)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{b}{ ~~Describe \code{b} here~~ }
  \item{prof_cut}{ ~~Describe \code{prof_cut} here~~ }
  \item{mu}{ ~~Describe \code{mu} here~~ }
  \item{sigma}{ ~~Describe \code{sigma} here~~ }
  \item{aboveQ}{ ~~Describe \code{aboveQ} here~~ }
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
function(x,b, prof_cut, mu=0, sigma=1, aboveQ){
   gauss_numer <- gauss.quad(49,kind="laguerre")
   if(aboveQ==FALSE){   
      mat <- rbind(like.mat(x,b, (prof_cut-gauss_numer$nodes)), dnorm(prof_cut-gauss_numer$nodes, mean=mu, sd=sigma))
      } else { mat <- rbind(like.mat(x,b, (gauss_numer$nodes+prof_cut)), dnorm(gauss_numer$nodes+prof_cut, mean=mu,                 sd=sigma))
   }   
   f_y <- rbind(apply(mat, 2, prod), exp(gauss_numer$nodes), gauss_numer$weights)
   sum(apply(f_y,2,prod))
   }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}

