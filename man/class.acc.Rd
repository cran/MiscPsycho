\name{class.acc}
\alias{class.acc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Classification Accuracy Statistic}
\description{
Computes the probability that individual i has a true score above (or below) cutscore m
}
\usage{
class.acc(x, b, prof_cut, mu = 0, sigma = 1, aboveQ = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A vector of item responses}
  \item{b}{A vector of item parameters}
  \item{prof_cut}{Cut score}
  \item{mu}{Mean of population distribution}
  \item{sigma}{Standard deviation of population distribution}
  \item{aboveQ}{Test for above or below a cut score}
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{prob}{Returns the probability that individual i has a true score above (or below) the cut score specified}
}
\author{Harold C. Doran}
\examples{
% set.seed(1)
% xx <- simRasch(200, 10) simulate data
% aa <- jml(xx$d) estimate item parameters
% scoreCon(aa$params) # this individuals theta score is -.347
% class.acc(as.numeric(xx$d[1,]), aa$p,.5) # what is the probability that the true score is above .5 on the theta scale
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}

