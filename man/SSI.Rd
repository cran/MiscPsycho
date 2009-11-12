\name{SSI}
\alias{SSI}
\alias{SSI.default}
\alias{SSI.formula}
\title{Similar Student Index}
\description{The similar student index uses a K nearest neighbor algorithm to generate a set of conditional norms for the outcome variable. The conditional norm is constructed on the basis of the K students in the data most like student \code{i} who are used as the comparison set}
\usage{
SSI(...)
\method{SSI}{default}(mf, y, k, ...)
\method{SSI}{formula}(formula, data, id, k, na.action, subset, ...)
}

\arguments{
 \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs}
    is a numeric variable giving the data values and \code{rhs} also numeric variables giving the 
	\code{conditioning} variables used to identify the nearest neighbor.}
  \item{data}{an optional data frame, list or environment (or object
          coercible by \code{as.data.frame} to a data frame) containing
          the variables in the model.  If not found in \code{data}, the
          variables are taken from \code{environment(formula)},
          typically the environment from which \code{SSI} is called.}
  \item{na.action}{a function which indicates what should happen when the data
          contain \code{NA}s.  Defaults to \code{getOption("na.action")}.}
  \item{subset}{an optional vector specifying a subset of observations to be
          used.}
  \item{id}{the individual (student) id identifying records in the data}
  \item{k}{the number of nearest neighbors to choose. k cannot be larger than the total number of pairwise 
  comparisons in the data.}
  \item{mf}{a model frame with the variables used for conditioning. Only implemented for the default method.}
  \item{y}{the numeric outcome variable. Only implemented for the default method}
  \item{\dots}{Not implemented}
}
\details{Implementation of the K nearest neighbor method is based on the euclidean distance metric. Because the 
process identifies the k nearest neighbors for each record in the data, the process can be relatively slow, executing in O(n^2logn)}

\value{
  A list with class \code{"SSI"} containing the following components:
  \item{Zscore}{the conditional \code{z}-score for each record in the data)}
  \item{percentile}{the conditional percentile for each reocrd in the data}
  \item{ID}{the individual's record id}
  \item{Iterations}{the number of Newton-Raphson iterations used}
  \item{model.frame}{the data matrix used for estimating the conditional norms. This data frame can differ from the
  original data depending on the use of \code{na.action}. }
}

\author{Harold Doran}

\examples{
## Generate sample data
## construct a norm for the math score based on the k = 20
## other individuals in the data most like student i. 
## readScore and scienceScore are used as the conditioning variables
## to compute the euclidean norm.
set.seed(1234)
tmp <- data.frame(ID = 1:100, mathScore = rnorm(100), readScore = rnorm(100), scienceScore = rnorm(100))
(result <- SSI(mathScore ~ readScore + scienceScore, tmp, k = 20, id=ID, na.action = na.omit))
summary(result)
str(result)
head(result$model.frame)
}

\keyword{misc}

