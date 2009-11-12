\name{cheat}
\alias{cheat}
\alias{cheat.default}
\alias{cheat.formula}
\title{Method to detect excessive similarity in student test responses}
\description{
Examines all pairwise comparisons within a group to assess the degree to which response patterns between individuals are too similar to have occured from random chance alone.
}
\usage{
cheat(...)
\method{cheat}{default}(dat, key, wrongChoice, alpha = .01, rfa = c('nr', 'uni', 'bsct'), bonf = c('yes','no'), con = 1e-12, lower = 0, upper = 50, ...)
\method{cheat}{formula}(formula, data, na.action, subset, key, wrongChoice, ...)
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
          typically the environment from which \code{cheat} is called.}
  \item{dat}{A data frame or matrix with item responses. Implemented only for the default method.}
  \item{na.action}{a function which indicates what should happen when the data
          contain \code{NA}s.  Defaults to \code{getOption("na.action")}.}
  \item{subset}{an optional vector specifying a subset of observations to be
          used.}
  \item{key}{a numeric vector containing the correct resposnes to each test item}
  \item{rfa}{the Root Finding Algorithm used. Options include \code{nr} for newton-raphson, 
  \code{uni} to use R's internal uniroot function, or \code{bsct} for the bisection method.}
  \item{alpha}{Level of significance}
  \item{bonf}{Option to choose bonferonni adjustment to the \code{alpha}}
  \item{con}{Tolerance for root finding algorithms}
  \item{lower}{the lower end points of the interval to be searched for the bisection and uniroot functions}
  \item{upper}{the upper end points of the interval to be searched for the bisection and uniroot functions}
  \item{wrongChoice}{a list containing the probability of choosing each incorrect response option. See 
  \code{\link{wrongProb}} for details}
  \item{\dots}{Not implemented}
}

\details{
 The dataframe must be organized with examinees as rows and their responses in columns
}

\references{Wesolowsky, G.E (2000). Detecting excessive similarity in answers on multiple choice exams. Journal of Applied Statistics (27)7}

\author{Harold C. Doran}

\value{
  A list with class \code{"cheat"} containing the following components:
  \item{Number of Possible Cheatering Pairs}{the number of individuals with similar response patterns)}
  \item{Possible Cheating Pairs}{the identified individuals. S28:32 denotes that individuals in rows 28 and 32 have 
  similar response patterns}
  \item{Number of Exact Matches}{Number of observed exact matches between the two individuals compared}
  \item{Observed Z Values}{the statistical result comparing number of observed exact matches to the expected}
  \item{Critical Z}{the z value used as the threshold}
  \item{Expected Number of Matches}{the expected number of matches between the two individuals compared}
}

\examples{
## Simulate data
NumStu   <- 30
NumItems <- 50
dat <- matrix(0, nrow=NumStu, ncol=NumItems) 
set.seed(1234)
for(i in 1:NumStu){
   dat[i,] <- sample(1:4, NumItems, replace=TRUE)
}
dat <- data.frame(dat)

## Add in explicit answer copying 
dat[(NumStu+1),] <- dat[NumStu,]
dat[(NumStu+2),] <- c(dat[(NumStu-1), 1:25], dat[(NumStu-2), 26:50 ])

## Answer Key
set.seed(1234)
key <- sample(1:4, NumItems, replace=TRUE)

## Formula interface
ff <- as.formula(paste('~', paste( names(dat), collapse= "+")))
## See wrongProb help page
mm <- wrongProb(ff, data = dat, key = key)
(result <- cheat(ff, data = dat, key = key, wrongChoice = mm))
summary(result)

## Default interface
(result <- cheat(dat, key = key, wrongChoice = mm))
}

\keyword{misc}

