\name{cheat}
\alias{cheat}

\title{Excessive Similarity in Student Responses}
\description{
 Examines all pairwise comparisons within a class to assess the degree to which student responses are similar enough to suggest possible cheating
}
\usage{
cheat(dat, key, alpha = 0.01, bonf = c("yes", "no"), con = 1e-05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{A dataframe containing the student raw responses to each test items}
  \item{key}{Answer Key}
  \item{alpha}{Level of significance}
  \item{bonf}{Option to choose bonferonni adjustment}
  \item{con}{Tolerance for Newton-Raphson iterations}
}
\details{
 The dataframe must be organized with students as columns and items as the rows
}
\references{Wesolowsky, G.E (2000). Detecting excessive similarity in answers on multiple choice exams. Journal of Applied Statistics (27)7}
\author{Harold C. Doran}

\examples{

## Simulate data
NumStu   <- 30
NumItems <- 50
dat <- matrix(0, ncol=NumStu, nrow=NumItems) 
for(i in 1:NumStu){
   dat[,i] <- sample(1:4, NumItems, replace=TRUE)
}
dat <- data.frame(dat)

## Add in explicit answer copying 
dat[, (NumStu+1)] <- dat[, NumStu]
dat[, (NumStu+2)] <- c(dat[1:25, (NumStu-1)], dat[26:50, (NumStu-2)])

## Answer Key
key <- sample(1:4, NumItems, replace=TRUE)

## Test function
cheat(dat, key)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}

