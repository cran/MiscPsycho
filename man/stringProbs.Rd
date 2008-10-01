\name{stringProbs}
\alias{stringProbs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to determine the probability of a particular normalized edit distance}
\description{
Given a large database of character strings to be compared, this function takes a random sample, 
without replacement, from the first character string, and compares it to all other string.2 characters.
}
\usage{
stringProbs(dat, N = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{A data frame with two columns. Column 1 contains the first string and column 2 contains the second.}
  \item{N}{The number of string.1 character strings to sample.}
}
\details{
  The argument N can be equal to the total number of rows in the dataframe, but it cannot be larger.
}
\value{
  A dataframe with the cumulative probability of each normalized edit distance data
}
\references{See Levenshtein documention distributed with this package}
\author{Harold C. Doran}

\examples{
dat <- data.frame(fname1 = c('Joseph McCall', 'Paul Jones', 'Larry Everett', 'Sam Thompson', 'Sally Fields', 'Doug Carter', 'Bill Friendly', 'Tom Davison', 'Frank Mann', 'Mary Jones'),
fname2 = c('Joe McCall', 'Paul Jones', 'Barry Everett', 'Samuel Thompson', 'Sally Fields', 'Douglas Carter', 'William Friend', 'Tommy Davison', 'Franklin Mann', 'Cary Jones'))

## Randomly sample five names from the data.
stringProbs(dat, N=5)
}

\keyword{misc}
