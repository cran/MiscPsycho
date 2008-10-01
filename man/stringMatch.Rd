\name{stringMatch}
\alias{stringMatch}
\title{Implementation of the Levenshtein Algorithm}
\description{
Function to compare the similarity of two different character strings
}
\usage{
stringMatch(string.1, string.2, normalize = c("YES", "NO"), penalty = 1, case.sensitive = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{string.1}{The first character string}
  \item{string.2}{The second character string}
  \item{normalize}{a character string indicating which method to use; if normalize = 'YES', then the edit distance is normalized to fall in the interval [0,1]}
  \item{penalty}{The edit cost}
  \item{case.sensitive}{logical; if TRUE, then a penalty occurs for differences in case of a character}
}
\value{
  a numeric value. If \code{normalize = 'YES"}, then the edit distance is normalized to fall in the interval [0,1]. Else, the Levenshtein edit distance is returned.
}
\references{http://en.wikipedia.org/wiki/Levenshtein\_distance}
\author{Harold C. Doram}

\examples{
## Return edit distance
stringMatch('William Clinton', 'Bill Clinton', normalize='NO')

## Return normalized edit distance
stringMatch('William Clinton', 'Bill Clinton', normalize='YES')

## Ignore differences in case
stringMatch('Bill Clinton', 'bill Clinton', normalize='YES', case.sensitive = FALSE)

## Do not ignore differences in case
stringMatch('Bill Clinton', 'bill Clinton', normalize='YES', case.sensitive = TRUE)
}
\keyword{misc}
