\name{MiscPsycho-package}
\alias{MiscPsycho-package}
\alias{MiscPsycho}
\docType{package}
\title{
Miscellaneous Psychometrics
}
\description{
MiscPsycho is a package containing functions that may be useful to applied psychometricians
}
\details{
\tabular{ll}{
Package: \tab MiscPsycho\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
Date: \tab 2008-01-21\cr
License: \tab GPL \cr
}
% xx <- simRasch(200, 10) simulate data
% classical(xx$data)
% aa <- jml(xx$data) estimate item parameters
% scoreCon(aa$p) create a score conversion table
}
\author{

Maintainer: Harold C. Doran <hdoran@air.org>
}
\seealso{
See the ltm package
}
\examples{
xx <- simRasch(200, 10)
aa <- jml(xx$data)
}
\keyword{misc}
