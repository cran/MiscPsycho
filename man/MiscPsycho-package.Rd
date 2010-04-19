\name{MiscPsycho-package}
\alias{MiscPsycho-package}
\alias{MiscPsycho}
\docType{package}
\title{Miscellaneous Psychometrics}
\description{
MiscPsycho is a package containing functions that may be useful to applied psychometricians
}
\details{
\tabular{ll}{
Package: \tab MiscPsycho\cr
Type: \tab Package\cr
Version: \tab 1.6\cr
Date: \tab 2010-04-20\cr
License: \tab GPL\cr
LazyLoad: \tab yes\cr
}
}
\author{
Maintainer: Harold C. Doran <hdoran@air.org>
}


\keyword{ package }

\examples{\dontrun{
params <- list("3pl" = list(a = c(1,1), b = c(0, 1), c = c(0,0)),
               "gpcm" = list(a = c(1,1), d = list(item1 = c(0,1,2,3,4), item2 = c(0,.5,1, 1.5))))
irt.ability(c(0,1,2,2), params, ind.dichot = c(1,2), method = "MLE")
irt.ability(c(0,1,2,2), params, ind.dichot = c(1,2), method = "MAP")
irt.ability(c(0,1,2,2), params, ind.dichot = c(1,2), method = "EAP")}
}
