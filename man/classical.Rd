\name{classical}
\alias{classical}
\alias{classical.default}
\alias{classical.formula}
\title{Classical Item Analysis}
\description{Provides p-values, standard errors of p-values, and point-biserial correlations for multiple choice test items. The standard errors can be design-consistent to reflect the sampling design}
\usage{
classical(...)
\method{classical}{default}(data, designSE = FALSE, group, na.rm = TRUE, use = 'everything', ...)
\method{classical}{formula}(formula, data, na.action, subset, designSE = FALSE, group, na.rm = TRUE, use = 'everything', ...)
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
          typically the environment from which \code{classical} is called.}
    \item{na.action}{a function which indicates what should happen when the data
          contain \code{NA}s.  Defaults to \code{getOption("na.action")}.}
	\item{na.rm}{logical.  Should missing values be removed?}
	\item{use}{See \code{cor} function}
	\item{subset}{an optional vector specifying a subset of observations to be
          used.}
  \item{designSE}{logical. If \code{TRUE} the function returns design-consistent standard errors to capture 
  clustering in the population}
  \item{group}{the grouping variable. Used only if designSE = TRUE}
  \item{\dots}{not implemented}
}
\author{Harold C. Doran}
\examples{
xx <- simRasch(200,10)
itemDat <- xx$dat
itemDat$group <- gl(10,20)
(aa <- classical(~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10, data = itemDat, design = TRUE, group = group))
summary(aa)
classical(itemDat[,1:10])
}
\keyword{misc}
