\name{analytes}
\title{Accessor for the analytes...}
\usage{analytes(object, standards=NULL, ...)}
\description{Accessor for the analytes}
\details{Subset an data set to only contain the analytes.}
\value{subsetted dataset}
\author{Henning Redestig \email{henning@psc.riken.jp}}
\alias{analytes}
\arguments{\item{object}{an \code{ExpressionSet}, \code{matrix} or \code{data.frame}}
\item{standards}{a logical vector indicating which rows are internal analytes}
\item{...}{not used}}
\examples{data(mix)
analytes(mix)
analytes(exprs(mix), fData(mix)$tag == 'IS')}
