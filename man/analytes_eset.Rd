\name{analytes_eset}
\title{Accessor for the analytes}
\usage{analytes_eset(object, ...)}
\description{Subset an expression set to remove the internal standards}
\alias{analytes_eset}
\alias{analytes,ExpressionSet,missing-method}
\value{\code{ExpressionSet}}
\author{Henning Redestig \email{henning@psc.riken.jp}}
\arguments{\item{object}{an \code{ExpressionSet}}
\item{...}{not used}}
\examples{data(mix)
analytes(mix)}
