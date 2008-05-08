\name{analytes_other}
\title{Accessor for the analytes...}
\usage{analytes_other(object, standards, ...)}
\description{Accessor for the analytes}
\details{Subset an expression set to remove the internal standards}
\alias{analytes_other}
\alias{analytes,matrix,logical-method}
\alias{analytes,data.frame,logical-method}
\value{\code{ExpressionSet}}
\author{Henning Redestig \email{henning@psc.riken.jp}}
\arguments{\item{object}{an \code{ExpressionSet}}
\item{standards}{a logical vector indicating which rows are internal standards}
\item{...}{not used}}
\examples{data(mix)
analytes(exprs(mix), fData(mix)$tag == 'IS')}
