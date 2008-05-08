\name{standards}
\title{Accessor for the Internal Standards}
\usage{standards(object, standards=NULL, ...)}
\description{Subset an data set to only contain the labeled internal standards.}
\value{subsetted dataset}
\alias{standards}
\author{Henning Redestig}
\arguments{\item{object}{an \code{ExpressionSet}, \code{matrix} or \code{data.frame}}
\item{standards}{a logical vector indicating which rows are internal standards}
\item{...}{not used}}
\examples{data(mix)
standards(mix)
standards(exprs(mix), fData(mix)$tag == 'IS')}
