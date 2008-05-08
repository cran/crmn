\name{standards_other}
\title{Accessor for the Internal Standards}
\usage{standards_other(object, standards, ...)}
\description{Subset an data set to only contain the labeled internal standards.}
\value{subsetted dataset}
\alias{standards_other}
\alias{standards,matrix,logical-method}
\alias{standards,data.frame,logical-method}
\author{Henning Redestig}
\arguments{\item{object}{an \code{matrix} or \code{data.frame}}
\item{standards}{a logical vector indicating which rows are internal standards}
\item{...}{not used}}
\examples{data(mix)
standards(exprs(mix), fData(mix)$tag == 'IS')}
