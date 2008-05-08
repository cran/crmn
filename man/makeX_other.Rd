\name{makeX_other}
\title{Make X...}
\usage{makeX_other(object, factors, ...)}
\description{Make X}
\details{Construct a design matrix 

Convenience function that just return the given design matrix.}
\value{the same design matrix}
\alias{makeX_other}
\alias{makeX,ANY,matrix-method}
\author{Henning Redestig \code{henning@psc.riken.jp}}
\arguments{\item{object,}{not used}
\item{factors}{a design matrix}
\item{...}{not used}}
\examples{data(mix)
makeX(mix, model.matrix(~pData(mix)[,"runorder"]))}
