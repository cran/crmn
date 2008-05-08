\name{mexprs}
\title{Matrix safe accessor of expression slot}
\usage{mexprs(object)}
\description{Get the expression data from an \code{ExpressionSet} or
just return the given matrix}
\value{the expression data}
\alias{mexprs}
\alias{mexprs,ExpressionSet-method}
\alias{mexprs,matrix-method}
\author{Henning Redestig}
\arguments{\item{object}{an \code{ExpressionSet} or \code{matrix}}}
\examples{data(mix)
head(mexprs(mix))
head(mexprs(exprs(mix)))}
