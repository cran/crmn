\name{set_mexprs}
\title{Accessor}
\usage{mexprs(object) <- value}
\description{Matrix safe setter of expression slot}
\details{Set the expression data in an \code{ExpressionSet} or
just return the given matrix}
\value{the expression data}
\alias{mexprs<-}
\alias{mexprs<-,ExpressionSet,matrix-method}
\alias{mexprs<-,matrix,matrix-method}
\author{Henning Redestig \code{henning@psc.riken.jp}}
\arguments{\item{object}{an \code{ExpressionSet} or \code{matrix}}
\item{value}{the value to assign}}
\examples{data(mix)
test <- mix
mexprs(test) <- exprs(mix) * 0
head(mexprs(test))
test <- exprs(mix)
mexprs(test) <- test * 0
head(mexprs(test))}
