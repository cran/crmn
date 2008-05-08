\name{makeX_eset}
\title{Make X...}
\usage{makeX_eset(object, factors, ...)}
\description{Make X}
\details{Construct a design matrix 

Make a design matrix from the pheno data slot of an expression
set, taking care that factors and numerical are handled
properly. No interactions are included and formula is the most
simple possible, i.e. \code{y~-1+term1+term2+...}}
\value{a design matrix}
\alias{makeX_eset}
\alias{makeX,ExpressionSet,character-method}
\author{Henning Redestig \code{henning@psc.riken.jp}}
\arguments{\item{object}{an \code{ExpressionSet}}
\item{factors}{column names from the pheno data of \code{object}}
\item{...}{not used}}
\examples{data(mix)
makeX(mix, "runorder")}
