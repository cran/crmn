\name{normalize}
\alias{normalize}
\title{Normalize...}
\usage{normalize(object, method, segments, ...)}
\description{Normalize}
\details{Normalize a metabolomics dataset

Wrapper function for \code{normFit} and \code{normPred}}
\author{Henning Redestig \code{henning@psc.riken.jp}}
\seealso{\code{normFit}, \code{normPred}}
\value{the normalized dataset}
\arguments{\item{object}{an \code{ExpressionSet}}
\item{method}{the desired method}
\item{segments}{normalization in a cross-validation setup, only to use for
validation/QC purposes.}
\item{...}{passed on to \code{normFit} and \code{normPred}}}
\examples{data(mix)
normalize(mix, "crmn", factor="type", ncomp=3)
#other methods
normalize(mix, "one")
normalize(mix, "avg")
normalize(mix, "nomis")
normalize(mix, "t1")
normalize(mix, "ri")
normalize(mix, "median")
normalize(mix, "totL2")
## can also do normalization with matrices
Y <- exprs(mix)
G <- model.matrix(~-1+mix$type)
isIS <- fData(mix)$tag == 'IS'
normalize(Y, "crmn", factor=G, ncomp=3, standards=isIS)}
