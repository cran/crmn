\name{weightnorm}
\alias{weightnorm}
\title{weigthnorm...}
\usage{weightnorm(object, weight="weight", lg=FALSE)}
\description{weigthnorm}
\details{Normalize by sample weight

Normalize each sample by dividing by the loaded sample weight. The
weight argument is takes from the pheno data (or given as numerical
vector with one value per sample). Missing values are not
tolerated.}
\value{the normalized expression set}
\author{Henning Redestig \code{henning@psc.riken.jp}}
\arguments{\item{object}{an \code{ExpressionSet}}
\item{weight}{a string naming the pheno data column with the weight
or a numeric vector with one weight value per sample.}
\item{lg}{is the assay data already on the log-scale or not. If lg,
the weight value is also log-transformed and subtraction is used
instead of division.}}
\examples{data(mix)
w <- runif(ncol(mix),1, 1.3)
weightnorm(mix, w)}
