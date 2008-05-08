\name{plot.nFit}
\alias{plot.nFit}
\title{Plot a statistics for CRMN normalization model}
\usage{plot.nFit(x, y, ...)}
\description{Simple plot function for a CRMN normalization model.
Shows Tz and the optimization (if computed) of the PCA model.}
\value{nothing}
\author{Henning Redestig \email{henning@psc.riken.jp}}
\arguments{\item{x}{an \code{nFit} object}
\item{y}{not used}
\item{...}{passed on to the scatter plot calls}}
\examples{data(mix)
nfit <- normFit(mix, "crmn", factors="type", ncomp=2)
plot(nfit)}
