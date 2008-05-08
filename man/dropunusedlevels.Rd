\name{dropunusedlevels}
\alias{dropunusedlevels}
\title{Drop unused levels}
\usage{dropunusedlevels(x)}
\description{Drop unused factor levels in a data frame.}
\author{Henning Redestig \code{henning@psc.riken.jp}}
\arguments{\item{x}{the data frame}}
\examples{iris[1:10,]$Species
dropunusedlevels(iris[1:10,])$Species}
