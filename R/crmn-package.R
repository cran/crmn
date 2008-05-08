#' Normalize metabolomics data using the CRMN method
#'
#'
#' \tabular{ll}{
#' Package: \tab metrik \cr
#' Type: \tab Package \cr
#' Version: \tab 0.0.3 \cr
#' Date: \tab 2009-05-14 \cr
#' Depends: \tab Biobase, pcaMethods (>= 1.20.2), pls, methods \cr
#' License: \tab GPL (>=3) \cr
#' LazyLoad: \tab yes \cr
#' }
#'
#' A package implementing the 'Cross-contribution robust 
#' normalization using multiple internal standards'. Can be used to
#' normalize metabolomics data. Do \code{openVignette("crmn")} to see
#' the manual.
#'
#' @name crmn
#' @aliases crmn
#' @docType package
#' @title CRMN
#' @author Henning Redestig \email{henning@@psc.riken.jp}
NULL

#' Mixture dilution series
#'
#' Multi-component dilution series. GC-TOF/MS measurements by Miyako Kusano.
#' Input concentrations are known and given in the original publication. 
#' 
#' @name mix
#' @aliases mix
#' @usage data(mix)
#' @docType data
#' @title Dilution mixture dataset.
#' @examples
#'  data(mix)
#'  fData(mix)
#'  exprs(mix)
#'  pData(mix)
#' @author Henning Redestig \email{henning@@psc.riken.jp}
NULL

