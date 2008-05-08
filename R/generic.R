#' @include norm.R
#' @include classes.R
#' @include misc.R

setGeneric("model", function(object, ...)
           standardGeneric("model"))
setGeneric("method", function(object, ...)
           standardGeneric("method"))
##' Accessor for the model
##'
##' Get the model
##'
##' @usage model(object, ...)
##' @param object an \code{nFit} object
##' @param ... not used
##' @return the model (content differs between normlization models)
##' @author Henning Redestig \email{henning@@psc.riken.jp}
##' @aliases model model,nFit-method
##' @exportMethod model
setMethod("model", signature(object="nFit"), function(object) object@model)

##' Accessor for the method
##'
##' Get the method
##'
##' @usage method(object, ...)
##' @param object an \code{nFit} object
##' @param ... not used
##' @return the method (content differs between normlization methods)
##' @author Henning Redestig \email{henning@@psc.riken.jp}
##' @aliases method method,nFit-method
##' @importFrom pcaMethods,method
##' @exportMethod method
setMethod("method", signature(object="nFit"), function(object) object@method)

setGeneric("sFit", function(object, ...)
           standardGeneric("sFit"))
##' Accessor for the standards model
##'
##' Get the sFit
##'
##' @usage sFit(object, ...)
##' @param object an \code{nFit} object
##' @param ... not used
##' @return the sFit is only defined for CRMN
##' @author Henning Redestig \email{henning@@psc.riken.jp}
##' @aliases sFit sFit,nFit-method
##' @exportMethod sFit
setMethod("sFit", signature(object="nFit"), function(object) object@sFit)


setGeneric("standards", function(object, standards=NULL, ...)
           standardGeneric("standards"))
setMethod("standards", signature(object="ExpressionSet",
                                 standards="missing"), standards_eset)
setMethod("standards", signature(object="matrix",
                                 standards="logical"), standards_other)
##' Accessor for the Internal Standards
##'
##' Subset an data set to only contain the labeled internal standards.
##'
##' @name standards
##' @usage standards(object, standards=NULL, ...)
##' @param object an \code{ExpressionSet}, \code{matrix} or \code{data.frame}
##' @param standards a logical vector indicating which rows are internal standards
##' @param ... not used
##' @return subsetted dataset
##' @author Henning Redestig \email{henning@@psc.riken.jp}
##' @aliases standards 
##' @exportMethod standards
##' @examples
##' data(mix)
##' standards(mix)
##' standards(exprs(mix), fData(mix)$tag == 'IS')
setMethod("standards", signature(object="data.frame",
                                 standards="logical"), standards_other)


setGeneric("analytes", function(object, standards=NULL, ...)
           standardGeneric("analytes"))
setMethod("analytes", signature(object="ExpressionSet",
                                 standards="missing"), analytes_eset)
setMethod("analytes", signature(object="matrix",
                                 standards="logical"), analytes_other)
##' Accessor for the analytes
##'
##' Subset an data set to only contain the analytes.
##'
##' @name analytes
##' @usage analytes(object, standards=NULL, ...)
##' @param object an \code{ExpressionSet}, \code{matrix} or \code{data.frame}
##' @param standards a logical vector indicating which rows are internal analytes
##' @param ... not used
##' @return subsetted dataset
##' @author Henning Redestig \email{henning@@psc.riken.jp}
##' @aliases analytes 
##' @exportMethod analytes
##' @examples
##' data(mix)
##' analytes(mix)
##' analytes(exprs(mix), fData(mix)$tag == 'IS')
setMethod("analytes", signature(object="data.frame",
                                 standards="logical"), analytes_other)


setGeneric("makeX", function(object, factors, ...)
           standardGeneric("makeX"))
setMethod("makeX", signature(object="ExpressionSet",
                             factors="character"), makeX_eset)
##' Make X
##'
##' Construct a design matrix 
##'
##' Make a design matrix from the pheno data slot of an expression
##' set, taking care that factors and numerical are handled
##' properly. No interactions are included and formula is the most
##' simple possible, i.e. \code{y~-1+term1+term2+...}. Can also be given
##' anything as object in which case \code{factor} must be a design matrix.
##' It that case the same design matrix is returned. 
##'
##' @param object an \code{ExpressionSet}
##' @param factors column names from the pheno data of \code{object}
##'  or a design matrix
##' @return a design matrix
##' @exportMethod makeX
##' @aliases makeX 
##' @author Henning Redestig \code{henning@@psc.riken.jp}
##' @examples
##' data(mix)
##' makeX(mix, "runorder")
##' runorder <- mix$runorder
##' makeX(mix, model.matrix(~-1+runorder))
setMethod("makeX", signature(object="ANY",
                             factors="matrix"), makeX_other)



setGeneric("mexprs", function(object)
           standardGeneric("mexprs"))
setMethod("mexprs", "ExpressionSet", function(object) exprs(object))
##' Matrix safe accessor of expression slot
##'
##' Accessor
##'
##' Get the expression data from an \code{ExpressionSet} or
##' just return the given matrix
##'
##' @param object an \code{ExpressionSet} or \code{matrix}
##' @usage mexprs(object)
##' @return the expression data
##' @exportMethod mexprs
##' @aliases mexprs mexprs,ExpressionSet-method mexprs,matrix-method
##' @author Henning Redestig \code{henning@@psc.riken.jp}
##' @examples
##' data(mix)
##' head(mexprs(mix))
##' head(mexprs(exprs(mix)))
setMethod("mexprs", "matrix", function(object) object)


##' Matrix safe setter of expression slot
##'
##' Accessor. Internal convenience function.
##' @name mexprs_bla
setGeneric("mexprs<-", function(object, value)
           standardGeneric("mexprs<-"))
setReplaceMethod("mexprs", signature("ExpressionSet","matrix"),
                 function(object, value) {
                   exprs(object) <- value
                   object
               })
##' Matrix safe setter of expression slot
##'
##' Accessor
##'
##' Set the expression data in an \code{ExpressionSet} or
##' just return the given matrix
##'
##' @name set_mexprs
##' @param object an \code{ExpressionSet} or \code{matrix}
##' @param value the value to assign
##' @usage mexprs(object) <- value
##' @return the expression data
##' @exportMethod "mexprs<-"
##' @aliases mexprs<- mexprs<-,ExpressionSet,matrix-method
##'   mexprs<-,matrix,matrix-method
##' @author Henning Redestig \code{henning@@psc.riken.jp}
##' @examples
##' data(mix)
##' test <- mix
##' mexprs(test) <- exprs(mix) * 0
##' head(mexprs(test))
##' test <- exprs(mix)
##' mexprs(test) <- test * 0
##' head(mexprs(test))
setReplaceMethod("mexprs", signature("matrix","matrix"),function(object, value) value)


##' Show 
##'
##' Show method for nFit
##'
##' Show some basic information for an nFit model
##'
##' @name show
##' @param object the \code{nFit} object
##' @return prints some basic information
##' @aliases show,nFit-method
##' @exportMethod show
##' @author Henning Redestig \code{henning@@psc.riken.jp}
##' @examples
##' data(mix)
##' normFit(mix, "avg")
setMethod("show", "nFit", show_nfit)

