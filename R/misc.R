##' Show method for nFit
##'
##' @title Show nfit
##' @param object the \code{nFit} object
##' @return prints some basic information
##' @aliases show_nfit
##' @author Henning Redestig \code{henning@@psc.riken.jp}
show_nfit <- function(object) {
  cat(method(object), "normalization model\n")
  cat("========================\n")
  if(method(object) %in% c("crmn")) {
    cat("Effect of experiment design on standards:\n")
    cat("-----------------------------------------\n")
    print(anova(sFit(object)$fit$fit))
    cat("\nCaptured Tz:\n")
    cat("--------------\n")
    print(summary(sFit(object)$fit$pc))
    cat("\n")
  }
  if(method(object) %in% c("crmn", "nomis")) {
    cat("R2 from Tz to analytes:\n")
    cat("-----------------------\n")
    print(sum(fitted(model(object)$fit)^2) / sum(residuals(model(object)$fit)^2))
  }
}

##' Simple plot function for a CRMN normalization model.
##' Shows Tz and the optimization (if computed) of the PCA model.
##'
##' @title Plot a statistics for CRMN normalization model
##' @param x an \code{nFit} object
##' @param y not used
##' @param ... passed on to the scatter plot calls
##' @return nothing
##' @author Henning Redestig \email{henning@@psc.riken.jp}
##' @export
##' @examples
##' data(mix)
##' nfit <- normFit(mix, "crmn", factors="type", ncomp=2)
##' plot(nfit)
plot.nFit <- function(x, y=NULL,...) {
  if(!method(x) == "crmn")
    stop("Can only plot CRMN normalization models")
  pcMod <- sFit(x)$fit$pc
  if(!is.null(sFit(x)$q2)) {
    par(mfrow=c(1,3))
    mindim <- min(length(drop(sFit(x)$q2)), nPcs(sFit(x)$fit$pc))
    xx <- rbind(drop(sFit(x)$q2)[1:mindim],
                drop(cumsum(sFit(x)$r2))[1:mindim])
    barplot(xx, beside=TRUE,
               sub=expression(T[z] ~~ optimization), ylim=c(0,1.1))
    legend(x="topleft", fill=c("white", "grey"),
           legend=c(expression(R^2),
             expression(Q^2)))
                                                   
  }
  else
    par(mfrow=c(1,2))    
  plot(scores(pcMod)[,1:2], xlab=expression(T[z1]), ylab=expression(T[z2]),...)
  plot(loadings(pcMod)[,1:2], xlab=expression(P[z1]), ylab=expression(P[z2]),...)

}


##' Subset an data set to only contain the labeled internal standards.
##'
##' @title Accessor for the Internal Standards
##' @param object an \code{ExpressionSet}
##' @param ... not used
##' @return subsetted dataset
##' @author Henning Redestig \email{henning@@psc.riken.jp}
##' @aliases standards_eset standards,ExpressionSet,missing-method 
##' @examples
##' data(mix)
##' standards(mix)
standards_eset <- function(object, ...) {
  if(is.null(fData(object)$tag))
    stop("No column named 'tag' in the feature data")
  if(all(!fData(object)$tag %in% "IS"))
    stop("No rows tagged as 'IS'")
  object[fData(object)$tag %in% "IS",]
}

##' Subset an data set to only contain the labeled internal standards.
##'
##' @title Accessor for the Internal Standards
##' @param object an \code{matrix} or \code{data.frame}
##' @param standards a logical vector indicating which rows are internal standards
##' @param ... not used
##' @return subsetted dataset
##' @author Henning Redestig \email{henning@@psc.riken.jp}
##' @aliases standards_other standards,matrix,logical-method standards,data.frame,logical-method 
##' @examples
##' data(mix)
##' standards(exprs(mix), fData(mix)$tag == 'IS')
standards_other <- function(object, standards, ...) {
  if(all(!standards))
    stop("No standards")
  object[standards,,drop=FALSE]
}

##' Subset an expression set to remove the internal standards
##'
##' @title Accessor for the analytes
##' @param object an \code{ExpressionSet}
##' @param ... not used
##' @aliases analytes_eset analytes,ExpressionSet,missing-method 
##' @return \code{ExpressionSet}
##' @author Henning Redestig \email{henning@@psc.riken.jp}
##' @export
##' @examples
##' data(mix)
##' analytes(mix)
analytes_eset<- function(object, ...) {
  if(is.null(fData(object)$tag))
    stop("No column named 'tag' in the feature data")
  chosen <- !fData(object)$tag %in% "IS"
  if(all(!chosen))
    stop("All rows tagged are tagged IS")
  object[chosen,]
}

##' Subset an expression set to remove the internal standards
##'
##' @title Accessor for the analytes
##' @param object an \code{ExpressionSet}
##' @param standards a logical vector indicating which rows are internal standards
##' @param ... not used
##' @aliases analytes_other analytes,matrix,logical-method analytes,data.frame,logical-method 
##' @return \code{ExpressionSet}
##' @author Henning Redestig \email{henning@@psc.riken.jp}
##' @export
##' @examples
##' data(mix)
##' analytes(exprs(mix), fData(mix)$tag == 'IS')
analytes_other<- function(object, standards, ...)
  object[!standards,,drop=FALSE]

##' Construct a design matrix 
##'
##' Make a design matrix from the pheno data slot of an expression
##' set, taking care that factors and numerical are handled
##' properly. No interactions are included and formula is the most
##' simple possible, i.e. \code{y~-1+term1+term2+...}
##' @title Make X
##' @param object an \code{ExpressionSet}
##' @param factors column names from the pheno data of \code{object}
##' @param ... not used
##' @return a design matrix
##' @export
##' @aliases makeX_eset makeX,ExpressionSet,character-method
##' @author Henning Redestig \code{henning@@psc.riken.jp}
##' @examples
##' data(mix)
##' makeX(mix, "runorder")
makeX_eset <- function(object, factors, ...) {
  x <- pData(object)[,factors,drop=FALSE]
  ## construct a Y matrix (the experiment related information)
  nm <- sapply(x, class) %in% c("numeric", "integer")
  fac <- x[,!nm,drop=FALSE]
  mod <- NULL
  for(i in 1:ncol(fac))
    if(length(levels(fac[,i])) > 1)
      mod <- cbind(mod, model.matrix(~-1+fac[,i]))
  num <- x[,nm,drop=FALSE]
  mod <- cbind(mod, num)
  as.matrix(mod)
}  

##' Construct a design matrix 
##'
##' Convenience function that just return the given design matrix. 
##'
##' @title Make X
##' @param object, not used
##' @param factors a design matrix
##' @param ... not used
##' @return the same design matrix
##' @aliases makeX_other makeX,ANY,matrix-method
##' @export
##' @author Henning Redestig \code{henning@@psc.riken.jp}
##' @examples
##' data(mix)
##' makeX(mix, model.matrix(~pData(mix)[,"runorder"]))
makeX_other <- function(object, factors, ...)
  factors

##' Drop unused factor levels in a data frame.
##'
##' @title Drop unused levels
##' @param x the data frame
##' @author Henning Redestig \code{henning@@psc.riken.jp}
##' @export
##' @examples
##' iris[1:10,]$Species
##' dropunusedlevels(iris[1:10,])$Species
dropunusedlevels <- function (x)  {
    if (!is.data.frame(x)) 
        stop("only data frames")
    for (i in 1:length(x)) if (is.factor(x[, i])) 
        x[, i] <- factor(x[, i])
    x
}
