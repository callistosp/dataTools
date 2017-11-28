#' Extract First Row
#'
#' This function extracts the first row for each patient from a longform
#' dataset, assuming patient identifiers are located in the first column of the
#' dataset.
#'
#' This function was created for quickly plotting and running statistics on
#' patient demographics. The function assumes the first column in \code{ds}
#' contains patient identifiers ordered sequentially (i.e. no all rows for a
#' single patient are grouped together). There is an optional logical variable
#' \code{C} which is used to indicate the presence of an IGNORE column. This
#' variable defaults to \code{FALSE}, but if this variable is \code{TRUE}, then
#' this column will be removed.
#'
#' @param ds Longform dataset with patient IDs in first column.
#' @param C Indicates inclusion of IGNORE=C column; if true, removes first
#'   column. Defaults to \code{FALSE}.
#' @return The first row for each patient are returned as a data.frame with all
#'   columns intact (except first comment column if \code{C} is \code{TRUE}).
#' @examples
#' dataset <- data.frame(c(rep(1,4), rep(2,4), rep(3,4)), rep(LETTERS[1:4], 3), rep(letters[1:4],3))
#' names(dataset) <- c("SID", "cov1", "cov2")
#' output <- firstRow(dataset)
#' output
#'
#' @author Samuel Callisto \email{calli055@@umn.edu} firstRow()

firstRow <- function(ds, C=FALSE){
  if(C){
    ds <- ds[,2:ncol(ds)]
  }
  pxList <- unique(ds[,1])
  dsnew <- list()
  for(i in 1:length(pxList)){
    ## extract all rows with Ith patient
    tmp <- ds[which(ds[,1] == pxList[i]),]
    ## add 1st row to list of new rows
    dsnew[[i]] <- tmp[1,]
  }
  ## return list as data.frame
  return(do.call("rbind", dsnew))
}
