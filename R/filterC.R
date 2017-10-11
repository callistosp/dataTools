#' Filter IGNORE=C
#'
#' This function filters out commented rows from a NONMEM-style
#' dataset, and removes the comment column for plotting.
#' @param ds longform dataset with IGNORE=C in first column
#' @keywords dataTools
#' @export
#' @examples
#' @author Samuel Callisto \email{calli055@umn.edu}
#' filterC()

filterC <- function(ds){
  ## filter columns with IGNORE=C
  dsFilt <- ds[which(ds[,1] != "C"),]
  ## return data.frame without comment column
  return(dsFilt[,2:ncol(dsFilt)])
}
