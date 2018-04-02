#' Filter IGNORE=C
#'
#' This function filters out commented rows from a NONMEM-style dataset, and
#' removes the comment column for plotting.
#'
#' This function takes a data.frame with the first column marked to indicate
#' rows to ignore. The default value for ignore is "C", similar to NONMEM,
#' however any alphanumeric character can be used. The function will return the
#' data.frame \code{ds} with all indicated rows removed as well as the entire
#' first column.
#'
#' @param ds Longform dataset with first column marked to indicate ignored rows.
#' @param ignore Character in the first column used to indicate which rows to
#'   ignore. Defaults to "C".
#' @return Data.frame \code{ds} with the rows marked ignore removed, and the
#'   entire first column removed.
#' @examples
#' dataset <- data.frame(rep(c("C","."), 10), c(1:20), LETTERS[1:20], letters[1:20])
#' names(dataset) <- c("C", "SID", "cov1", "cov2")
#' output <- filterC(dataset)
#' output
#'
#' @author Samuel Callisto \email{calli055@@umn.edu}
#' filterC()

filterC <- function(ds, ignore = "C"){
  ## filter columns with IGNORE=C
  dsFilt <- ds[which(ds[,1] != ignore),]
  ## return data.frame without comment column
  return(dsFilt[,2:ncol(dsFilt)])
}
