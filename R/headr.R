#' Strip Unit Rows While Importing
#'
#' This function reads in a dataset with a header and a unit row ignoring the
#' unit row to preserve data types.
#'
#' This helper function extends the functionality of \code{\link{read.csv}} by
#' importing headers separately and skipping unit rows located in the specified
#' column. If using \code{\link{read.csv}} for this, the datatype in each column
#' defaults to \code{character}. This function allows the datatype to be
#' inferred during import, which saves time for users who wish to perform
#' calculations on the data once imported without manually typecasting.
#'
#'
#' @param file Indicates file location of .csv containing dataset.
#' @param header.row Indicates the row number that the headers are located in.
#'   Defaults to 1.
#' @param data.start Indicates the row number that the data begins (i.e. the
#'   first non-unit row). Defaults to 3.
#' @return file will be imported as a data.frame with the unit rows removed.
#'
#' @author Samuel Callisto \email{calli055@@umn.edu} headr()

headr <- function(file, header.row=1, data.start=3){
  headers <- read.csv(file = file, skip=header.row-1, header = F, nrows = 1, as.is = T)
  dataset <- read.csv(file=file, skip = data.start-1, header = F, as.is=T)
  names(dataset) <- headers
  return(dataset)
}
