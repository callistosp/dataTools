#' headr()
#'
#' This function reads in a dataset with a header and a unit row
#' ignoring the unit row in preserving data types
#' @param file location of .csv containing dataset with header followed by single unit row
#' @keywords dataTools
#' @export
#' @examples
#' headr()

headr <- function(file, header.row=1, data.start=3){
  headers <- read.csv(file = file, skip=header.row-1, header = F, nrows = 1, as.is = T)
  dataset <- read.csv(file=file, skip = data.start-1, header = F, as.is=T)
  names(dataset) <- headers
  return(dataset)
}
