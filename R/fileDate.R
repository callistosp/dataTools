#' Append Date to Filename
#'
#' fileDate is a small helper function which produces a
#' string containing today's date. The output is
#' intended to be appended to files which are frequently
#' iterated for organizational purposes.
#'
#' @param prefix: string to append before the date; defaults to "_"
#' @param suffix: string to be appended after the date, defaults to ".csv"
#' @return character string containing prefix, date, and suffix.
#' @examples
#' write.csv(iris, fileDate("iris_"))
#'
#' @author Samuel Callisto \email{calli055@@umn.edu}
#' @export

fileDate <- function(prefix="_", suffix=".csv"){
  return(paste0(prefix, gsub("-","", as.character(Sys.Date())), suffix))
}
