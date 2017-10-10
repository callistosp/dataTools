#' numericTime()
#'
#' This function converts clock times stored as strings
#' into numeric hours
#' @param vec vector containing time values of format HH:MM stored as strings
#' @keywords dataTools
#' @export
#' @examples
#' numericTime()

numericTime <- function(vec){
  sapply(strsplit(vec,":"),
         function(x) {
           x <- as.numeric(x)
           round(x[1]+x[2]/60,2)
         }
  )
}
